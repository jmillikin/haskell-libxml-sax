{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module: Text.XML.LibXML.SAX
-- Copyright: 2010 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Bindings for the libXML2 SAX interface
--
-----------------------------------------------------------------------------

module Text.XML.LibXML.SAX
	(
	-- * Parser
	  Parser
	, newParserIO
	, newParserST
	
	-- ** Parser input
	, parseBytes
	, parseComplete
	
	-- * Callbacks
	, Callback
	, setCallback
	, clearCallback
	
	-- ** Parse events
	, parsedBeginDocument
	, parsedEndDocument
	, parsedBeginElement
	, parsedEndElement
	, parsedCharacters
	, parsedReference
	, parsedComment
	, parsedInstruction
	, parsedCDATA
	, parsedWhitespace
	, parsedInternalSubset
	, parsedExternalSubset
	
	-- ** Warning and error reporting
	, reportWarning
	, reportError
	
	) where

import qualified Control.Exception as E
import           Control.Monad (when, unless)
import qualified Control.Monad.ST as ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import           Data.Char (chr, isDigit)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.XML.Types as X
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Foreign hiding (free, void)
import           Foreign.C
import qualified Foreign.Concurrent as FC
import           Text.ParserCombinators.ReadP ((+++))
import qualified Text.ParserCombinators.ReadP as ReadP

data Context = Context

-- | A 'Parser' tracks the internal state of a LibXML parser context.
--
-- As LibXML is a very stateful library, parsers must operate within either
-- the 'IO' or 'ST.ST' monad. Use 'newParserIO' or 'newParserST' to create
-- parsers in the appropriate monad.
--
-- In general, clients should prefer 'newParserST', because ST values can be
-- safely computed with no side effects.
data Parser m = Parser
	{ parserHandle :: ForeignPtr Context
	, parserErrorRef :: IORef (Maybe E.SomeException)
	, parserToIO :: forall a. m a -> IO a
	, parserFromIO :: forall a. IO a -> m a
	}

newParserIO :: Maybe T.Text -- ^ An optional filename or URI
            -> IO (Parser IO)
newParserIO filename = E.block $ do
	ref <- newIORef Nothing
	
	raw <- maybeWith withUTF8 filename cAllocParser
	managed <- newForeignPtr_ raw
	
	FC.addForeignPtrFinalizer managed (cFreeParser raw)
	FC.addForeignPtrFinalizer managed (freeCallbacks raw)
	
	return (Parser managed ref id id)

newParserST :: Maybe T.Text -- ^ An optional filename or URI
            -> ST.ST s (Parser (ST.ST s))
newParserST filename = ST.unsafeIOToST $ do
	p <- newParserIO filename
	return $ p
		{ parserToIO = ST.unsafeSTToIO
		, parserFromIO = ST.unsafeIOToST
		}

parseImpl :: Parser m -> (Ptr Context -> IO a) -> m ()
parseImpl p io = parserFromIO p $ do
	writeIORef (parserErrorRef p) Nothing
	E.block (void (withParserIO p io))
	
	threw <- readIORef (parserErrorRef p)
	case threw of
		Nothing -> return ()
		Just exc -> E.throwIO exc

parseBytes :: Parser m -> B.ByteString -> m ()
parseBytes p bytes = parseImpl p $ \h ->
	BU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	cParseChunk h cstr (fromIntegral len) 0

-- | Finish parsing any buffered data, and check that the document was
-- closed correctly.
-- 
parseComplete :: Parser m -> m ()
parseComplete p = parseImpl p (\h -> cParseChunk h nullPtr 0 1)

-- Callbacks {{{

freeCallbacks :: Ptr Context -> IO ()
freeCallbacks ctx = do
	getcb_startDocument ctx >>= freeFunPtr
	getcb_endDocument ctx >>= freeFunPtr
	getcb_startElementNs ctx >>= freeFunPtr
	getcb_endElementNs ctx >>= freeFunPtr
	getcb_characters ctx >>= freeFunPtr
	getcb_reference ctx >>= freeFunPtr
	getcb_comment ctx >>= freeFunPtr
	getcb_processingInstruction ctx >>= freeFunPtr
	getcb_cdataBlock ctx >>= freeFunPtr
	getcb_ignorableWhitespace ctx >>= freeFunPtr
	getcb_internalSubset ctx >>= freeFunPtr
	getcb_externalSubset ctx >>= freeFunPtr
	getcb_warning ctx >>= freeFunPtr
	getcb_error ctx >>= freeFunPtr

data Callback m a = Callback (Parser m -> a -> IO ()) (Parser m -> IO ())

-- | Set a callback computation to run when a particular parse event occurs.
-- The callback should return 'True' to continue parsing, or 'False'
-- to abort.
--
-- Alternatively, callbacks may throw an 'E.Exception' to abort parsing. The
-- exception will be propagated through to the caller of 'parseBytes' or
-- 'parseComplete'.
setCallback :: Parser m -> Callback m a -> a -> m ()
setCallback p (Callback set _) io = parserFromIO p (set p io)

-- | Remove a callback from the parser. This might also change the parser's
-- behavior, such as automatically expanding entity references when no
-- 'parsedReference' callback is set.
clearCallback :: Parser m -> Callback m a -> m ()
clearCallback p (Callback _ clear) = parserFromIO p (clear p)

catchRef :: Parser m -> Ptr Context -> m Bool -> IO ()
catchRef p cb_ctx io = withParserIO p $ \ctx ->
	(cWantCallback ctx cb_ctx >>=) $ \want ->
	when (want == 1) $ do
		continue <- E.catch (E.unblock (parserToIO p io)) $ \e -> do
			writeIORef (parserErrorRef p) (Just e)
			return False
		unless continue (cStopParser ctx)

catchRefIO :: Parser m -> Ptr Context -> IO Bool -> IO ()
catchRefIO p cb_ctx io = catchRef p cb_ctx (parserFromIO p io)

callback :: (Parser m -> a -> IO (FunPtr b))
         -> (Ptr Context -> IO (FunPtr b))
         -> (Ptr Context -> FunPtr b -> IO ())
         -> Callback m a
callback wrap getPtr setPtr = Callback set clear where
	set p io = withForeignPtr (parserHandle p) $ \ctx -> do
		free ctx
		wrap p io >>= setPtr ctx
	clear p = withForeignPtr (parserHandle p) $ \ctx -> do
		free ctx
		setPtr ctx nullFunPtr
	free ctx = getPtr ctx >>= freeFunPtr

-- begin document {{{

parsedBeginDocument :: Callback m (m Bool)
parsedBeginDocument = callback wrap_startDocument
	getcb_startDocument
	setcb_startDocument

type StartDocumentSAXFunc = Ptr Context -> IO ()

wrap_startDocument :: Parser m -> m Bool -> IO (FunPtr StartDocumentSAXFunc)
wrap_startDocument p io = newcb_startDocument (\ctx -> catchRef p ctx io)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_startDocument"
	getcb_startDocument :: Ptr Context -> IO (FunPtr StartDocumentSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_startDocument"
	setcb_startDocument :: Ptr Context -> FunPtr StartDocumentSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_startDocument  :: StartDocumentSAXFunc -> IO (FunPtr StartDocumentSAXFunc)

-- }}}

-- end document {{{

parsedEndDocument :: Callback m (m Bool)
parsedEndDocument = callback wrap_endDocument
	getcb_endDocument
	setcb_endDocument

type EndDocumentSAXFunc = Ptr Context -> IO ()

wrap_endDocument :: Parser m -> m Bool -> IO (FunPtr EndDocumentSAXFunc)
wrap_endDocument p io = newcb_endDocument (\ctx -> catchRef p ctx io)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_endDocument"
	getcb_endDocument :: Ptr Context -> IO (FunPtr EndDocumentSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_endDocument"
	setcb_endDocument :: Ptr Context -> FunPtr EndDocumentSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_endDocument  :: EndDocumentSAXFunc -> IO (FunPtr EndDocumentSAXFunc)

-- }}}

-- begin element {{{

parsedBeginElement :: Callback m (X.Name -> [(X.Name, [X.Content])] -> m Bool)
parsedBeginElement = callback wrap_beginElement
	getcb_startElementNs
	setcb_startElementNs

type StartElementNsSAX2Func = (Ptr Context -> CString -> CString -> CString -> CInt -> Ptr CString -> CInt -> CInt -> Ptr CString -> IO ())

wrap_beginElement :: Parser m -> (X.Name -> [(X.Name, [X.Content])] -> m Bool) -> IO (FunPtr StartElementNsSAX2Func)
wrap_beginElement p io =
	newcb_startElementNs $ \ctx cln cpfx cns _ _ n_attrs _ raw_attrs ->
	catchRefIO p ctx $ do
		refCB <- getcb_reference ctx
		let hasRefCB = refCB /= nullFunPtr
		
		ns <- maybePeek peekUTF8 (castPtr cns)
		pfx <- maybePeek peekUTF8 (castPtr cpfx)
		ln <- peekUTF8 (castPtr cln)
		attrs <- peekAttributes hasRefCB (castPtr raw_attrs) n_attrs
		parserToIO p (io (X.Name ln ns pfx) attrs)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_startElementNs"
	getcb_startElementNs :: Ptr Context -> IO (FunPtr StartElementNsSAX2Func)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_startElementNs"
	setcb_startElementNs :: Ptr Context -> FunPtr StartElementNsSAX2Func -> IO ()

foreign import ccall "wrapper"
	newcb_startElementNs :: StartElementNsSAX2Func -> IO (FunPtr StartElementNsSAX2Func)

peekAttributes :: Bool -> Ptr CString -> CInt -> IO [(X.Name, [X.Content])]
peekAttributes hasRefCB ptr = loop 0 where
	loop _      0 = return []
	loop offset n = do
		local <- peekUTF8 =<< peekElemOff ptr (offset + 0)
		prefix <- maybePeek peekUTF8 =<< peekElemOff ptr (offset + 1)
		ns <- maybePeek peekUTF8 =<< peekElemOff ptr (offset + 2)
		
		val_begin <- peekElemOff ptr (offset + 3)
		val_end <- peekElemOff ptr (offset + 4)
		val <- peekUTF8Len (val_begin, minusPtr val_end val_begin)
		
		let content = if hasRefCB
			then parseAttributeContent val
			else [X.ContentText val]
		let attr = (X.Name local ns prefix, content)
		attrs <- loop (offset + 5) (n - 1)
		
		return (attr:attrs)

parseAttributeContent :: T.Text -> [X.Content]
parseAttributeContent = parse . T.unpack where
	parse chars = case ReadP.readP_to_S parser chars of
		(cs,_):_ -> cs
		_ -> error "parseAttributeContent: no parse"
	parser = ReadP.manyTill content ReadP.eof
	content = charRef +++ reference +++ text
	charRef = do
		void (ReadP.string "&#")
		val <- ReadP.munch1 (isDigit)
		void (ReadP.char ';')
		return (X.ContentText (T.singleton (chr (read val))))
	reference = do
		void (ReadP.char '&')
		name <- ReadP.munch1 (/= ';')
		void (ReadP.char ';')
		return (X.ContentEntity (T.pack name))
	text = do
		chars <- ReadP.munch1 (/= '&')
		return (X.ContentText (T.pack chars))

-- }}}

-- end element {{{

parsedEndElement :: Callback m (X.Name -> m Bool)
parsedEndElement = callback wrap_endElementNs
	getcb_endElementNs
	setcb_endElementNs

type EndElementNsSAX2Func = (Ptr Context -> CString -> CString -> CString -> IO ())

wrap_endElementNs :: Parser m -> (X.Name -> m Bool) -> IO (FunPtr EndElementNsSAX2Func)
wrap_endElementNs p io =
	newcb_endElementNs $ \ctx cln cpfx cns ->
	catchRefIO p ctx $ do
		ns <- maybePeek peekUTF8 (castPtr cns)
		prefix <- maybePeek peekUTF8 (castPtr cpfx)
		local <- peekUTF8 (castPtr cln)
		parserToIO p (io (X.Name local ns prefix))

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_endElementNs"
	getcb_endElementNs :: Ptr Context -> IO (FunPtr EndElementNsSAX2Func)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_endElementNs"
	setcb_endElementNs :: Ptr Context -> FunPtr EndElementNsSAX2Func -> IO ()

foreign import ccall "wrapper"
	newcb_endElementNs :: EndElementNsSAX2Func -> IO (FunPtr EndElementNsSAX2Func)

-- }}}

-- characters, cdata, and whitespace {{{

parsedCharacters :: Callback m (T.Text -> m Bool)
parsedCharacters = callback wrap_characters
	getcb_characters
	setcb_characters

-- | If 'parsedCDATA' is set, it receives any text contained in CDATA
-- blocks. By default, all text is received by 'parsedCharacters'.
parsedCDATA :: Callback m (T.Text -> m Bool)
parsedCDATA = callback wrap_characters
	getcb_cdataBlock
	setcb_cdataBlock

-- | If 'parsedWhitespace' is set, it receives any whitespace marked as
-- ignorable by the document's DTD. By default, all text is received by
-- 'parsedCharacters'.
parsedWhitespace :: Callback m (T.Text -> m Bool)
parsedWhitespace = callback wrap_characters
	getcb_ignorableWhitespace
	setcb_ignorableWhitespace

type CharactersSAXFunc = (Ptr Context -> CString -> CInt -> IO ())

wrap_characters :: Parser m -> (T.Text -> m Bool) -> IO (FunPtr CharactersSAXFunc)
wrap_characters p io =
	newcb_characters $ \ctx cstr clen ->
	catchRefIO p ctx $ do
		text <- peekUTF8Len (castPtr cstr, fromIntegral clen)
		parserToIO p (io text)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_characters"
	getcb_characters :: Ptr Context -> IO (FunPtr CharactersSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_cdataBlock"
	getcb_cdataBlock :: Ptr Context -> IO (FunPtr CharactersSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_ignorableWhitespace"
	getcb_ignorableWhitespace :: Ptr Context -> IO (FunPtr CharactersSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_characters"
	setcb_characters :: Ptr Context -> FunPtr CharactersSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_cdataBlock"
	setcb_cdataBlock :: Ptr Context -> FunPtr CharactersSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_ignorableWhitespace"
	setcb_ignorableWhitespace :: Ptr Context -> FunPtr CharactersSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_characters :: CharactersSAXFunc -> IO (FunPtr CharactersSAXFunc)

-- }}}

-- entity reference {{{

-- | If 'parsedReference' is set, entity references in element and attribute
-- content will reported separately from text, and will not be automatically
-- expanded.
--
-- Use this when processing documents in passthrough mode, to preserve
-- existing entity references.
parsedReference :: Callback m (T.Text -> m Bool)
parsedReference = callback wrap_reference
	getcb_reference
	setcb_reference

type ReferenceSAXFunc = Ptr Context -> CString -> IO ()

wrap_reference :: Parser m -> (T.Text -> m Bool) -> IO (FunPtr ReferenceSAXFunc)
wrap_reference p io =
	newcb_reference $ \ctx cstr ->
	catchRefIO p ctx $ do
		text <- peekUTF8 (castPtr cstr)
		parserToIO p (io text)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_reference"
	getcb_reference :: Ptr Context -> IO (FunPtr ReferenceSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_reference"
	setcb_reference :: Ptr Context -> FunPtr ReferenceSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_reference :: ReferenceSAXFunc -> IO (FunPtr ReferenceSAXFunc)

-- }}}

-- comment {{{

parsedComment :: Callback m (T.Text -> m Bool)
parsedComment = callback wrap_comment
	getcb_comment
	setcb_comment

type CommentSAXFunc = Ptr Context -> CString -> IO ()

wrap_comment :: Parser m -> (T.Text -> m Bool) -> IO (FunPtr CommentSAXFunc)
wrap_comment p io =
	newcb_comment $ \ctx cstr ->
	catchRefIO p ctx $ do
		text <- peekUTF8 (castPtr cstr)
		parserToIO p (io text)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_comment"
	getcb_comment :: Ptr Context -> IO (FunPtr CommentSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_comment"
	setcb_comment :: Ptr Context -> FunPtr CommentSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_comment :: CommentSAXFunc -> IO (FunPtr CommentSAXFunc)

-- }}}

-- processing instruction {{{

parsedInstruction :: Callback m (X.Instruction -> m Bool)
parsedInstruction = callback wrap_processingInstruction
	getcb_processingInstruction
	setcb_processingInstruction

type ProcessingInstructionSAXFunc = Ptr Context -> CString -> CString -> IO ()

wrap_processingInstruction :: Parser m -> (X.Instruction -> m Bool) -> IO (FunPtr ProcessingInstructionSAXFunc)
wrap_processingInstruction p io =
	newcb_processingInstruction $ \ctx ctarget cdata ->
	catchRefIO p ctx $ do
		target <- peekUTF8 (castPtr ctarget)
		value <- peekUTF8 (castPtr cdata)
		parserToIO p (io (X.Instruction target value))

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_processingInstruction"
	getcb_processingInstruction :: Ptr Context -> IO (FunPtr ProcessingInstructionSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_processingInstruction"
	setcb_processingInstruction :: Ptr Context -> FunPtr ProcessingInstructionSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_processingInstruction :: ProcessingInstructionSAXFunc -> IO (FunPtr ProcessingInstructionSAXFunc)

-- }}}

-- external subset {{{

parsedExternalSubset :: Callback m (T.Text -> Maybe X.ExternalID -> m Bool)
parsedExternalSubset = callback wrap_externalSubset
	getcb_externalSubset
	setcb_externalSubset

type ExternalSubsetSAXFunc = Ptr Context -> CString -> CString -> CString -> IO ()

wrap_externalSubset :: Parser m -> (T.Text -> Maybe X.ExternalID -> m Bool) -> IO (FunPtr ExternalSubsetSAXFunc)
wrap_externalSubset p io =
	newcb_externalSubset $ \ctx cname cpublic csystem ->
	catchRefIO p ctx $ do
		name <- peekUTF8 (castPtr cname)
		public <- maybePeek peekUTF8 (castPtr cpublic)
		system <- maybePeek peekUTF8 (castPtr csystem)
		let external = case (public, system) of
			(Nothing, Just s) -> Just (X.SystemID s)
			(Just p', Just s) -> Just (X.PublicID p' s)
			_ -> Nothing
		parserToIO p (io name external)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_externalSubset"
	getcb_externalSubset :: Ptr Context -> IO (FunPtr ExternalSubsetSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_externalSubset"
	setcb_externalSubset :: Ptr Context -> FunPtr ExternalSubsetSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_externalSubset :: ExternalSubsetSAXFunc -> IO (FunPtr ExternalSubsetSAXFunc)

-- }}}

-- internal subset {{{

parsedInternalSubset :: Callback m (T.Text -> Maybe X.ExternalID -> m Bool)
parsedInternalSubset = callback wrap_internalSubset
	getcb_internalSubset
	setcb_internalSubset

type InternalSubsetSAXFunc = Ptr Context -> CString -> CString -> CString -> IO ()

wrap_internalSubset :: Parser m -> (T.Text -> Maybe X.ExternalID -> m Bool) -> IO (FunPtr InternalSubsetSAXFunc)
wrap_internalSubset p io =
	newcb_internalSubset $ \ctx cname cpublic csystem ->
	catchRefIO p ctx $ do
		name <- peekUTF8 (castPtr cname)
		public <- maybePeek peekUTF8 (castPtr cpublic)
		system <- maybePeek peekUTF8 (castPtr csystem)
		let external = case (public, system) of
			(Nothing, Just s) -> Just (X.SystemID s)
			(Just p', Just s) -> Just (X.PublicID p' s)
			_ -> Nothing
		parserToIO p (io name external)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_internalSubset"
	getcb_internalSubset :: Ptr Context -> IO (FunPtr InternalSubsetSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_internalSubset"
	setcb_internalSubset :: Ptr Context -> FunPtr InternalSubsetSAXFunc -> IO ()

foreign import ccall "wrapper"
	newcb_internalSubset :: InternalSubsetSAXFunc -> IO (FunPtr InternalSubsetSAXFunc)

-- }}}

-- warning and error {{{

reportWarning :: Callback m (T.Text -> m Bool)
reportWarning = callback wrap_FixedError
	getcb_warning
	setcb_warning

reportError :: Callback m (T.Text -> m Bool)
reportError = callback wrap_FixedError
	getcb_error
	setcb_error

type FixedErrorFunc = Ptr Context -> CString -> IO ()

wrap_FixedError :: Parser m -> (T.Text -> m Bool) -> IO (FunPtr FixedErrorFunc)
wrap_FixedError p io =
	newcb_FixedError $ \ctx cmsg ->
	catchRefIO p ctx $ do
		msg <- peekUTF8 cmsg
		parserToIO p (io msg)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_warning"
	getcb_warning :: Ptr Context -> IO (FunPtr FixedErrorFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_error"
	getcb_error :: Ptr Context -> IO (FunPtr FixedErrorFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_warning"
	setcb_warning :: Ptr Context -> FunPtr FixedErrorFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_error"
	setcb_error :: Ptr Context -> FunPtr FixedErrorFunc -> IO ()

foreign import ccall "wrapper"
	newcb_FixedError :: FixedErrorFunc -> IO (FunPtr FixedErrorFunc)

-- }}}

-- }}}

withParserIO :: Parser m -> (Ptr Context -> IO a) -> IO a
withParserIO p io = withForeignPtr (parserHandle p) io

peekUTF8 :: CString -> IO T.Text
peekUTF8 = fmap (TE.decodeUtf8) . B.packCString

peekUTF8Len :: CStringLen -> IO T.Text
peekUTF8Len = fmap (TE.decodeUtf8) . B.packCStringLen

withUTF8 :: T.Text -> (CString -> IO a) -> IO a
withUTF8 = BU.unsafeUseAsCString . TE.encodeUtf8

freeFunPtr :: FunPtr a -> IO ()
freeFunPtr ptr = if ptr == nullFunPtr
	then return ()
	else freeHaskellFunPtr ptr

void :: Functor f => f a -> f ()
void = fmap (const ())

foreign import ccall unsafe "hslibxml-shim.h hslibxml_alloc_parser"
	cAllocParser :: CString -> IO (Ptr Context)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_free_parser"
	cFreeParser :: Ptr Context -> IO ()

foreign import ccall safe "libxml/parser.h xmlParseChunk"
	cParseChunk :: Ptr Context -> CString -> CInt -> CInt -> IO CInt

foreign import ccall safe "libxml/parser.h xmlStopParser"
	cStopParser :: Ptr Context -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_want_callback"
	cWantCallback :: Ptr Context -> Ptr a -> IO CInt

-- Unbound callback FFI definitions {{{

{-

data Entity = Entity

data ParserInput = ParserInput

data Enumeration = Enumeration

data ElementContent = ElementContent

data XmlError = XmlError

type IsStandaloneSAXFunc = Ptr Context -> IO CInt

type HasInternalSubsetSAXFunc = Ptr Context -> IO CInt

type HasExternalSubsetSAXFunc = Ptr Context -> IO CInt

type ExternalEntityLoader = CString -> CString -> Ptr Context -> IO (Ptr ParserInput)

type GetEntitySAXFunc = Ptr Context -> CString -> IO (Ptr Entity)

type EntityDeclSAXFunc = Ptr Context -> CString -> CInt -> CString -> CString -> CString -> IO ()

type NotationDeclSAXFunc = Ptr Context -> CString -> CString -> CString -> IO ()

type AttributeDeclSAXFunc = Ptr Context -> CString -> CString -> CInt -> CInt -> CString -> Ptr Enumeration -> IO ()

type ElementDeclSAXFunc = Ptr Context -> CString -> CInt -> Ptr ElementContent -> IO ()

type UnparsedEntityDeclSAXFunc = Ptr Context -> CString -> CString -> CString -> CString -> IO ()

type GetParameterEntitySAXFunc = Ptr Context -> CString -> IO (Ptr Entity)

type XmlStructuredErrorFunc = Ptr Context -> Ptr XmlError -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_isStandalone"
	getcb_isStandalone :: Ptr Context -> IO (FunPtr IsStandaloneSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_hasInternalSubset"
	getcb_hasInternalSubset :: Ptr Context -> IO (FunPtr HasInternalSubsetSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_hasExternalSubset"
	getcb_hasExternalSubset :: Ptr Context -> IO (FunPtr HasExternalSubsetSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_resolveEntity"
	getcb_resolveEntity :: Ptr Context -> IO (FunPtr ResolveEntitySAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_getEntity"
	getcb_getEntity :: Ptr Context -> IO (FunPtr GetEntitySAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_entityDecl"
	getcb_entityDecl :: Ptr Context -> IO (FunPtr EntityDeclSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_notationDecl"
	getcb_notationDecl :: Ptr Context -> IO (FunPtr NotationDeclSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_attributeDecl"
	getcb_attributeDecl :: Ptr Context -> IO (FunPtr AttributeDeclSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_elementDecl"
	getcb_elementDecl :: Ptr Context -> IO (FunPtr ElementDeclSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_unparsedEntityDecl"
	getcb_unparsedEntityDecl :: Ptr Context -> IO (FunPtr UnparsedEntityDeclSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_getParameterEntity"
	getcb_getParameterEntity :: Ptr Context -> IO (FunPtr GetParameterEntitySAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_serror"
	getcb_serror :: Ptr Context -> IO (FunPtr XmlStructuredErrorFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_isStandalone"
	setcb_isStandalone :: Ptr Context -> FunPtr IsStandaloneSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_hasInternalSubset"
	setcb_hasInternalSubset :: Ptr Context -> FunPtr HasInternalSubsetSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_hasExternalSubset"
	setcb_hasExternalSubset :: Ptr Context -> FunPtr HasExternalSubsetSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_resolveEntity"
	setcb_resolveEntity :: Ptr Context -> FunPtr ResolveEntitySAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_getEntity"
	setcb_getEntity :: Ptr Context -> FunPtr GetEntitySAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_entityDecl"
	setcb_entityDecl :: Ptr Context -> FunPtr EntityDeclSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_notationDecl"
	setcb_notationDecl :: Ptr Context -> FunPtr NotationDeclSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_attributeDecl"
	setcb_attributeDecl :: Ptr Context -> FunPtr AttributeDeclSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_elementDecl"
	setcb_elementDecl :: Ptr Context -> FunPtr ElementDeclSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_unparsedEntityDecl"
	setcb_unparsedEntityDecl :: Ptr Context -> FunPtr UnparsedEntityDeclSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_getParameterEntity"
	setcb_getParameterEntity :: Ptr Context -> FunPtr GetParameterEntitySAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_serror"
	setcb_serror :: Ptr Context -> FunPtr XmlStructuredErrorFunc -> IO ()

-}

-- }}}
