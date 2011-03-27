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
	
	-- ** Callbacks
	, Callback
	, setCallback
	, clearCallback
	
	, parsedBeginDocument
	, parsedEndDocument
	, parsedBeginElement
	, parsedEndElement
	, parsedCharacters
	, parsedComment
	, parsedInstruction
	, parsedDoctype
	
	-- *** Buffer-based callbacks
	, parsedCharactersBuffer
	, parsedCommentBuffer
	
	-- ** Parser input
	, parseText
	, parseLazyText
	, parseBytes
	, parseLazyBytes
	, parseBuffer
	, parseComplete
	) where
import qualified Control.Exception as E
import           Control.Monad (when, unless)
import qualified Control.Monad.ST as ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.XML.Types as X
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Foreign hiding (free)
import           Foreign.C
import qualified Foreign.Concurrent as FC

data Context = Context

data Parser m = Parser
	{ parserHandle :: ForeignPtr Context
	, parserErrorRef :: IORef (Maybe E.SomeException)
	, parserOnError :: T.Text -> m ()
	, parserToIO :: forall a. m a -> IO a
	, parserFromIO :: forall a. IO a -> m a
	}

newParserIO :: (T.Text -> IO ()) -- ^ An error handler, called if parsing fails
            -> Maybe T.Text -- ^ An optional filename or URI
            -> IO (Parser IO)
newParserIO onError filename = E.block $ do
	ref <- newIORef Nothing
	
	raw <- maybeWith withUTF8 filename cAllocParser
	managed <- newForeignPtr_ raw
	
	FC.addForeignPtrFinalizer managed (cFreeParser raw)
	FC.addForeignPtrFinalizer managed (freeCallbacks raw)
	
	return (Parser managed ref onError id id)

newParserST :: (T.Text -> ST.ST s ()) -- ^ An error handler, called if parsing fails
            -> Maybe T.Text -- ^ An optional filename or URI
            -> ST.ST s (Parser (ST.ST s))
newParserST onError filename = ST.unsafeIOToST $ do
	p <- newParserIO (\_ -> return ()) filename
	return $ p
		{ parserToIO = ST.unsafeSTToIO
		, parserFromIO = ST.unsafeIOToST
		, parserOnError = onError
		}

freeCallbacks :: Ptr Context -> IO ()
freeCallbacks ctx = do
	cGetCB_StartDocument ctx >>= freeFunPtr
	cGetCB_EndElement ctx >>= freeFunPtr
	cGetCB_StartElement ctx >>= freeFunPtr
	cGetCB_EndElement ctx >>= freeFunPtr
	cGetCB_Characters ctx >>= freeFunPtr
	cGetCB_Instruction ctx >>= freeFunPtr
	cGetCB_Comment ctx >>= freeFunPtr

-- | A callback should return 'True' to continue parsing, or 'False'
-- to cancel.
--
data Callback m a = Callback (Parser m -> a -> IO ()) (Parser m -> IO ())

setCallback :: Parser m -> Callback m a -> a -> m ()
setCallback p (Callback set _) io = parserFromIO p (set p io)

clearCallback :: Parser m -> Callback m a -> m ()
clearCallback p (Callback _ clear) = parserFromIO p (clear p)

catchRef :: Parser m -> m Bool -> IO ()
catchRef p io = do
	continue <- E.catch (E.unblock (parserToIO p io)) $ \e -> do
		writeIORef (parserErrorRef p) (Just e)
		return False
	unless continue (withParserIO p cStopParser)

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

-- Callback wrappers
type CUString = Ptr CUChar

type Callback0 = Ptr Context -> IO ()

type StartElementNsSAX2Func = (Ptr Context -> CUString -> CUString
                               -> CUString -> CInt -> Ptr CUString -> CInt
                               -> CInt -> Ptr CUString -> IO ())
type EndElementNsSAX2Func = (Ptr Context -> CUString -> CUString -> CUString -> IO ())

type CharactersSAXFunc = (Ptr Context -> CUString -> CInt -> IO ())

type CommentSAXFunc = Ptr Context -> CUString -> IO ()

type ProcessingInstructionSAXFunc = Ptr Context -> CUString -> CUString -> IO ()

type ExternalSubsetSAXFunc = Ptr Context -> CUString -> CUString -> CUString -> IO ()

foreign import ccall "wrapper"
	allocCallback0 :: Callback0 -> IO (FunPtr Callback0)

foreign import ccall "wrapper"
	allocCallbackBeginElement :: StartElementNsSAX2Func -> IO (FunPtr StartElementNsSAX2Func)

foreign import ccall "wrapper"
	allocCallbackEndElement :: EndElementNsSAX2Func -> IO (FunPtr EndElementNsSAX2Func)

foreign import ccall "wrapper"
	allocCallbackCharacters :: CharactersSAXFunc -> IO (FunPtr CharactersSAXFunc)

foreign import ccall "wrapper"
	allocCallbackComment :: CommentSAXFunc -> IO (FunPtr CommentSAXFunc)

foreign import ccall "wrapper"
	allocCallbackInstruction :: ProcessingInstructionSAXFunc -> IO (FunPtr ProcessingInstructionSAXFunc)

foreign import ccall "wrapper"
	allocCallbackExternalSubset :: ExternalSubsetSAXFunc -> IO (FunPtr ExternalSubsetSAXFunc)

-- localname, prefix, namespace, value_begin, value_end
data CAttribute = CAttribute CString CString CString CString CString

splitCAttributes :: CInt -> Ptr CString -> IO [CAttribute]
splitCAttributes = loop 0 where
	loop _      0 _     = return []
	loop offset n attrs = do
		c_ln <- peekElemOff attrs (offset + 0)
		c_prefix <- peekElemOff attrs (offset + 1)
		c_ns <- peekElemOff attrs (offset + 2)
		c_vbegin <- peekElemOff attrs (offset + 3)
		c_vend <- peekElemOff attrs (offset + 4)
		as <- loop (offset + 5) (n - 1) attrs
		return (CAttribute c_ln c_prefix c_ns c_vbegin c_vend : as)

convertCAttribute :: CAttribute -> IO (X.Name, [X.Content])
convertCAttribute (CAttribute c_ln c_pfx c_ns c_vbegin c_vend) = do
	ln <- peekUTF8 c_ln
	pfx <- maybePeek peekUTF8 c_pfx
	ns <- maybePeek peekUTF8 c_ns
	val <- peekUTF8Len (c_vbegin, minusPtr c_vend c_vbegin)
	return (X.Name ln ns pfx, [X.ContentText val])

-- Exposed callbacks

wrapCallback0 :: Parser m -> m Bool -> IO (FunPtr Callback0)
wrapCallback0 p io = allocCallback0 (\_ -> catchRef p io)

parsedBeginDocument :: Callback m (m Bool)
parsedBeginDocument = callback wrapCallback0
	cGetCB_StartDocument
	cSetCB_StartDocument

parsedEndDocument :: Callback m (m Bool)
parsedEndDocument = callback wrapCallback0
	cGetCB_EndDocument
	cSetCB_EndDocument

wrapBeginElement :: Parser m -> (X.Name -> Map X.Name [X.Content] -> m Bool)
                 -> IO (FunPtr StartElementNsSAX2Func)
wrapBeginElement p io =
	allocCallbackBeginElement $ \_ cln cpfx cns _ _ n_attrs _ raw_attrs ->
	catchRef p $ parserFromIO p $ do
		ns <- maybePeek peekUTF8 (castPtr cns)
		pfx <- maybePeek peekUTF8 (castPtr cpfx)
		ln <- peekUTF8 (castPtr cln)
		c_attrs <- splitCAttributes n_attrs (castPtr raw_attrs)
		attrs <- mapM convertCAttribute c_attrs
		parserToIO p (io (X.Name ln ns pfx) (Map.fromList attrs))

parsedBeginElement :: Callback m (X.Name -> Map X.Name [X.Content] -> m Bool)
parsedBeginElement = callback wrapBeginElement
	cGetCB_StartElement
	cSetCB_StartElement

wrapEndElement :: Parser m -> (X.Name -> m Bool)
               -> IO (FunPtr EndElementNsSAX2Func)
wrapEndElement p io =
	allocCallbackEndElement $ \_ cln cpfx cns ->
	catchRef p $ parserFromIO p $ do
		ns <- maybePeek peekUTF8 (castPtr cns)
		pfx <- maybePeek peekUTF8 (castPtr cpfx)
		ln <- peekUTF8 (castPtr cln)
		parserToIO p (io (X.Name ln ns pfx))

parsedEndElement :: Callback m (X.Name -> m Bool)
parsedEndElement = callback wrapEndElement
	cGetCB_EndElement
	cSetCB_EndElement

wrapCharacters :: Parser m -> (T.Text -> m Bool)
               -> IO (FunPtr CharactersSAXFunc)
wrapCharacters p io =
	allocCallbackCharacters $ \_ cstr clen ->
	catchRef p $ parserFromIO p $ do
		text <- peekUTF8Len (castPtr cstr, fromIntegral clen)
		parserToIO p (io text)

parsedCharacters :: Callback m (T.Text -> m Bool)
parsedCharacters = callback wrapCharacters
	cGetCB_Characters
	cSetCB_Characters

wrapComment :: Parser m -> (T.Text -> m Bool)
            -> IO (FunPtr CommentSAXFunc)
wrapComment p io =
	allocCallbackComment $ \_ cstr ->
	catchRef p $ parserFromIO p $ do
		text <- peekUTF8 (castPtr cstr)
		parserToIO p (io text)

parsedComment :: Callback m (T.Text -> m Bool)
parsedComment = callback wrapComment
	cGetCB_Comment
	cSetCB_Comment

wrapInstruction :: Parser m -> (X.Instruction -> m Bool)
                -> IO (FunPtr ProcessingInstructionSAXFunc)
wrapInstruction p io =
	allocCallbackInstruction $ \_ ctarget cdata ->
	catchRef p $ parserFromIO p $ do
		target <- peekUTF8 (castPtr ctarget)
		value <- peekUTF8 (castPtr cdata)
		parserToIO p (io (X.Instruction target value))

parsedInstruction :: Callback m (X.Instruction -> m Bool)
parsedInstruction = callback wrapInstruction
	cGetCB_Instruction
	cSetCB_Instruction

wrapExternalSubset :: Parser m -> (X.Doctype -> m Bool) -> IO (FunPtr ExternalSubsetSAXFunc)
wrapExternalSubset p io =
	allocCallbackExternalSubset $ \_ cname cpublic csystem ->
	catchRef p $ parserFromIO p $ do
		name <- peekUTF8 (castPtr cname)
		public <- maybePeek peekUTF8 (castPtr cpublic)
		system <- maybePeek peekUTF8 (castPtr csystem)
		let external = case (public, system) of
			(Nothing, Just s) -> Just (X.SystemID s)
			(Just p', Just s) -> Just (X.PublicID p' s)
			_ -> Nothing
		parserToIO p (io (X.Doctype name external []))

parsedDoctype :: Callback m (X.Doctype -> m Bool)
parsedDoctype = callback wrapExternalSubset
	cGetCB_ExternalSubset
	cSetCB_ExternalSubset

wrapCharactersBuffer :: Parser m -> ((Ptr Word8, Integer) -> m Bool)
                     -> IO (FunPtr CharactersSAXFunc)
wrapCharactersBuffer p io =
	allocCallbackCharacters $ \_ cstr clen ->
	catchRef p $ do
		io (castPtr cstr, fromIntegral clen)

parsedCharactersBuffer :: Callback m ((Ptr Word8, Integer) -> m Bool)
parsedCharactersBuffer = callback wrapCharactersBuffer
	cGetCB_Characters
	cSetCB_Characters

wrapCommentBuffer :: Parser m -> ((Ptr Word8, Integer) -> m Bool)
            -> IO (FunPtr CommentSAXFunc)
wrapCommentBuffer p io =
	allocCallbackComment $ \_ cstr ->
	catchRef p $ parserFromIO p $ do
		clen <- cXmlStrlen cstr
		parserToIO p (io (castPtr cstr, fromIntegral clen))

parsedCommentBuffer :: Callback m ((Ptr Word8, Integer) -> m Bool)
parsedCommentBuffer = callback wrapCommentBuffer
	cGetCB_Comment
	cSetCB_Comment

withParserIO :: Parser m -> (Ptr Context -> IO a) -> IO a
withParserIO p io = withForeignPtr (parserHandle p) io

parseImpl :: Parser m -> (Ptr Context -> IO CInt) -> m ()
parseImpl p io = parserFromIO p $ do
	writeIORef (parserErrorRef p) Nothing
	rc <- E.block (withParserIO p io)
	
	threw <- readIORef (parserErrorRef p)
	case threw of
		Nothing -> return ()
		Just exc -> E.throwIO exc
	
	when (rc /= 0) $ do
		err <- getParseError p
		parserToIO p (parserOnError p err)

parseText :: Parser m -> T.Text -> m ()
parseText p = parseBytes p . TE.encodeUtf8

parseLazyText :: Parser m -> TL.Text -> m ()
parseLazyText p = parseText p . T.concat . TL.toChunks

parseBytes :: Parser m -> B.ByteString -> m ()
parseBytes p bytes = parseImpl p $ \h ->
	BU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
	cParseChunk h cstr (fromIntegral len) 0

parseLazyBytes :: Parser m -> BL.ByteString -> m ()
parseLazyBytes p = parseBytes p . B.concat . BL.toChunks

parseBuffer :: Parser m -> (Ptr Word8, Integer) -> m ()
parseBuffer p (ptr, len) = parseImpl p $ \h ->
	cParseChunk h (castPtr ptr) (fromIntegral len) 0

-- | Finish parsing any buffered data, and check that the document was
-- closed correctly.
-- 
parseComplete :: Parser m -> m ()
parseComplete p = parseImpl p (\h -> cParseChunk h nullPtr 0 1)

getParseError :: Parser m -> IO T.Text
getParseError p = withParserIO p (\h -> cGetLastError h >>= peekUTF8)

peekUTF8 :: CString -> IO T.Text
peekUTF8 = fmap (TE.decodeUtf8) . B.packCString

peekUTF8Len :: CStringLen -> IO T.Text
peekUTF8Len = fmap (TE.decodeUtf8) . B.packCStringLen

withUTF8 :: T.Text -> (CString -> IO a) -> IO a
withUTF8 = B.useAsCString . TE.encodeUtf8

freeFunPtr :: FunPtr a -> IO ()
freeFunPtr ptr = if ptr == nullFunPtr
	then return ()
	else freeHaskellFunPtr ptr

foreign import ccall unsafe "hslibxml-shim.h hslibxml_alloc_parser"
	cAllocParser :: CString -> IO (Ptr Context)

foreign import ccall unsafe "libxml/parser.h xmlFreeParserCtxt"
	cFreeParser :: Ptr Context -> IO ()

foreign import ccall safe "libxml/parser.h xmlParseChunk"
	cParseChunk :: Ptr Context -> CString -> CInt -> CInt -> IO CInt

foreign import ccall safe "libxml/parser.h xmlStopParser"
	cStopParser :: Ptr Context -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_get_last_error"
	cGetLastError :: Ptr Context -> IO CString

foreign import ccall unsafe "libxml/parser.h xmlStrlen"
	cXmlStrlen :: Ptr CUChar -> IO CInt

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_start_document"
	cGetCB_StartDocument :: Ptr Context -> IO (FunPtr Callback0)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_end_document"
	cGetCB_EndDocument :: Ptr Context -> IO (FunPtr Callback0)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_start_element"
	cGetCB_StartElement :: Ptr Context -> IO (FunPtr StartElementNsSAX2Func)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_end_element"
	cGetCB_EndElement :: Ptr Context -> IO (FunPtr EndElementNsSAX2Func)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_characters"
	cGetCB_Characters :: Ptr Context -> IO (FunPtr CharactersSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_instruction"
	cGetCB_Instruction :: Ptr Context -> IO (FunPtr ProcessingInstructionSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_comment"
	cGetCB_Comment :: Ptr Context -> IO (FunPtr CommentSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_getcb_external_subset"
	cGetCB_ExternalSubset :: Ptr Context -> IO (FunPtr ExternalSubsetSAXFunc)

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_start_document"
	cSetCB_StartDocument :: Ptr Context -> FunPtr Callback0 -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_end_document"
	cSetCB_EndDocument :: Ptr Context -> FunPtr Callback0 -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_start_element"
	cSetCB_StartElement :: Ptr Context -> FunPtr StartElementNsSAX2Func -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_end_element"
	cSetCB_EndElement :: Ptr Context -> FunPtr EndElementNsSAX2Func -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_characters"
	cSetCB_Characters :: Ptr Context -> FunPtr CharactersSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_instruction"
	cSetCB_Instruction :: Ptr Context -> FunPtr ProcessingInstructionSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_comment"
	cSetCB_Comment :: Ptr Context -> FunPtr CommentSAXFunc -> IO ()

foreign import ccall unsafe "hslibxml-shim.h hslibxml_setcb_external_subset"
	cSetCB_ExternalSubset :: Ptr Context -> FunPtr ExternalSubsetSAXFunc -> IO ()
