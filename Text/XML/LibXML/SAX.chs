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
{-# LANGUAGE ForeignFunctionInterface #-}

#include <libxml/parser.h>
#include <string.h>

module Text.XML.LibXML.SAX
	( Parser
	, ParserCallbacks (..)
	, newParser
	, parse
	, parseComplete
	) where
import qualified Control.Exception as E
import qualified Control.Monad.ST as ST
import qualified Data.STRef as ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.XML.Types as X
import Foreign
import Foreign.C
import qualified Foreign.Concurrent as FC

data ParserCallbacks s = ParserCallbacks
	{ parsedBeginElement :: X.Name -> [X.Attribute] -> ST.ST s ()
	, parsedEndElement :: X.Name -> ST.ST s ()
	, parsedCharacters :: T.Text -> ST.ST s ()
	, parsedComment :: T.Text -> ST.ST s ()
	, parsedProcessingInstruction :: X.Instruction -> ST.ST s ()
	}

{# pointer *xmlParserCtxt as Context newtype #}

data Parser s = Parser
	{ parserContext :: ForeignPtr Context
	, parserCallbackPtr :: ForeignPtr ()
	, parserOnError :: T.Text -> ST.ST s ()
	, parserErrorRef :: ST.STRef s (Maybe E.SomeException)
	}

newParser :: ParserCallbacks s
          -> (T.Text -> ST.ST s ()) -- ^ An error handler, called if parsing fails
          -> Maybe T.Text -- ^ An optional filename or URI
          -> ST.ST s (Parser s)
newParser callbacks onError filename = do
	ref <- ST.newSTRef Nothing
	ST.unsafeIOToST $ do
		cCallbacks <- mallocForeignPtrBytes {# sizeof xmlSAXHandler #}
		withForeignPtr cCallbacks $ \raw -> do
			{# call memset #} raw 0 {# sizeof xmlSAXHandler #}
			{#set xmlSAXHandler->initialized #} raw xmlSax2Magic
			
			wrappedBegin (onBeginElement ref (parsedBeginElement callbacks))
				>>= {#set xmlSAXHandler->startElementNs #} raw
			wrappedEnd (onEndElement ref (parsedEndElement callbacks))
				>>= {#set xmlSAXHandler->endElementNs #} raw
			wrappedText (onCharacters ref (parsedCharacters callbacks))
				>>= {#set xmlSAXHandler->characters #} raw
			wrappedComment (onComment ref (parsedComment callbacks))
				>>= {#set xmlSAXHandler->comment #} raw
			wrappedPI (onProcessingInstruction ref (parsedProcessingInstruction callbacks))
				>>= {#set xmlSAXHandler->processingInstruction #} raw
			
			FC.addForeignPtrFinalizer cCallbacks $ freeParserCallbacks raw
		
		ctxFP <- withForeignPtr cCallbacks $ \sax ->
			maybeWith withUTF8 filename $ \cFilename -> do
				Context ctx <- {#call xmlCreatePushParserCtxt #} sax nullPtr nullPtr 0 cFilename
				newForeignPtr xmlFreeParserCtxt ctx
		
		return $ Parser ctxFP cCallbacks onError ref

freeParserCallbacks :: Ptr () -> IO ()
freeParserCallbacks raw = do
	{# get xmlSAXHandler->startElementNs #} raw >>= freeHaskellFunPtr
	{# get xmlSAXHandler->endElementNs #} raw >>= freeHaskellFunPtr
	{# get xmlSAXHandler->characters #} raw >>= freeHaskellFunPtr
	{# get xmlSAXHandler->comment #} raw >>= freeHaskellFunPtr
	{# get xmlSAXHandler->processingInstruction #} raw >>= freeHaskellFunPtr

foreign import ccall "libxml/parser.h &xmlFreeParserCtxt"
	xmlFreeParserCtxt :: FunPtr (Ptr Context -> IO ())

-- | Feed some text into the parser. This may be performed multiple times
-- per 'Parser' value, in which case the internal parser state is retained
-- between computations.
-- 
parse :: Parser s -> B.ByteString -> ST.ST s ()
parse p bytes = parse' p $ \h ->
	B.unsafeUseAsCStringLen bytes $ \(cs, csLen) -> do
	{# call xmlParseChunk #} h cs (fromIntegral csLen) 0

-- | Finish parsing any buffered data, and check that the document was
-- closed correctly.
-- 
parseComplete :: Parser s -> ST.ST s ()
parseComplete p = parse' p $ \h ->
	{#call xmlParseChunk #} h nullPtr 0 1

withParser :: Parser s -> (Context -> IO a) -> ST.ST s a
withParser p io = ST.unsafeIOToST $ withForeignPtr (parserContext p) $ io . Context

parse' :: Parser s -> (Context -> IO CInt) -> ST.ST s ()
parse' p io = do
	let ref = parserErrorRef p
	ST.writeSTRef ref Nothing
	maybeErr <- withParser p $ \h -> do
		rc <- E.block $ io h
		touchForeignPtr $ parserCallbackPtr p
		maybeError <- ST.unsafeSTToIO $ ST.readSTRef ref
		case maybeError of
			Just err -> E.throwIO err
			Nothing -> return ()
		case rc of
			0 -> return Nothing
			_ -> do
				let Context h' = h
				errInfo <- {#call xmlCtxtGetLastError #} $ castPtr h'
				message <- peekUTF8 =<< {#get xmlError->message #} errInfo
				return $ Just message
	case maybeErr of
		Nothing -> return ()
		Just err -> parserOnError p err

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

convertCAttribute :: CAttribute -> IO X.Attribute
convertCAttribute (CAttribute c_ln c_pfx c_ns c_vbegin c_vend) = do
	ln <- peekUTF8 c_ln
	pfx <- maybePeek peekUTF8 c_pfx
	ns <- maybePeek peekUTF8 c_ns
	val <- peekUTF8Len (c_vbegin, minusPtr c_vend c_vbegin)
	return (X.Attribute (X.Name ln ns pfx) [X.ContentText val])

peekUTF8 :: CString -> IO T.Text
peekUTF8 cstr = do
	chunk <- B.packCString cstr
	return $ TE.decodeUtf8 $ BL.fromChunks [chunk]

peekUTF8Len :: CStringLen -> IO T.Text
peekUTF8Len cstr = do
	chunk <- B.packCStringLen cstr
	return $ TE.decodeUtf8 $ BL.fromChunks [chunk]

withUTF8 :: T.Text -> (CString -> IO a) -> IO a
withUTF8 text io = B.useAsCString bytes io where
	bytes = B.concat (BL.toChunks (TE.encodeUtf8 text))

-- Callback adapters
type CUString = Ptr CUChar

type StartElementNsSAX2Func = (Ptr () -> CUString -> CUString
                               -> CUString -> CInt -> Ptr CUString -> CInt
                               -> CInt -> Ptr CUString -> IO ())
type EndElementNsSAX2Func = (Ptr () -> CUString -> CUString -> CUString
                             -> IO ())
type CharactersSAXFunc = (Ptr () -> CUString -> CInt -> IO ())

type CommentSAXFunc = Ptr () -> CUString -> IO ()

type ProcessingInstructionSAXFunc = Ptr () -> CUString -> CUString -> IO ()

catchRef :: ST.STRef s (Maybe E.SomeException) -> Ptr () -> IO (ST.ST s ()) -> IO ()
catchRef ref ctx getST = E.catch io onError where
	io = do
		st <- getST
		E.unblock $ ST.unsafeSTToIO st
	onError e = do
		ST.unsafeSTToIO $ ST.writeSTRef ref $ Just e
		{# call xmlStopParser #} (Context (castPtr ctx))
		return ()

onBeginElement :: ST.STRef s (Maybe E.SomeException)
               -> (X.Name -> [X.Attribute] -> ST.ST s ())
               -> StartElementNsSAX2Func
onBeginElement ref st ctx cln cpfx cns _ _ n_attrs _ raw_attrs = catchRef ref ctx $ do
	ns <- maybePeek peekUTF8 $ castPtr cns
	pfx <- maybePeek peekUTF8 $ castPtr cpfx
	ln <- peekUTF8 $ castPtr cln
	c_attrs <- splitCAttributes n_attrs (castPtr raw_attrs)
	attrs <- mapM convertCAttribute c_attrs
	return $ st (X.Name ln ns pfx) attrs

onEndElement :: ST.STRef s (Maybe E.SomeException)
             -> (X.Name -> ST.ST s ())
             -> EndElementNsSAX2Func
onEndElement ref st ctx cln cpfx cns = catchRef ref ctx $ do
	ns <- maybePeek peekUTF8 $ castPtr cns
	pfx <- maybePeek peekUTF8 $ castPtr cpfx
	ln <- peekUTF8 $ castPtr cln
	return $ st (X.Name ln ns pfx)

onCharacters :: ST.STRef s (Maybe E.SomeException)
             -> (T.Text -> ST.ST s ())
             -> CharactersSAXFunc
onCharacters ref st ctx ctext ctextlen = catchRef ref ctx $ do
	text <- peekUTF8Len (castPtr ctext, fromIntegral ctextlen)
	return $ st text

onComment :: ST.STRef s (Maybe E.SomeException)
          -> (T.Text -> ST.ST s ())
          -> CommentSAXFunc
onComment ref st ctx ctext = catchRef ref ctx $ do
	text <- peekUTF8 (castPtr ctext)
	return $ st text

onProcessingInstruction :: ST.STRef s (Maybe E.SomeException)
                        -> (X.Instruction -> ST.ST s ())
                        -> ProcessingInstructionSAXFunc
onProcessingInstruction ref st ctx ctarget cdata = catchRef ref ctx $ do
	target <- peekUTF8 (castPtr ctarget)
	value <- peekUTF8 (castPtr cdata)
	return $ st (X.Instruction target value)

foreign import ccall "wrapper"
	wrappedBegin :: StartElementNsSAX2Func -> IO (FunPtr StartElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedEnd :: EndElementNsSAX2Func -> IO (FunPtr EndElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedText :: CharactersSAXFunc -> IO (FunPtr CharactersSAXFunc)

foreign import ccall "wrapper"
	wrappedComment :: CommentSAXFunc -> IO (FunPtr CommentSAXFunc)

foreign import ccall "wrapper"
	wrappedPI :: ProcessingInstructionSAXFunc -> IO (FunPtr ProcessingInstructionSAXFunc)

-- XML_SAX2_MAGIC
xmlSax2Magic :: CUInt
xmlSax2Magic = 0xDEEDBEAF
