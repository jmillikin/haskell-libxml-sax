{- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

#include <libxml/parser.h>

module Text.XML.LibXML.SAX
	( Parser
	, Event (..)
	, Error (..)
	, newParser
	, parse
	, eventsToElement
	) where

import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.FailableList as FL
import qualified Data.XML.Types as X
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Foreign
import Foreign.C
import Control.Exception (bracket)

data Event =
	  BeginElement X.Name [X.Attribute]
	| EndElement X.Name
	| Characters T.Text
	| Comment T.Text
	| ProcessingInstruction X.Instruction
	deriving (Show, Eq)

data Error = Error T.Text
	deriving (Show, Eq)

-- | An opaque reference to a libXML SAX parser.
-- 
newtype Parser = Parser (ForeignPtr Context)

data Context = Context
data SAXHandler = SAXHandler

instance Storable SAXHandler where
	sizeOf _ = {#sizeof xmlSAXHandler #}
	alignment _ = alignment (undefined :: FunPtr ())
	peekByteOff = undefined
	pokeByteOff _ _ _ = return ()

{#pointer *xmlParserCtxt as ContextPtr -> Context #}
{#pointer *xmlSAXHandler as SAXHandlerPtr -> SAXHandler #}
{#pointer *_xmlSAXHandler as SAXHandlerPtr nocode #}

-- | Construct a new, empty parser.
-- 
newParser :: IO Parser
newParser = let n = nullPtr in do
	context <- {#call xmlCreatePushParserCtxt #} n n n 0 n
	autoptr <- newForeignPtr xmlFreeParserCtxt context
	return $ Parser autoptr

foreign import ccall "libxml/parser.h &xmlFreeParserCtxt"
	xmlFreeParserCtxt :: FunPtr (Ptr Context -> IO ())

-- | Feed some text into the parser. This may be performed multiple times
-- per 'Parser' value, in which case the internal parser state is retained
-- between computations.
-- 
-- If the third parameter is 'True', the parser assumes that this is the
-- last input and checks that the document was closed correctly.
-- 
parse :: Parser -> BL.ByteString -> Bool -> IO (FL.FailableList Error Event)
parse (Parser fptr) lazyBytes final = io where
	io =
		withForeignPtr fptr $ \ctx ->
		withHandlers ctx $ \ref -> do
		chunkRC <- foldM (parseChunk ctx) Nothing $ BL.toChunks lazyBytes
		rc <- case chunkRC of
			Nothing -> if final
				then {#call xmlParseChunk #} ctx nullPtr 0 1
				else return 0
			Just err -> return err
		events <- convertEvents `fmap` readIORef ref
		case rc of
			0 -> return events
			_ -> do
				errInfo <- {#call xmlCtxtGetLastError #} (castPtr ctx)
				message <- peekCString =<< {#get xmlError->message #} errInfo
				return $ FL.append events $ FL.Fail $ Error $ T.pack message
	
	parseChunk _ (Just err) _ = return $ Just err
	parseChunk ctx _ bytes = B.unsafeUseAsCStringLen bytes $ \(cs, csLen) -> do
		rc <- {#call xmlParseChunk #} ctx cs (fromIntegral csLen) 0
		return $ if rc == 0
			then Nothing
			else Just rc
	
	convertEvents = foldr FL.Next FL.Done . reverse

withHandlers :: Ptr Context -> (IORef [Event] -> IO a) -> IO a
withHandlers ctxt block = do
	eventRef <- newIORef []
	withFunPtr (onBeginElement eventRef) wrappedBegin $ \b -> do
	withFunPtr (onEndElement eventRef) wrappedEnd $ \e -> do
	withFunPtr (onCharacters eventRef) wrappedText $ \t -> do
	withFunPtr (onComment eventRef) wrappedComment $ \c -> do
	withFunPtr (onProcessingInstruction eventRef) wrappedProcessingInstruction $ \pI -> do
	
	bracket
		(setContextHandlers ctxt)
		(freeContextHandlers ctxt) $ \handlers -> do
		
		{#set xmlSAXHandler->initialized #} handlers xmlSax2Magic
		{#set xmlSAXHandler->startElementNs #} handlers b
		{#set xmlSAXHandler->endElementNs #} handlers e
		{#set xmlSAXHandler->characters #} handlers t
		{#set xmlSAXHandler->comment #} handlers c
		{#set xmlSAXHandler->processingInstruction #} handlers pI
		
		block eventRef

setContextHandlers :: Ptr Context -> IO (Ptr SAXHandler)
setContextHandlers ctxt = do
	handlers <- {#call calloc #} 1 {#sizeof xmlSAXHandler #}
	let handlers' = castPtr handlers
	{# set xmlParserCtxt->sax #} ctxt handlers'
	return handlers'

freeContextHandlers :: Ptr Context -> Ptr SAXHandler -> IO ()
freeContextHandlers ctxt handlers = do
	{# set xmlParserCtxt->sax #} ctxt nullPtr
	free handlers

withFunPtr :: a -> (a -> IO (FunPtr a)) -> (FunPtr a -> IO b) -> IO b
withFunPtr f mkPtr block = bracket (mkPtr f) freeHaskellFunPtr block

-- localname, prefix, namespace, value_begin, value_end
data CAttribute = CAttribute CString CString CString CString CString

splitCAttributes :: CInt -> Ptr CString -> IO [CAttribute]
splitCAttributes = splitCAttributes' 0 where
	splitCAttributes' _      0 _     = return []
	splitCAttributes' offset n attrs = do
		c_ln <- peekElemOff attrs (offset + 0)
		c_prefix <- peekElemOff attrs (offset + 1)
		c_ns <- peekElemOff attrs (offset + 2)
		c_vbegin <- peekElemOff attrs (offset + 3)
		c_vend <- peekElemOff attrs (offset + 4)
		as <- splitCAttributes' (offset + 5) (n - 1) attrs
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

type CUString = Ptr CUChar

type StartElementNsSAX2Func = (Ptr () -> CUString -> CUString
                               -> CUString -> CInt -> Ptr CUString -> CInt
                               -> CInt -> Ptr CUString -> IO ())
type EndElementNsSAX2Func = (Ptr () -> CUString -> CUString -> CUString
                             -> IO ())
type CharactersSAXFunc = (Ptr () -> CUString -> CInt -> IO ())

type CommentSAXFunc = Ptr () -> CUString -> IO ()

type ProcessingInstructionSAXFunc = Ptr () -> CUString -> CUString -> IO ()

onBeginElement :: IORef [Event] -> StartElementNsSAX2Func
onBeginElement eventref _ cln cpfx cns _ _ n_attrs _ raw_attrs = do
	ns <- maybePeek peekUTF8 $ castPtr cns
	pfx <- maybePeek peekUTF8 $ castPtr cpfx
	ln <- peekUTF8 $ castPtr cln
	es <- readIORef eventref
	c_attrs <- splitCAttributes n_attrs (castPtr raw_attrs)
	attrs <- mapM convertCAttribute c_attrs
	writeIORef eventref ((BeginElement (X.Name ln ns pfx) attrs):es)

onEndElement :: IORef [Event] -> EndElementNsSAX2Func
onEndElement eventref _ cln cpfx cns = do
	ns <- maybePeek peekUTF8 $ castPtr cns
	pfx <- maybePeek peekUTF8 $ castPtr cpfx
	ln <- peekUTF8 $ castPtr cln
	es <- readIORef eventref
	writeIORef eventref ((EndElement (X.Name ln ns pfx)):es)

onCharacters :: IORef [Event] -> CharactersSAXFunc
onCharacters eventref _ ctext ctextlen = do
	text <- peekUTF8Len (castPtr ctext, fromIntegral ctextlen)
	es <- readIORef eventref
	writeIORef eventref ((Characters text):es)

onComment :: IORef [Event] -> CommentSAXFunc
onComment eventRef _ ctext = do
	text <- peekUTF8 (castPtr ctext)
	es <- readIORef eventRef
	writeIORef eventRef ((Comment text):es)

onProcessingInstruction :: IORef [Event] -> ProcessingInstructionSAXFunc
onProcessingInstruction eventRef _ ctarget cdata = do
	target <- peekUTF8 (castPtr ctarget)
	value <- peekUTF8 (castPtr cdata)
	es <- readIORef eventRef
	let instruction = X.Instruction target value
	writeIORef eventRef ((ProcessingInstruction instruction):es)

-- | Convert a list of events to a single 'X.Element'. If the events do not
-- contain at least one valid element, 'Nothing' will be returned instead.
eventsToElement :: [Event] -> Maybe X.Element
eventsToElement es = case eventsToNodes es >>= X.isElement of
	(e:_) -> Just e
	_ -> Nothing

eventsToNodes :: [Event] -> [X.Node]
eventsToNodes = concatMap blockToNodes . splitBlocks

-- Split event list into a sequence of "blocks", which are the events including
-- and between a pair of tags. <start><start2/></start> and <start/> are both
-- single blocks.
splitBlocks :: [Event] -> [[Event]]
splitBlocks es = ret where
	(_, _, ret) = foldl splitBlocks' (0, [], []) es
	
	splitBlocks' (depth, accum, allAccum) e = split where
		split = if depth' == 0
			then (depth', [], allAccum ++ [accum'])
			else (depth', accum', allAccum)
		accum' = accum ++ [e]
		depth' :: Integer
		depth' = depth + case e of
			(BeginElement _ _) -> 1
			(EndElement _) -> (- 1)
			_ -> 0

blockToNodes :: [Event] -> [X.Node]
blockToNodes [] = []
blockToNodes (begin:rest) = nodes where
	end = last rest
	nodes = case (begin, end) of
		(BeginElement name' attrs, EndElement _) -> [node name' attrs]
		(Characters t, _) -> [X.NodeContent (X.ContentText t)]
		_ -> []
	
	node n as = X.NodeElement $ X.Element n as $ eventsToNodes $ init rest

foreign import ccall "wrapper"
	wrappedBegin :: StartElementNsSAX2Func -> IO (FunPtr StartElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedEnd :: EndElementNsSAX2Func -> IO (FunPtr EndElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedText :: CharactersSAXFunc -> IO (FunPtr CharactersSAXFunc)

foreign import ccall "wrapper"
	wrappedComment :: CommentSAXFunc -> IO (FunPtr CommentSAXFunc)

foreign import ccall "wrapper"
	wrappedProcessingInstruction :: ProcessingInstructionSAXFunc -> IO (FunPtr ProcessingInstructionSAXFunc)

-- XML_SAX2_MAGIC
xmlSax2Magic :: CUInt
xmlSax2Magic = 0xDEEDBEAF
