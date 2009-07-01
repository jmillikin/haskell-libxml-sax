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

module Text.XML.LibXML.SAX (
	 Parser
	,Event(..)
	,Attribute(..)
	,QName(..)
	,mkParser
	,parse
	) where

import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Foreign
import Foreign.C
import Control.Exception (bracket)

data Event =
	  BeginElement QName [Attribute]
	| EndElement QName
	| Characters String
	| ParseError String
	deriving (Show, Eq)

data Attribute = Attribute QName String
	deriving (Show, Eq)

-- Namespace, prefix, local name
data QName = QName String String String
	deriving (Show, Eq)

data Parser = Parser (ForeignPtr Context)

data Context = Context
data SAXHandler = SAXHandler

instance Storable SAXHandler where
	sizeOf _ = {#sizeof xmlSAXHandler #}
	alignment _ = alignment (undefined :: FunPtr ())
	peekByteOff = undefined
	pokeByteOff handler offset val = return ()

{#pointer *xmlParserCtxt as ContextPtr -> Context #}
{#pointer *xmlSAXHandler as SAXHandlerPtr -> SAXHandler #}
{#pointer *_xmlSAXHandler as SAXHandlerPtr nocode #}

mkParser :: IO Parser
mkParser = let n = nullPtr in do
	context <- {#call xmlCreatePushParserCtxt #} n n n 0 n
	autoptr <- newForeignPtr xmlFreeParserCtxt context
	return $ Parser autoptr

foreign import ccall "libxml/parser.h &xmlFreeParserCtxt"
	xmlFreeParserCtxt :: FunPtr (Ptr Context -> IO ())

parse :: Parser -> String -> Bool -> IO [Event]
parse (Parser fptr) s final = do
	withCStringLen s $ \(cs, cs_len) -> do
	withForeignPtr fptr $ \ctxt -> do
	withHandlers ctxt $ \eventRef -> do
	
	let cFinal = if final then 1 else 0
	
	rc <- {#call xmlParseChunk #} ctxt cs (fromIntegral cs_len) cFinal
	events <- readIORef eventRef
	return $ events ++ (checkReturn rc)
	
withHandlers :: Ptr Context -> (IORef [Event] -> IO a) -> IO a
withHandlers ctxt block = do
	eventRef <- newIORef []
	withFunPtr (onBeginElement eventRef) wrappedBegin $ \b -> do
	withFunPtr (onEndElement eventRef) wrappedEnd $ \e -> do
	withFunPtr (onCharacters eventRef) wrappedText $ \t -> do
	
	bracket
		(setContextHandlers ctxt)
		(freeContextHandlers ctxt) $ \handlers -> do
		
		{#set xmlSAXHandler->initialized #} handlers xmlSax2Magic
		{#set xmlSAXHandler->startElementNs #} handlers b
		{#set xmlSAXHandler->endElementNs #} handlers e
		{#set xmlSAXHandler->characters #} handlers t
		
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

checkReturn :: CInt -> [Event]
checkReturn rc = if rc == 0 then [] else [ParseError (show rc)]
-- TODO: show full error message

-- localname, prefix, namespace, value_begin, value_end
data CAttribute = CAttribute CString CString CString CString CString

splitCAttributes :: CInt -> Ptr CString -> IO [CAttribute]
splitCAttributes = splitCAttributes' 0

splitCAttributes' _      0 _     = return []
splitCAttributes' offset n attrs = do
	c_ln <- peekElemOff attrs (offset + 0)
	c_prefix <- peekElemOff attrs (offset + 1)
	c_ns <- peekElemOff attrs (offset + 2)
	c_vbegin <- peekElemOff attrs (offset + 3)
	c_vend <- peekElemOff attrs (offset + 4)
	as <- splitCAttributes' (offset + 5) (n - 1) attrs
	return (CAttribute c_ln c_prefix c_ns c_vbegin c_vend : as)

convertCAttribute :: CAttribute -> IO Attribute
convertCAttribute (CAttribute c_ln c_pfx c_ns c_vbegin c_vend) = do
	ln <- peekCString c_ln
	pfx <- peekNullable c_pfx
	ns <- peekNullable c_ns
	val <- peekCStringLen (c_vbegin, minusPtr c_vend c_vbegin)
	return (Attribute (QName ns pfx ln) val)

peekNullable :: CString -> IO String
peekNullable ptr = if ptr == nullPtr then return "" else peekCString ptr

type CUString = Ptr CUChar

type StartElementNsSAX2Func = (Ptr () -> CUString -> CUString
                               -> CUString -> CInt -> Ptr CUString -> CInt
                               -> CInt -> Ptr CUString -> IO ())
type EndElementNsSAX2Func = (Ptr () -> CUString -> CUString -> CUString
                             -> IO ())
type CharactersSAXFunc = (Ptr () -> CUString -> CInt -> IO ())

onBeginElement :: IORef [Event] -> StartElementNsSAX2Func
onBeginElement eventref _ cln cpfx cns _ _ n_attrs _ raw_attrs = do
	ns <- peekNullable $ castPtr cns
	pfx <- peekNullable $ castPtr cpfx
	ln <- peekCString $ castPtr cln
	es <- readIORef eventref
	c_attrs <- splitCAttributes n_attrs (castPtr raw_attrs)
	attrs <- mapM convertCAttribute c_attrs
	writeIORef eventref (es ++ [BeginElement (QName ns pfx ln) attrs])

onEndElement :: IORef [Event] -> EndElementNsSAX2Func
onEndElement eventref _ cln cpfx cns = do
	ns <- peekNullable $ castPtr cns
	pfx <- peekNullable $ castPtr cpfx
	ln <- peekCString $ castPtr cln
	es <- readIORef eventref
	writeIORef eventref (es ++ [EndElement (QName ns pfx ln)])

onCharacters :: IORef [Event] -> CharactersSAXFunc
onCharacters eventref _ ctext ctextlen = do
	text <- (peekCStringLen (castPtr ctext, fromIntegral ctextlen))
	es <- readIORef eventref
	writeIORef eventref (es ++ [Characters text])

foreign import ccall "wrapper"
	wrappedBegin :: StartElementNsSAX2Func -> IO (FunPtr StartElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedEnd :: EndElementNsSAX2Func -> IO (FunPtr EndElementNsSAX2Func)

foreign import ccall "wrapper"
	wrappedText :: CharactersSAXFunc -> IO (FunPtr CharactersSAXFunc)

-- XML_SAX2_MAGIC
xmlSax2Magic = 0xDEEDBEAF :: CUInt
