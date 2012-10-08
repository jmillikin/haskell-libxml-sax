{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as B8
import           Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Text as T

import           Test.Chell

import qualified Text.XML.LibXML.SAX as SAX
import qualified Data.XML.Types as X

tests :: Suite
tests = suite "libxml-sax"
	test_Instruction
	test_Comment
	test_InternalSubset
	test_InternalSubsetEmpty
	test_ExternalSubset
	test_Element
	test_Content
	test_ContentNoReference
	test_PlainCDATA
	test_PassthroughCDATA
	test_AttributeContent
	test_AttributeContentNoReference
	test_AttributeOrder
	test_AttributeContentAmpersand

main :: IO ()
main = defaultMain [tests]

test_Instruction :: Test
test_Instruction = test_Chunks "instruction"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedInstruction (\pi_ -> add (X.EventInstruction pi_))
	)
	[ ("<?something foo bar?>",
	   [ X.EventInstruction (X.Instruction "something" "foo bar")
	   ])
	, ("<doc/>", [])
	]

test_Comment :: Test
test_Comment = test_Chunks "comment"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
	)
	[ ("<!-- something foo bar -->",
	   [ X.EventComment " something foo bar "
	   ])
	, ("<doc/>", [])
	]

test_InternalSubsetEmpty :: Test
test_InternalSubsetEmpty = test_Chunks "internal subset (empty)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
		set SAX.parsedInternalSubset (\name id_ -> add (X.EventBeginDoctype name id_))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE PUBLIC \"foo\" \"bar\" [\n",
	   [
	   ])
	, ("]", [])
	, (">",
	   [ X.EventBeginDoctype "SOME_DOCTYPE" (Just (X.PublicID "foo" "bar"))
	   ])
	, ("<doc/>", [])
	]

test_InternalSubset :: Test
test_InternalSubset = test_Chunks "internal subset"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
		set SAX.parsedInternalSubset (\name id_ -> add (X.EventBeginDoctype name id_))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE PUBLIC \"foo\" \"bar\" [\n",
	   [
	   ])
	, ("<!ENTITY ent \"some entity\">",
	   [ X.EventBeginDoctype "SOME_DOCTYPE" (Just (X.PublicID "foo" "bar"))
	   ])
	, ("]", [])
	, (">", [])
	, ("<doc/>", [])
	]

test_ExternalSubset :: Test
test_ExternalSubset = test_Chunks "external subset"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
		set SAX.parsedExternalSubset (\name id_ -> add (X.EventBeginDoctype name id_))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE PUBLIC \"foo\" \"bar\" [\n",
	   [
	   ])
	, ("<!ENTITY ent \"some entity\">",
	   [
	   ])
	, ("]", [])
	, (">",
	   [ X.EventBeginDoctype "SOME_DOCTYPE" (Just (X.PublicID "foo" "bar"))
	   ])
	, ("<doc/>", [])
	]

test_Element :: Test
test_Element = test_Chunks "element begin/end"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
		set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
	)
	[ ("<doc>",
	   [ X.EventBeginElement "doc" []
	   ])
	, ("</doc>",
	   [ X.EventEndElement "doc"
	   ])
	]

test_Content :: Test
test_Content = test_Chunks "content"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
		set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
		set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
		set SAX.parsedReference (\name -> add (X.EventContent (X.ContentEntity name)))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE [<!ENTITY ref \"some reference\">]>",
	  [
	  ])
	, ("<doc>",
	   [ X.EventBeginElement "doc" []
	   ])
	, (" text &ref; <",
	   [ X.EventContent (X.ContentText " text ")
	   , X.EventContent (X.ContentEntity "ref")
	   , X.EventContent (X.ContentText " ")
	   ])
	, ("/doc>",
	   [ X.EventEndElement "doc"
	   ])
	]

test_ContentNoReference :: Test
test_ContentNoReference = test_Chunks "content (no reference CB)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE [<!ENTITY ref \"some reference\">]>",
	  [
	  ])
	, ("<doc>", [])
	, (" text &ref; <",
	   [ X.EventContent (X.ContentText " text ")
	   , X.EventContent (X.ContentText "some reference")
	   , X.EventContent (X.ContentText " ")
	   ])
	, ("/doc>",
	   [
	   ])
	]

test_PlainCDATA :: Test
test_PlainCDATA = test_Chunks "cdata (plain)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
	)
	[ ("<doc>", [])
	, ("<![CDATA[<cdata>]]>",
	   [ X.EventContent (X.ContentText "<cdata>")
	   ])
	, ("</doc>", [])
	]

test_PassthroughCDATA :: Test
test_PassthroughCDATA = test_Chunks "cdata (passthrough)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
		set SAX.parsedCDATA (\txt -> add (X.EventCDATA txt))
	)
	[ ("<doc>", [])
	, ("<![CDATA[<cdata>]]>",
	   [ X.EventCDATA "<cdata>"
	   ])
	, ("</doc>", [])
	]

test_AttributeContent :: Test
test_AttributeContent = test_Chunks "attribute content"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
		set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
		set SAX.parsedReference (\name -> add (X.EventContent (X.ContentEntity name)))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE [<!ENTITY ref \"some reference\">]>",
	  [
	  ])
	, ("<doc a='text &amp; &ref; text'/>",
	   [ X.EventBeginElement "doc" [("a", [ X.ContentText "text ", X.ContentText "&", X.ContentText " "
	                                      , X.ContentEntity "ref", X.ContentText " text"])]
	   , X.EventEndElement "doc"
	   ])
	]

test_AttributeContentNoReference :: Test
test_AttributeContentNoReference = test_Chunks "attribute content (no reference CB)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
		set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE [<!ENTITY ref \"some reference\">]>",
	  [
	  ])
	, ("<doc a='text &ref; text'/>",
	   [ X.EventBeginElement "doc" [("a", [X.ContentText "text some reference text"])]
	   , X.EventEndElement "doc"
	   ])
	]

test_AttributeContentAmpersand :: Test
test_AttributeContentAmpersand = test_Chunks "attribute content (with ampersand)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
		set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
	)
	[ ("<doc a='&amp;foo'/>",
	   [ X.EventBeginElement "doc" [("a", [X.ContentText "&foo"])]
	   , X.EventEndElement "doc"
	   ])
	]

test_AttributeOrder :: Test
test_AttributeOrder = test_Chunks "attribute order"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
		set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
		set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
		set SAX.parsedReference (\name -> add (X.EventContent (X.ContentEntity name)))
		set SAX.parsedCDATA (\txt -> add (X.EventCDATA txt))
	)
	[ ("<doc z='1' a='2' b='3'/>",
	   [ X.EventBeginElement "doc" [("z", [X.ContentText "1"]), ("a", [X.ContentText "2"]), ("b", [X.ContentText "3"])]
	   , X.EventEndElement "doc"
	   ])
	]

test_Chunks :: String -> (SAX.Parser IO -> (X.Event -> IO Bool) -> IO ()) -> [(String, [X.Event])] -> Test
test_Chunks name setup chunks = assertions name $ do
	ref <- liftIO (newIORef [])
	p <- liftIO (SAX.newParserIO Nothing)
	
	liftIO (SAX.setCallback p SAX.reportError (error . T.unpack))
	
	let add ev = modifyIORef ref (ev:) >> return True
	liftIO (setup p add)
	
	forM_ chunks $ \(chunk, expected) -> do
		liftIO (SAX.parseBytes p (B8.pack chunk))
		result <- liftIO (fmap reverse (readIORef ref))
		liftIO (writeIORef ref [])
		$assert (equal expected result)
	
	liftIO (SAX.parseComplete p)
	result <- liftIO (fmap reverse (readIORef ref))
	$assert (equal [] result)
