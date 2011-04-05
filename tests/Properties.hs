{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import           Control.Monad (forM_)

import qualified Data.ByteString.Char8 as B8
import           Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import qualified Data.Text as T

import qualified Text.XML.LibXML.SAX as SAX
import qualified Data.XML.Types as X

import           Test.HUnit
import qualified Test.Framework as F
import           Test.Framework.Providers.HUnit (testCase)

tests :: [F.Test]
tests = [ test_Instruction
        , test_Comment
        , test_InternalSubset
        , test_InternalSubsetEmpty
        , test_ExternalSubset
        , test_Element
        , test_Content
        , test_ContentNoReference
        , test_PlainCDATA
        , test_PassthroughCDATA
        , test_AttributeContent
        , test_AttributeContentNoReference
        , test_AttributeOrder
        ]

main :: IO ()
main = F.defaultMain tests

test_Instruction :: F.Test
test_Instruction = test_Chunks "instruction"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedInstruction (\pi -> add (X.EventInstruction pi))
	)
	[ ("<?something foo bar?>",
	   [ X.EventInstruction (X.Instruction "something" "foo bar")
	   ])
	, ("<doc/>", [])
	]

test_Comment :: F.Test
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

test_InternalSubsetEmpty :: F.Test
test_InternalSubsetEmpty = test_Chunks "internal subset (empty)"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
		set SAX.parsedInternalSubset (\name id -> add (X.EventBeginDoctype name id))
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

test_InternalSubset :: F.Test
test_InternalSubset = test_Chunks "internal subset"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
		set SAX.parsedInternalSubset (\name id -> add (X.EventBeginDoctype name id))
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

test_ExternalSubset :: F.Test
test_ExternalSubset = test_Chunks "external subset"
	(\p add -> do
		let set cb st = SAX.setCallback p cb st
		set SAX.parsedComment (\txt -> add (X.EventComment txt))
		set SAX.parsedExternalSubset (\name id -> add (X.EventBeginDoctype name id))
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

test_Element :: F.Test
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

test_Content :: F.Test
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

test_ContentNoReference :: F.Test
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

test_PlainCDATA :: F.Test
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

test_PassthroughCDATA :: F.Test
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

test_AttributeContent :: F.Test
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
	, ("<doc a='text &ref; text'/>",
	   [ X.EventBeginElement "doc" [("a", [X.ContentText "text ", X.ContentEntity "ref", X.ContentText " text"])]
	   , X.EventEndElement "doc"
	   ])
	]

test_AttributeContentNoReference :: F.Test
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

test_AttributeOrder :: F.Test
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

test_Chunks :: String -> (SAX.Parser IO -> (X.Event -> IO Bool) -> IO ()) -> [(String, [X.Event])] -> F.Test
test_Chunks name setup chunks = testCase name $ do
	ref <- newIORef []
	p <- SAX.newParserIO Nothing
	
	SAX.setCallback p SAX.reportError (error . T.unpack)
	
	let add ev = modifyIORef ref (ev:) >> return True
	setup p add
	
	forM_ chunks $ \(chunk, expected) -> do
		SAX.parseBytes p (B8.pack chunk)
		result <- fmap reverse (readIORef ref)
		writeIORef ref []
		assertEqual ("chunk " ++ show chunk) expected result
	
	SAX.parseComplete p
	result <- fmap reverse (readIORef ref)
	assertEqual "eof" [] result
