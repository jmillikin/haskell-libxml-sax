{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import           Control.Monad (forM_)
import qualified Control.Monad.ST as ST

import qualified Data.ByteString.Char8 as B8
import qualified Data.STRef as ST
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
        , test_PlainCDATA
        , test_PassthroughCDATA
        , test_AttributeContent
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
	, (" text &ref; ",
	   [
	   ])
	, ("</doc>",
	   [ X.EventContent (X.ContentText " text ")
	   , X.EventContent (X.ContentEntity "ref")
	   , X.EventContent (X.ContentText " ")
	   , X.EventEndElement "doc"
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
		set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
		set SAX.parsedReference (\name -> add (X.EventContent (X.ContentEntity name)))
		set SAX.parsedCDATA (\txt -> add (X.EventCDATA txt))
	)
	[ ("<!DOCTYPE SOME_DOCTYPE [<!ENTITY ref \"some reference\">]>",
	  [
	  ])
	, ("<doc a='text &ref; text'/>",
	   [ X.EventBeginElement "doc" [("a", [X.ContentText "text ", X.ContentEntity "ref", X.ContentText " text"])]
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

test_Chunks :: String -> (SAX.Parser (ST.ST ST.RealWorld) -> (X.Event -> ST.ST ST.RealWorld Bool) -> ST.ST ST.RealWorld ()) -> [(String, [X.Event])] -> F.Test
test_Chunks name setup chunks = testCase name $ do
	let onError err = error (T.unpack err)
	
	ref <- ST.stToIO (ST.newSTRef [])
	p <- ST.stToIO (SAX.newParserST onError Nothing)
	let add ev = ST.modifySTRef ref (ev:) >> return True
	ST.stToIO (setup p add)
	
	forM_ chunks $ \(chunk, expected) -> do
		ST.stToIO (SAX.parseBytes p (B8.pack chunk))
		result <- ST.stToIO (fmap reverse (ST.readSTRef ref))
		ST.stToIO (ST.writeSTRef ref [])
		assertEqual ("chunk " ++ show chunk) expected result
	
	ST.stToIO (SAX.parseComplete p)
	result <- ST.stToIO (fmap reverse (ST.readSTRef ref))
	assertEqual "eof" [] result
