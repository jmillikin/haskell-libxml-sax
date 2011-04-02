{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2011 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module Main (tests, main) where

import           Control.Monad (forM_)
import qualified Control.Monad.ST as ST

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.STRef as ST
import qualified Data.Text as T

import qualified Text.XML.LibXML.SAX as SAX
import qualified Data.XML.Types as X

import           Test.HUnit
import qualified Test.Framework as F
import           Test.Framework.Providers.HUnit (testCase)

tests :: [F.Test]
tests = [test_Everything]

main :: IO ()
main = F.defaultMain tests

test_Everything :: F.Test
test_Everything = testCase "everything" $ do
	let onError err = error (T.unpack err)
	
	ref <- ST.stToIO (ST.newSTRef [])
	p <- ST.stToIO (SAX.newParserST onError Nothing)
	ST.stToIO (setCallbacks p ref)
	forM_ chunks $ \(chunk, expected) -> do
		ST.stToIO (SAX.parseBytes p (B8.pack chunk))
		result <- ST.stToIO (fmap reverse (ST.readSTRef ref))
		ST.stToIO (ST.writeSTRef ref [])
		assertEqual ("chunk " ++ show chunk) expected result
	
	ST.stToIO (SAX.parseComplete p)
	result <- ST.stToIO (fmap reverse (ST.readSTRef ref))
	assertEqual "eof" [X.EventEndDocument] result

chunks :: [(String, [X.Event])]
chunks =
	[ ("", [])
	, ("<?something foo bar?>",
	   [ X.EventBeginDocument
	   , X.EventInstruction (X.Instruction "something" "foo bar")
	   ])
	, ("<!-- comment here -->",
	   [ X.EventComment " comment here "
	   ])
	, ("<!DOCTYPE SOME_DOCTYPE [",
	   [
	   ])
	, ("<!ENTITY ent \"some entity\">",
	   [
	   ])
	, ("]>",
	   [
	   ])
	, ("<doc>",
	   [ X.EventBeginElement "doc" Map.empty
	   ])
	, ("<with-attr a='b'>",
	   [ X.EventBeginElement "with-attr"
	     $ Map.fromList [("a", [X.ContentText "b"])]
	   ])
	, ("</with-attr>",
	   [ X.EventEndElement "with-attr"
	   ])
	, ("<no-close/>",
	   [ X.EventBeginElement "no-close" Map.empty
	   , X.EventEndElement "no-close"
	   ])
	, ("<with-text> text </with-text>",
	   [ X.EventBeginElement "with-text" Map.empty
	   , X.EventContent (X.ContentText " text ")
	   , X.EventEndElement "with-text"
	   ])
	, ("<with-cdata><![CDATA[<text>&here]]></with-cdata>",
	   [ X.EventBeginElement "with-cdata" Map.empty
	   , X.EventCDATA "<text>&here"
	   , X.EventEndElement "with-cdata"
	   ])
	, ("<with-entity>&ent;</with-entity>",
	   [ X.EventBeginElement "with-entity" Map.empty
	   , X.EventContent (X.ContentEntity "ent")
	   , X.EventEndElement "with-entity"
	   ])
	, ("<with-attr-entity a='ent &ent; attr'/>",
	   [ X.EventBeginElement "with-attr-entity"
	     $ Map.fromList [("a", [ X.ContentText "ent "
	                           , X.ContentEntity "ent"
	                           , X.ContentText " attr"])]
	   , X.EventEndElement "with-attr-entity"
	   ])
	, ("</doc>",
	   [ X.EventEndElement "doc"
	   ])
	]

setCallbacks :: SAX.Parser (ST.ST s) -> ST.STRef s [X.Event] -> ST.ST s ()
setCallbacks p ref = do
	let set cb st = SAX.setCallback p cb st
	let add ev = ST.modifySTRef ref (ev:) >> return True
	
	set SAX.parsedBeginDocument (add X.EventBeginDocument)
	set SAX.parsedEndDocument (add X.EventEndDocument)
	set SAX.parsedBeginElement (\n as -> add (X.EventBeginElement n as))
	set SAX.parsedEndElement (\n -> add (X.EventEndElement n))
	set SAX.parsedCharacters (\txt -> add (X.EventContent (X.ContentText txt)))
	set SAX.parsedReference (\name -> add (X.EventContent (X.ContentEntity name)))
	set SAX.parsedCDATA (\txt -> add (X.EventCDATA txt))
	set SAX.parsedComment (\txt -> add (X.EventComment txt))
	set SAX.parsedInstruction (\pi -> add (X.EventInstruction pi))
