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

module Main () where

import Test.HUnit
import Text.XML.LibXML.SAX

allTests = "allTests" ~: TestList [beginTests, endTests, textTests, incrementalTests]
beginTests = "beginTests" ~: TestList [testBegin, testBeginNS, testBeginPrefix]
endTests = "endTests" ~: TestList [testEnd, testEndNS, testEndPrefix]
textTests = "textTests" ~: TestList [testText]
incrementalTests = "incrementalTests" ~: TestList [incrBegin, incrEnd, incrText]

testBegin = TestCase $ do
	parser <- mkParser
	[event] <- parse parser "<test>" False
	BeginElement (QName "" "" "test") [] @=? event

testBeginNS = TestCase $ do
	parser <- mkParser
	[event] <- parse parser "<test xmlns='urn:test'>" False
	BeginElement (QName "urn:test" "" "test") [] @=? event

testBeginPrefix = TestCase $ do
	parser <- mkParser
	[event] <- parse parser "<t:test xmlns:t='urn:test'>" False
	BeginElement (QName "urn:test" "t" "test") [] @=? event

testEnd = TestCase $ do
	parser <- mkParser
	[_, event] <- parse parser "<test/>" False
	EndElement (QName "" "" "test") @=? event

testEndNS = TestCase $ do
	parser <- mkParser
	[_, event] <- parse parser "<test xmlns='urn:test'/>" False
	EndElement (QName "urn:test" "" "test") @=? event

testEndPrefix = TestCase $ do
	parser <- mkParser
	[_, event] <- parse parser "<t:test xmlns:t='urn:test'/>" False
	EndElement (QName "urn:test" "t" "test") @=? event

testText = TestCase $ do
	parser <- mkParser
	[_, event, _] <- parse parser "<test>text here</test>" False
	Characters "text here" @=? event

incrBegin = TestCase $ do
	parser <- mkParser
	let parse' = parse parser
	
	[] <- parse' "<test" False
	[event] <- parse' ">" False
	BeginElement (QName "" "" "test") [] @=? event

incrEnd = TestCase $ do
	parser <- mkParser
	let parse' = parse parser
	
	[_] <- parse' "<test></test" False
	[event] <- parse' ">" False
	EndElement (QName "" "" "test") @=? event

incrText = TestCase $ do
	parser <- mkParser
	let parse' = parse parser
	
	[_] <- parse' "<test>text" False
	[] <- parse' " more text" False
	[event, _] <- parse' "</test>" False
	Characters "text more text" @=? event

main = do
	runTestTT allTests

