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
	parser <- newParser
	[event] <- incrementalParse parser "<test>"
	BeginElement (QName "" "" "test") [] @=? event

testBeginNS = TestCase $ do
	parser <- newParser
	[event] <- incrementalParse parser "<test xmlns='urn:test'>" 
	BeginElement (QName "urn:test" "" "test") [] @=? event

testBeginPrefix = TestCase $ do
	parser <- newParser
	[event] <- incrementalParse parser "<t:test xmlns:t='urn:test'>" 
	BeginElement (QName "urn:test" "t" "test") [] @=? event

testEnd = TestCase $ do
	parser <- newParser
	[_, event] <- incrementalParse parser "<test/>"
	EndElement (QName "" "" "test") @=? event

testEndNS = TestCase $ do
	parser <- newParser
	[_, event] <- incrementalParse parser "<test xmlns='urn:test'/>"
	EndElement (QName "urn:test" "" "test") @=? event

testEndPrefix = TestCase $ do
	parser <- newParser
	[_, event] <- incrementalParse parser "<t:test xmlns:t='urn:test'/>"
	EndElement (QName "urn:test" "t" "test") @=? event

testText = TestCase $ do
	parser <- newParser
	[_, event, _] <- incrementalParse parser "<test>text here</test>"
	Characters "text here" @=? event

incrBegin = TestCase $ do
	parser <- newParser
	let parse = incrementalParse parser
	
	[] <- parse "<test"
	[event] <- parse ">"
	BeginElement (QName "" "" "test") [] @=? event

incrEnd = TestCase $ do
	parser <- newParser
	let parse = incrementalParse parser
	
	[_] <- parse "<test></test"
	[event] <- parse ">"
	EndElement (QName "" "" "test") @=? event

incrText = TestCase $ do
	parser <- newParser
	let parse = incrementalParse parser
	
	[_] <- parse "<test>text"
	[] <- parse " more text"
	[event, _] <- parse "</test>"
	Characters "text more text" @=? event

main = do
	runTestTT allTests

