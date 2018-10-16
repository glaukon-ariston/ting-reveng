{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Author: Glaukon Ariston
Date: 24.05.2016
Abstract:
    Assembler for the Ting pen's script language implemeted as an EDSL in 
    Haskell.

The assembler implementation code is stolen from the following project:

https://github.com/quietfanatic/neskell
http://wall.org/~lewis/2013/10/15/asm-monad.html

-}

module Main where

import Prelude hiding (and, or, not, return, putStr, putStrLn)
import IOUtil (putStr, putStrLn)

import Control.Monad.Fix (MonadFix(..))

import Ting.Instructions
import Ting.Registers
import Ting.Operand (Operand(..))
import Ting.Linker (makeBook, makeBookFromTest, BookDescription(..))
import Ting.Assembler2 (Assembler)
import Ting.Repl (repl, runTestBattery, debugTestBattery)

import Ting.Test (testBattery)


testBook :: IO ()
testBook = do
    makeBookFromTest testBattery bookDesc "books"
    runTestBattery testBattery
    where
        bookDesc = BookDescription 
            { _name = "Ting Sanity Test"
            , _publisher = "Glaukon Ariston"
            , _author = "Glaukon Ariston"
            , _bookVersion = "1"
            , _url = "http://github.com"
            , _thumb = ""
            , _file = ""
            }

main :: IO ()
main = do
    putStrLn "Ting Emulator Test"
    testBook

