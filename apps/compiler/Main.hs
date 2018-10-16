{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Author: Glaukon Ariston
Date: 14.10.2018
Abstract:
    Assembler for the Ting pen's script language implemeted as an EDSL in 
    Haskell.

The assembler implementation code is stolen from the following project:

https://github.com/quietfanatic/neskell
http://wall.org/~lewis/2013/10/15/asm-monad.html

stack exec ting-compiler "C:\Tata\Language\English\New Building Blocks 3_Textbook"
stack exec ting-compiler "C:\Tata\Language\German\Giorgio Motta, Mirjana Klobučar\Wir+2 Udžbenik njemačkog jezika za 5. razred osnovne škole 2. godina učenja"

-}

module Main where

import Prelude hiding (Word, getContents, putStr, putStrLn) --(IO, Read, Show, String)
import IOUtil (putStr, putStrLn)

import Control.Monad (filterM)
import Control.Monad.Fix (MonadFix(..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)
import System.FilePath ((</>), takeExtension, takeBaseName)
import System.Environment (getArgs)
import Data.List (sort)

import Ting.Instructions
import Ting.Registers
import Ting.Operand (Operand(..))
import Ting.Linker (makeBook, uploadBook, BookDescription(..))
import Ting.Assembler2 (Assembler)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn . unlines $ [ "Usage:"
                , "ting-compile mp3Directory"
                ]
        (dir:[]) -> do
            simpleCompile dir
        _ -> error "Unhandled case"


simpleCompile :: FilePath -> IO ()
simpleCompile dir = do
    soundsLib <- sounds dir
    let naked = True
    makeBook naked bookId bookDesc tingId program soundsLib destinationDir
    uploadBook bookId destinationDir



destinationDir :: FilePath
destinationDir = "books"


bookId :: Int 
bookId = 8200

tingId :: Int 
tingId = 15001

bookDesc :: BookDescription 
bookDesc = BookDescription 
    { _name = "New Building Blocks 3"
    , _publisher = "Profil Klett"
    , _author = "Kristina Čajo Anđel, Ankica Knezović"
    --, _author = "Kristina Cajo Andel, Ankica Knezovic"
    , _bookVersion = "1"
    , _url = "https://github.com/glaukon-ariston/ting-reveng"
    , _thumb = ""
    , _file = ""
    }


program :: (MonadFix m, Instructions m) => Program m ()
program = [ 
    ("main", TopLevel, mdo 
        end
    )
    ]


sounds :: FilePath -> IO SoundLib
sounds dir = do
    items <- listDirectory dir
    withCurrentDirectory dir $ do
        files <- filterM doesFileExist items
        let mp3s = sort . filter (\f -> takeExtension f == ".mp3") $ files
        return $ [(takeBaseName m, dir </> m) | m <- mp3s]
