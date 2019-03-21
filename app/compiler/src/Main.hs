{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
Author: Glaukon Ariston
Date: 14.10.2018
Abstract:
    Assembler for the Ting pen's script language implemeted as an EDSL in 
    Haskell.


stack exec ting-compiler "books\New_Building_Blocks_1_TextBook.yaml"
stack exec ting-compiler "books\New_Building_Blocks_1_WorkBook.yaml"
stack exec ting-compiler "books\New_Building_Blocks_2_TextBook.yaml"
stack exec ting-compiler "books\New_Building_Blocks_2_WorkBook.yaml"
stack exec ting-compiler "books\New_Building_Blocks_3_TextBook.yaml"
stack exec ting-compiler "books\New_Building_Blocks_3_WorkBook.yaml"
stack exec ting-compiler "books\Wir+2_TextBook.yaml"
stack exec ting-compiler "books\Glazbena petica_TextBook.yaml"
stack exec ting-compiler "books\Razigrani_Zvuci_1_TextBook_Pjevajmo.yaml"

ghcid --command="stack repl compiler" -o ghcid.txt

-}

module Main where

import Prelude hiding (Word, getContents, putStr, putStrLn) --(IO, Read, Show, String)
import System.IO.Console.IOUtil (putStr, putStrLn)
import Formatting (fprint, (%))
import Formatting.Formatters (int, string)
import Text.Printf (printf)

import Control.Monad (filterM)
import Control.Monad.Fix (MonadFix(..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)
import System.FilePath ((</>), takeExtension, takeBaseName)
import System.Environment (getArgs)
import Data.List (sort)

import Ting.Instructions
import Ting.Registers
import Ting.Operand (Operand(..))
import Ting.Linker (makeBook, uploadBook)
import Ting.Assembler2 (Assembler)
import Ting.BookConfig (BookConfig(..), BookDescription(..), parseBookConfig)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn . unlines $ [ "Usage:"
                , "ting-compile yamlFile"
                ]
        (yamlFile:[]) -> do
            simpleCompile yamlFile
        xs -> error $ "Unhandled case: " ++ show xs


simpleCompile :: FilePath -> IO ()
simpleCompile yamlFile = do
    (BookConfig {..}) <- parseBookConfig yamlFile
    soundsLib <- sounds _mp3s
    let naked = True
    makeBook naked _bookId _description _tingIdBase program soundsLib destinationDir
    uploadBook _bookId destinationDir



destinationDir :: FilePath
destinationDir = "books"


program :: (MonadFix m, Instructions m) => Program m ()
program = [ 
    ("main", TopLevel, mdo 
        end
    )
    ]


sounds :: [FilePath] -> IO SoundLib
sounds dirs = do
    mp3s <- mapM collectMp3s dirs
    return $ concat mp3s
    where 
        collectMp3s dir = do
            items <- listDirectory dir
            withCurrentDirectory dir $ do
                files <- filterM doesFileExist items
                let mp3s = sort . filter (\f -> takeExtension f == ".mp3") $ files
                --fprint ("Collected " % int % " mp3 files from\n" % string % "\n") (length mp3s) dir
                putStr $ printf "Collected %d mp3 files from\n%s\n" (length mp3s) dir
                return $ [(takeBaseName m, dir </> m) | m <- mp3s]
