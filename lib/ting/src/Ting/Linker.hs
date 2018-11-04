{-# LANGUAGE RecordWildCards #-}
{-
Author: Glaukon Ariston
Date: 18.07.2016
Abstract:

Ting Pen -- Assembly Languge as Haskell EDSL

Based on:
Russell O’Connor: ICFP 2014 Post-Mortem
http://r6.ca/blog/20140803T030905Z.html

Lewis: An Asm Monad
http://wall.org/~lewis/2013/10/15/asm-monad.html
https://github.com/quietfanatic/neskell

Monads to Machine Code (Part 1)
http://www.stephendiehl.com/posts/monads_machine_code.html
https://github.com/sdiehl/tinyjit

24 Days of GHC Extensions: Overloaded Strings
https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html
https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings
https://kwangyulseo.com/tag/overloadedstrings/

ghci Debugging
:set -fbreak-on-exception
:trace main
:print _exception
:force _exception
:hist

-}

module Ting.Linker 
    ( makeBook
    , makeBookFromTest
    , uploadBook
    )
where

import Prelude hiding (and, or, not, divMod, putStr, putStrLn)
import System.IO.Console.IOUtil (putStr, putStrLn)

import System.IO (Handle, IOMode(WriteMode), withBinaryFile)

import Data.List (intercalate)

import Text.Printf (printf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, copyFile, doesDirectoryExist)
import System.FilePath ((</>))
--import Data.Hash.MD5 (md5s)
import Crypto.Hash (Digest, MD5, hashlazy)

import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (pack)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)

import Control.Monad (filterM)

import Ting.Instructions (Program, SoundLib, SnippetType(TopLevel))
import Ting.Assembler (Assembler(..), asm)
import Ting.Test (TestBattery(..))
import Ting.OUFWriter (createOUF)
import Ting.BookConfig (BookDescription(..))


link :: Bool -> Int -> Int -> Program Assembler () -> SoundLib -> FilePath -> IO ()
link naked bookID tingIdBase program soundLib filepath = do
    putStr $ printf "Linking %s (bookID %d, tingIdBase %d) ...\n" filepath bookID tingIdBase
    timestamp <- round `fmap` getPOSIXTime
    let procedures = asm naked tingIdBase program soundLib
    putStr $ printf "\tAssembled actions #%d\n" $ length procedures 
    sounds <- mapM loadSound soundLib
    putStr $ printf "\tSounds #%d\n" $ length sounds 
    createOUF filepath bookID tingIdBase procedures sounds timestamp
    where
        loadSound (_,s) = B.readFile s

md5 :: B.ByteString -> Digest MD5
md5 = hashlazy

md5sum :: FilePath -> IO String
md5sum filepath = do
    xs <- B.readFile filepath
    let md5Digest = md5 xs
    return $ show md5Digest

putUtf8Str :: String -> Handle -> IO ()
putUtf8Str s h = B.hPut h . T.encodeUtf8 . T.pack $ s

makeDescription ::BookDescription -> FilePath -> FilePath -> IO ()
makeDescription (BookDescription {..}) oufFile descFile = do
    thumbMD5 <- md5sum _thumb
    fileMD5 <- md5sum oufFile
    let description = 
            [ "Name: " ++ _name
            , "Publisher: " ++ _publisher
            , "Author: " ++ _author
            , "Book Version: " ++ _bookVersion
            , "URL: " ++ _url
            , "ThumbMD5: " ++ thumbMD5
            , "FileMD5: " ++ fileMD5
            , "ScriptMD5: "
            , "Book Area Code: " ++ _bookAreaCode ]
    putStr $ printf "Book description:\n%s\n" . intercalate "\n" . map ("    " ++) $ description
    withBinaryFile descFile WriteMode . putUtf8Str
        $ intercalate "\n" $ description

makeMap :: Int -> Program Assembler () -> SoundLib -> FilePath -> IO ()
makeMap tingIdBase program soundLib mapFile = do
    putStr $ printf "Building map file %s ...\n" mapFile
    withBinaryFile mapFile WriteMode . putUtf8Str
        $ intercalate "\n" . map (\(i,n) -> show i ++ " " ++ n) $ zip [tingIdBase..] resources
    where
        resources = map (\(n,_,_) -> n) program ++ map snd soundLib

makeThumb :: FilePath -> FilePath -> IO ()
makeThumb imageFile thumbFile = do
    putStr $ printf "Creating thumb image %s ...\n" thumbFile
    copyFile imageFile thumbFile

makeBook :: Bool -> Int -> BookDescription -> Int -> Program Assembler () -> SoundLib -> FilePath -> IO ()
makeBook naked bookId bookDescription tingIdBase program soundLib resourceDir = do
    putStr $ printf "Making book '%d' (resourceDir='%s') ...\n" bookId resourceDir
    let bookDir = resourceDir </> printf "%05d" bookId
    createDirectoryIfMissing True bookDir
    let fileNames = map (\f -> printf "%05d_en.%s" bookId f) ["ouf", "png", "txt", "map"]
    let (oufFile:thumbFile:descFile:mapFile:[]) = map (bookDir </>) fileNames
    link naked bookId tingIdBase program soundLib oufFile
    makeThumb (_thumb bookDescription) thumbFile
    makeDescription bookDescription oufFile descFile
    makeMap tingIdBase program soundLib mapFile

makeBookFromTest :: TestBattery Assembler -> BookDescription -> FilePath -> IO ()
makeBookFromTest testBattery bookDescription resourceDir = do
    printf "About to make a Ting book from TestBattery. resourceDir = %s\n" resourceDir
    case testBattery of
        TestBattery {..} -> do
            let program = map (\(n,p,_) -> (n,TopLevel,p)) _tests ++ _library
            makeBook naked _bookId bookDescription _tingIdBase program _soundLib resourceDir
    where
        naked = False

uploadBook :: Int -> FilePath -> IO ()
uploadBook bookId resourceDir = do
    let drives = ['J' .. 'N'] ++ ['D' .. 'I']
    let folders = map (\drive -> printf "%c:/$ting" drive) drives
    mounts <- filterM doesDirectoryExist folders
    if null mounts then
        putStrLn "## No mounted TING folder found. No files have been copied!"
    else do
        let destDir = head mounts
        putStr $ printf "Copying files to %s ...\n" destDir
        let bookDir = resourceDir </> printf "%05d" bookId
        let fileNames = map (\f -> printf "%05d_en.%s" bookId f) ["ouf", "png", "txt", "map"]
        mapM_ (\f -> do
            putStrLn f
            copyFile (bookDir </> f) (destDir </> f)) fileNames

