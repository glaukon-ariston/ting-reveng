{-# LANGUAGE RecursiveDo, GeneralizedNewtypeDeriving, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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

module Ting.Linker where

import Prelude hiding (and, or, not, divMod, putStr, putStrLn)
import System.IO.Console.IOUtil (putStr, putStrLn)

import System.IO (Handle, IOMode(WriteMode), withBinaryFile)

import Data.Word (Word8, Word16,Word32)
import Data.Bits ((.&.), (.|.), shiftR, shiftL, complement)
import Data.List (find, intercalate)

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

import qualified Data.Binary.Builder as Br
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
-- import Control.Monad
import Control.Applicative (Applicative)


import Control.Exception (assert)
--import Debug.Trace (trace)

import Ting.Instructions (Program, SoundLib, SnippetType(TopLevel))
import Ting.Assembler (Assembler(..), asm)
import Ting.Test (TestBattery(..))


data BookDescription = BookDescription
    { _name :: String
    , _publisher :: String
    , _author :: String
    , _bookVersion :: String
    , _url :: String
    , _thumb :: String
    , _file :: String
    }

{- https://github.com/entropia/tip-toi-reveng/blob/master/src/GMEWriter.hs -}
newtype SPutM a = SPutM (StateT Int (Writer Br.Builder) a)
    deriving (Functor, Applicative, Monad, MonadFix)
type SPut = SPutM ()

putWord8 :: Word8 -> SPut
putWord8 w = SPutM $ do
    tell (Br.singleton w)
    modify (+1)

putWord16 :: Word16 -> SPut
putWord16 w = SPutM $ do
    tell (Br.putWord16be w)
    modify (+2)

putWord32 :: Word32 -> SPut
putWord32 w = SPutM $ do
    tell (Br.putWord32be w)
    modify (+4)

putBS :: B.ByteString -> SPut
putBS bs = SPutM $ do
    tell (Br.fromLazyByteString bs)
    modify (+ fromIntegral (B.length bs))

putList :: Integral n => (n -> SPut) -> [SPut] -> SPut
putList h xs = do
    h $ fromIntegral (length xs)
    sequence_ xs

seek :: Int -> SPut
seek to = SPutM $ do
    now <- get
    --when (trace (printf "seek to 0x%04x from 0x%04x" to now) (now > to)) $ do
    when (now > to) $ do
        fail $ printf "Cannot seek to 0x%04X, already at 0x%04X" to (assert False now)
    tell $ Br.fromLazyByteString $ B.replicate (fromIntegral (to-now)) 0
    modify (+ (to-now))

here :: SPutM Int
here = SPutM get

-- Puts something, returning the offset to the beginning of it.
getOffset :: SPut -> SPutM Int
getOffset (SPutM what) = SPutM $ do
    a <- get
    what
    return a

(@@) :: SPut -> SPutM Int
(@@) = getOffset

runSPut :: SPut -> B.ByteString
--runSPut (SPutM act) = Br.toLazyByteString $ evalState (execWriterT act) 0
runSPut (SPutM act) = Br.toLazyByteString $ execWriter (evalStateT act 0)

-- ======================================================================

data ResourceType = Empty | Sound | Code deriving (Enum)


indexTableOffset :: Int
indexTableOffset = 0x68

nextResourceOffset :: Int -> Int
nextResourceOffset offset = (offset + 0x1FF) .&. (complement 0x1FF)

{- Tingeltangel-master\src\main\java\tingeltangel\core\IndexTableCalculator.java -}
magicOffsetFix :: [Int]
magicOffsetFix = [
    578, 562, 546, 530, 514, 498, 482, 466
    , 322, 306, 290, 274, 258, 242, 226, 210
    , -446, -462, -478, -494, -510, -526, -542, -558
    , -702, -718, -734, -750, -766, -782, -798, -814 
    ]

decodeMagicOffset :: Int -> Int -> Int
decodeMagicOffset i magic = c <# 8
    where
        -- assert $ (magic & 0xFF) == 0
        a = magic #> 8
        -- create an index out of bits 3,4,5,7,9
        b = ((a #> 3) & 1) 
            .|. (((a #> 4) & 1) <# 1) 
            .|. (((a #> 5) & 1) <# 2) 
            .|. (((a #> 7) & 1) <# 3) 
            .|. (((a #> 9) & 1) <# 4)
        c = a - (i-1)*26 + (magicOffsetFix !! b)

        (#>) = shiftR
        (<#) = shiftL
        (&) = (.&.)

encodeResourceOffset :: Int -> Int -> Int
encodeResourceOffset i offset = case magic of
        Just (v,_) -> v
        Nothing -> error $ printf "Cannot derive magicOffset for the resource %d and file offset 0x%04X" i offset
    where
        magic = find (\(_,o ) -> o == offset) $ map go magicOffsetFix
        b = (offset #> 8) + (i-1)*26
        go adjustment = let 
            v = (b - adjustment) <# 8
            in (v, decodeMagicOffset i v)

        (#>) = shiftR
        (<#) = shiftL

buildIndexTable :: ResourceType -> Int -> Int -> [B.ByteString] -> (Int, [Int])
buildIndexTable resourceType firstIndex firstOffset rs = foldl go (firstOffset,[]) $ zip [firstIndex..] rs
    where
        --firstOffset' = trace (printf "buildIndexTable: firstIndex %d firstOffset %x rs[%d]" firstIndex firstOffset (length rs)) firstOffset
        go (resourceOffset, es) (i,r) = 
            let
                resourceSize = fromIntegral $ B.length r :: Int
                es' = [
                    encodeResourceOffset i resourceOffset
                    ,  resourceSize
                    , fromEnum resourceType]
            in (nextResourceOffset (resourceOffset + resourceSize), es ++ es')
 
oufHeader :: Int -> Int -> [B.ByteString] -> [B.ByteString] -> Int -> SPut
oufHeader bookID tingIdBase program soundLib timestamp =
    mapM_ (putWord32 . fromIntegral) fields
    where
        tingIDsCount = length program + length soundLib
        fields = [
              indexTableOffset -- indexTableOffset (indexTableOffset == 0x68)
            , 0x02 -- unknown1 (unknown1 == 0x2)
            , tingIdBase -- firstTingID
            , (tingIdBase + tingIDsCount - 1) -- lastTingID
            , tingIDsCount -- tingIDsCount
            , bookID -- bookID
            , 11 -- unknown2 (unknown2 >= 1 && unknown2 <= 20)
            , timestamp -- timeCompiled (UNIX timestamp)
            , 0 -- unknown3 (unknown3 == 0)
            , 0xFFFF -- unknown4 (unknown4 == 0xFFFF)
            ]

oufIndexTable :: [B.ByteString] -> [B.ByteString] -> SPut
oufIndexTable program soundLib =
    mapM_ (putWord32 . fromIntegral) $ programEntries ++ soundEntries
    where
        tingIDsCount = length program + length soundLib
        indexTableSize = 3*4*tingIDsCount
        firstResourceOffset = nextResourceOffset $ indexTableOffset + indexTableSize
        (nextOffset, programEntries) = buildIndexTable Code 0 firstResourceOffset program
        (_, soundEntries) = buildIndexTable Sound (length program) nextOffset soundLib

oufResources :: [B.ByteString] -> SPut
oufResources = mapM_ go
    where
        go r = mdo
            offset <- here
            --seek $ nextResourceOffset (trace (printf "current offset 0x%04x" offset) offset)
            seek $ nextResourceOffset offset
            putBS r

{- Produce a Ting's OUF file. -}
linkBS :: Int -> Int -> [B.ByteString] -> [B.ByteString] -> Int -> SPut
linkBS bookID tingIdBase program soundLib timestamp = mdo
    oufHeader bookID tingIdBase program soundLib timestamp
    seek indexTableOffset
    oufIndexTable program soundLib
    oufResources program
    oufResources soundLib

link :: Bool -> Int -> Int -> Program Assembler () -> SoundLib -> FilePath -> IO ()
link naked bookID tingIdBase program soundLib filepath = mdo
    putStr $ printf "Linking %s (bookID %d, tingIdBase %d) ...\n" filepath bookID tingIdBase
    timestamp <- round `fmap` getPOSIXTime
    let procedures = asm naked tingIdBase program soundLib
    putStr $ printf "\tAssembled actions #%d\n" $ length procedures 
    sounds <- mapM loadSound soundLib
    putStr $ printf "\tSounds #%d\n" $ length sounds 
    let oufData = runSPut $ linkBS bookID tingIdBase procedures sounds timestamp
    B.writeFile filepath oufData
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

makeDescription ::BookDescription -> FilePath -> IO ()
makeDescription (BookDescription {..}) descFile = do
    thumbMD5 <- md5sum _thumb
    fileMD5 <- md5sum _file
    let description = 
            [ "Name: " ++ _name
            , "Publisher: " ++ _publisher
            , "Author: " ++ _author
            , "Book Version: " ++ _bookVersion
            , "URL: " ++ _url
            , "ThumbMD5: " ++ thumbMD5
            , "FileMD5: " ++ fileMD5
            , "ScriptMD5: "
            , "Book Area Code: en" ]
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

makeBook :: Bool -> Int -> BookDescription -> Int -> Program Assembler () -> SoundLib -> FilePath -> IO ()
makeBook naked bookId bookDescription tingIdBase program soundLib resourceDir = do
    putStr $ printf "Making book '%d' (resourceDir='%s') ...\n" bookId resourceDir
    let bookDir = resourceDir </> printf "%05d" bookId
    createDirectoryIfMissing True bookDir
    let fileNames = map (\f -> printf "%05d_en.%s" bookId f) ["ouf", "png", "txt", "map"]
    let (oufFile:thumbFile:descFile:mapFile:[]) = map (bookDir </>) fileNames
    link naked bookId tingIdBase program soundLib oufFile
    makeDescription bookDescription { _thumb = thumbFile, _file = oufFile } descFile
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

