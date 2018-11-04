{-# LANGUAGE RecursiveDo, GeneralizedNewtypeDeriving #-}
{-
Author: Glaukon Ariston
Date: 18.07.2016
Abstract:
-}

module Ting.OUFWriter
    ( createOUF
    )
where

import Data.List (find)
import Data.Word (Word8, Word16,Word32)
import Data.Bits ((.&.), (.|.), shiftR, shiftL, complement)
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as Br

import Text.Printf (printf)
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
-- import Control.Monad
import Control.Applicative (Applicative)
import Control.Exception (assert)
--import Debug.Trace (trace)


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
linkOUF :: Int -> Int -> [B.ByteString] -> [B.ByteString] -> Int -> SPut
linkOUF bookID tingIdBase program soundLib timestamp = mdo
    oufHeader bookID tingIdBase program soundLib timestamp
    seek indexTableOffset
    oufIndexTable program soundLib
    oufResources program
    oufResources soundLib

createOUF :: FilePath -> Int -> Int -> [B.ByteString] -> [B.ByteString] -> Int -> IO ()
createOUF filePath bookID tingIdBase procedures sounds timestamp = do
    let oufData = runSPut $ linkOUF bookID tingIdBase procedures sounds timestamp
    B.writeFile filePath oufData
