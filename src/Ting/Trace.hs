{-# LANGUAGE TemplateHaskell #-}
{-
From
http://stackoverflow.com/questions/13379356/finding-the-line-number-of-a-function-in-haskell
https://github.com/ahammar/placeholders/blob/master/src/Development/Placeholders.hs

Usage:
	mdo {play $ map (:[]) $lineNumber; shl a bits}
-}

module Ting.Trace (withLocation, lineNumber) where

import Prelude hiding (and, or, not, divMod, putStr, putStrLn)
import System.IO.Console.IOUtil (putStr, putStrLn)

import Language.Haskell.TH

lineNumber :: Q Exp
lineNumber = justLine =<< location

withLocation' :: String -> IO a -> IO a
withLocation' s f = do { putStrLn s ; f }

withLocation :: Q Exp
withLocation = withFileLine [| withLocation' |]

withFileLine :: Q Exp -> Q Exp
withFileLine f = do
    let loc = fileLine =<< location
    appE f loc

justLine :: Loc -> Q Exp
justLine loc = do
    let floc = formatJustLine loc
    [| $(litE $ stringL floc) |]

formatJustLine :: Loc -> String
formatJustLine loc = let (line, _) = loc_start loc
    in show line

fileLine :: Loc -> Q Exp
fileLine loc = do
    let floc = formatLoc loc
    [| $(litE $ stringL floc) |]

formatLoc :: Loc -> String
formatLoc loc = let file = loc_filename loc
                    (line, col) = loc_start loc
                in concat [file, ":", show line, ":", show col]

