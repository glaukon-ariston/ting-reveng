{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 15.09.2016
Abstract:
-}

module Ting.Common where

i# :: (Integral a, Num b) => a -> b
i# = fromIntegral

{- Convert from enum to word -}
e2i :: (Enum a, Num b) => a -> b
e2i = i# . fromEnum

type Result a = Either String a

success :: (Monad m) => a -> m (Result a)
success a = return . Right $ a

failure :: (Monad m) => String -> m (Result a)
failure x = return . Left $ x

