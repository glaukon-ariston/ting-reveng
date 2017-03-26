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

import Prelude hiding (and, or, not, return)
import Control.Monad.Fix (MonadFix(..))

import Ting.Instructions
import Ting.Registers
import Ting.Operand (Operand(..))
import Ting.Linker (makeBook, makeBookFromTest, BookDescription(..))
import Ting.Assembler2 (Assembler)
import Ting.Repl (repl, runTestBattery, debugTestBattery)
import Ting.Test (testBattery)


digits :: SoundLib
digits = [
    ("0", "./sounds/0.mp3")
    , ("1", "./sounds/1.mp3")
    , ("2", "./sounds/2.mp3")
    , ("3", "./sounds/3.mp3")
    , ("4", "./sounds/4.mp3")
    , ("5", "./sounds/5.mp3")
    , ("6", "./sounds/6.mp3")
    , ("7", "./sounds/7.mp3")
    , ("8", "./sounds/8.mp3")
    , ("9", "./sounds/9.mp3")
    ]

sounds :: SoundLib
sounds = [
    ("0", "./sounds/0.mp3")
    , ("1", "./sounds/1.mp3")
    , ("2", "./sounds/2.mp3")
    , ("3", "./sounds/3.mp3")
    , ("4", "./sounds/4.mp3")
    , ("5", "./sounds/5.mp3")
    , ("6", "./sounds/6.mp3")
    , ("7", "./sounds/7.mp3")
    , ("8", "./sounds/8.mp3")
    , ("9", "./sounds/9.mp3")
    ]

program :: (MonadFix m, Instructions m) => Program m ()
program = [ 
    ("main", TopLevel, mdo
        clearver
        playoid (S "0")
    )
    , ("reg_playoid", TopLevel, mdo
        set r1 (S "1")
        callid "play"
    )
    , ("r0", TopLevel, mdo 
        set r0 (S "2")
        playoid r0
    )
    , ("inc r1", TopLevel, mdo
        add r1 1
        callid "play"
    )
    , ("dec r1", TopLevel, mdo
        sub r1 1
        callid "play"
    )
    , ("play", Subroutine, mdo 
        playoid r1
        ret
    )
    ]

simple1 :: (MonadFix m, Instructions m) => Program m ()
simple1 = [ 
    ("main", TopLevel, mdo 
        end
    )
    ]

{-
Conclusion: The Zero and Sing flags are affected by the CMP instruction only. Also, they 
survive the END instruction.
-}
testFlags :: (MonadFix m, Instructions m) => Program m ()
testFlags = [ 
    ("testZeroFlag", TopLevel, mdo
        skipThen je (playoid (S "1")) (playoid (S "0"))
        set r0 0xffff
        skipThen je (playoid (S "1")) (playoid (S "0"))
        set r0 0
        skipThen je (playoid (S "1")) (playoid (S "0"))
        
        or r0 0xffff
        skipThen je (playoid (S "1")) (playoid (S "0"))
        and r0 0
        skipThen je (playoid (S "1")) (playoid (S "0"))
        not r0
        skipThen je (playoid (S "1")) (playoid (S "0"))
        not r0
        skipThen je (playoid (S "1")) (playoid (S "0"))
        
        set r0 0xffff
        add r0 1
        skipThen je (playoid (S "1")) (playoid (S "0"))
        add r0 1
        skipThen je (playoid (S "1")) (playoid (S "0"))
        sub r0 1
        skipThen je (playoid (S "1")) (playoid (S "0"))
        sub r0 1
        skipThen je (playoid (S "1")) (playoid (S "0"))

        cmp r0 0
        skipThen je (playoid (S "1")) (playoid (S "0"))
    )
    , ("testSignFlag", TopLevel, mdo
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        set r0 0xffff
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        set r0 0
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        
        or r0 0xffff
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        and r0 0
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        not r0
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        not r0
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        
        set r0 0xffff
        add r0 1
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        add r0 1
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        sub r0 1
        skipThen jb (playoid (S "1")) (playoid (S "0"))
        sub r0 1
        skipThen jb (playoid (S "1")) (playoid (S "0"))

        cmp r0 0
        skipThen jb (playoid (S "1")) (playoid (S "0"))
    )
    ]

testIndirectJmp :: (MonadFix m, Instructions m) => Program m ()
testIndirectJmp = 
    [ ("set_ra_0", TopLevel, mdo
        set ra 0
        playoid (S "9")
        playoid (S "1")
    )
    , ("add_ra_1", TopLevel, mdo
        add ra 1
        playoid (S "9")
        playoid (S "2")
    )
    , ("sub_ra_1", TopLevel, mdo
        sub ra 1
        playoid (S "9")
        playoid (S "3")
    )
    , ("speak", TopLevel, mdo
        playoid (S "9")
        playoid (S "4")
        callid "speakDigit"
    )
    {-
    Non-immediate jumps are not supported. (Sigh)
    , ("speakDigit2", mdo
        playoid (S "9")
        playoid (S "5")
        add ra ra -- x2 <= Assembler2
        set rx ra       -- x2
        add ra ra       -- x4
        add ra rx       -- x6 = x2*(1+2) <= Assembler: sizeof set + sizeof ret
        add ra _table
        jmp ra

        _table <- label
        playoid (S "0")
        end
        playoid (S "1")
        end
        playoid (S "2")
        end
        playoid (S "3")
        end
        playoid (S "4")
        end
        playoid (S "5")
        end
        playoid (S "6")
        end
        playoid (S "7")
        end
        playoid (S "8")
        end
        playoid (S "9")
        end
    )
    , ("jmp_reg", mdo
        playoid (S "9")
        playoid (S "6")
        set r0 _play1
        jmp r0
        playoid (S "0")
        _play1 <- label
        playoid (S "1")
        end
    )
    -}
    ]

saveBookTestIndirectJmp :: IO ()
saveBookTestIndirectJmp = makeBook bookId bookDesc tingId (testIndirectJmp++prelude) preludeSoundLib "books"
    where
        bookId = 8011
        tingId = 15001
        bookDesc = BookDescription 
            { _name = "Test Indirect Jmp"
            , _publisher = "Glaukon Ariston"
            , _author = "Glaukon Ariston"
            , _bookVersion = "1"
            , _url = "http://github.com"
            , _thumb = ""
            , _file = ""
            }

saveBookTestFlags :: IO ()
saveBookTestFlags = makeBook bookId bookDesc tingId testFlags digits "books"
    where
        bookId = 8000
        tingId = 15001
        bookDesc = BookDescription 
            { _name = "Test Zero+Sign Flags"
            , _publisher = "Glaukon Ariston"
            , _author = "Glaukon Ariston"
            , _bookVersion = "1"
            , _url = "http://github.com"
            , _thumb = ""
            , _file = ""
            }

saveBookOne :: IO ()
saveBookOne = makeBook bookId bookDesc tingId program sounds "books"
    where
        bookId = 8000
        tingId = 15001
        bookDesc = BookDescription 
            { _name = "Ting RevEng"
            , _publisher = "Glaukon Ariston"
            , _author = "Glaukon Ariston"
            , _bookVersion = "1"
            , _url = "http://github.com"
            , _thumb = ""
            , _file = ""
            }

evalBook :: Program Assembler () -> IO ()
evalBook p = repl bookId tingId p sounds
    where
        bookId = 8000
        tingId = 15001

debugBook :: IO ()
debugBook = do
    let acs = debugTestBattery testBattery
    putStrLn $ show acs

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
    putStrLn "Ting Emulator"
    --saveBook
    --evalBook simple1
    --evalBook program
    testBook
    --debugBook
    --saveBookTestFlags
    --saveBookTestIndirectJmp

