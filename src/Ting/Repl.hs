--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 29.07.2016
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

-}

module Ting.Repl where

import Prelude hiding (putStr, putStrLn)
import IOUtil (putStr, putStrLn)

import Data.List (find, groupBy, sortBy)
import Data.Function (on)
import Data.Word (Word16)

import Control.Monad.State (StateT, runStateT, runState, gets, modify, liftIO, foldM)
import Control.Exception (assert)
import Text.Read (readMaybe)
--import Safe (readMay)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

import Ting.Emulator (Context(..), Emulator, ActionResponse)
import qualified Ting.Emulator as E 
    ( initialState
    , evalProc
    , disasm
    , symbol
    , setBreakpoint
    , showBreakpoints
    , setWatch
    , showWatches
    , stepIn
    , stepOver
    , continueExecution
    , debug
    , here
    , procedures
    , sounds
    , showCallStack
    , readRegister
    , writeRegister
    , clearOutput)
import Ting.Instructions (Program, SnippetType(TopLevel), SoundLib)
import Ting.Assembler2 (Assembler, AsmResult, asm2)
import Ting.Test (Test, TestBattery(..))
import Ting.Common (success, failure)


data ReplState = ReplState
    { _emulator :: Context
    , _lastAction :: String
    }

-- https://wiki.haskell.org/Simple_StateT_use
type Repl a = StateT ReplState IO a

-- ========================================================= Repl

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

type Action = Either String (Emulator ActionResponse)
type Command = [String] -> Repl Action
type Params = String
type Help = String
type Token = String
data CmdCategory = ProgramInfo | Tools | Registers | Execution | Debugging | Breakpoints | Miscellaneous 
    deriving (Ord, Eq, Show)
type CmdEntry = (CmdCategory, Token, Params, Help, Command)

commands :: [CmdEntry]
commands = 
    [ (Execution, "eval", "tingId | proc name", "Execute the procedure", eval)
    , (Execution, "here", "<None>", "Show the current context", here)
    , (Execution, "callStack", "<None>", "Show the call stack", showCallStack)
    , (Registers, "read", "r", "Show the current value of the register r", readRegister)
    , (Registers, "write", "r val", "Set the value of the register r", writeRegister)
    , (Tools, "radix", "i", "Convert the supplied number to different number bases using the Word16 container", radix)
    , (Breakpoints, "break", "(tingId|proc name):address +|-", "Break execution at the specified location", breakpoint)
    , (Breakpoints, "watch", "register r|w|rw +|-", "Break execution when access on register is detected", watch)
    , (Breakpoints, "show", "break|watch", "Show the currently set breakpoints/watches", showThing)
    , (Debugging, "debug", "tingId | proc name", "Prepare the procedure for execution", debug)
    , (Debugging, "step", "[n]", "'step into', execute the next 'n' instructions (step into in case of callid)", step)
    , (Debugging, "next", "[n]", "'step over', execute the next 'n' instructions (step over in case of callid)", next)
    , (Debugging, "continue", "<None>", "Continue execution until the program ends or it hits a breakpoint", continue)
    , (ProgramInfo, "dasm", "tingId | proc name", "Disassemble the specified procedure", disasm)
    , (ProgramInfo, "symbol", "tingId | proc name | sound name", "Show symbol info", symbol)
    , (ProgramInfo, "procedures", "<None>", "Show the names of all procedures with their associated tingId", procedures)
    , (ProgramInfo, "sounds", "<None>", "Show the names of all sounds with their associated tingId", sounds)
    , (Miscellaneous, "help", "[cmd]", "Show help", help)
    , (Miscellaneous, "quit", "<None>", "Exit the repl", quit)
    , (Miscellaneous, "<ENTER>", "<None>", "Repeat the last command", undefined)
    ]

debug :: [String] -> Repl Action
debug [] = return $ Left (printf "Expecting tingId or procedure name as a parameter, none given.")
debug (p:_) = return $ Right (E.debug p)

disasm :: [String] -> Repl Action
disasm [] = failure $ printf "Expecting tingId or procedure name as a parameter, none given."
disasm (p:_) = success (E.disasm p)

symbol :: [String] -> Repl Action
symbol [] = failure $ printf "Expecting tingId or procedure name or sound name as a parameter, none given."
symbol (p:_) = success (E.symbol p)

procedures :: [String] -> Repl Action
procedures _ = success E.procedures

sounds :: [String] -> Repl Action
sounds _ = success E.sounds

readRegister :: [String] -> Repl Action
readRegister [] = failure $ printf "Expecting register name, none given."
readRegister (reg:_) = success $ E.readRegister reg

writeRegister :: [String] -> Repl Action
writeRegister [] = failure $ printf "Expecting register name and the new value, none given."
writeRegister (_:[]) = failure $ printf "Expecting register name and the new value."
writeRegister (reg:val:_) = success $ E.writeRegister reg val

radix :: [String] -> Repl Action
radix [] = failure $ printf "Expecting integer number, none given."
radix (r:_) = return $ case (readMaybe r :: Maybe Word16) of
    Just i -> Left $ printf "%d x%04x o%06o b%016b" i i i i
    Nothing -> Left $ printf "Expecting integer number, got '%s'" r

breakpoint :: [String] -> Repl Action
breakpoint [] = failure $ printf "Expecting tingId or procedure name plus address as a parameter, none given."
breakpoint (location:[]) = failure $ printf "Expecting 'location operation', got '%s'" location
breakpoint (location:op:_) = success $ E.setBreakpoint location op

watch :: [String] -> Repl Action
watch [] = failure $ printf "Expecting 'register access operation', none given."
watch (reg:[]) = failure $ printf "Expecting 'register access operation', got '%s'" reg
watch (reg:access:[]) = failure $ printf "Expecting 'register access operation', got '%s %s'" reg access
watch (reg:access:op:_) = success $ E.setWatch reg access op

showThing :: [String] -> Repl Action
showThing [] = failure $ printf "Expecting 'break|watch', none given."
showThing ("break":_) = success $ E.showBreakpoints
showThing ("watch":_) = success $ E.showWatches
showThing _ = failure $ printf "Expecting 'break|watch'."

here :: [String] -> Repl Action
here _ = success $ E.here

showCallStack :: [String] -> Repl Action
showCallStack _ = success $ E.showCallStack

continue :: [String] -> Repl Action
continue _ = success $ E.continueExecution

step :: [String] -> Repl Action
step _ = success $ E.stepIn

next :: [String] -> Repl Action
next _ = success $ E.stepOver

help :: [String] -> Repl Action
help _ = return $ Left helpStr
    where
        helpStr :: String
        helpStr = unlines 
            . concatMap showGroup 
            . sortBy (compare `on` (category . head))
            . groupBy ((==) `on` category) $ commands
        showGroup :: [CmdEntry] -> [String]
        showGroup xs@((cat,_,_,_,_):_) = (show cat) : (map showItem xs) ++ [""]
            where
                wsSep = 2
                cmdIndent = wsSep + (maximum . map (\(_,c,_,_,_) -> length c) $ xs)
                paramsIndent = wsSep + (maximum . map (\(_,_,p,_,_) -> length p) $ xs)
                showItem :: CmdEntry -> String
                showItem (_,c,p,h,_) =
                    take wsSep (repeat ' ')
                    ++ take cmdIndent (c ++ repeat ' ')
                    ++ take paramsIndent (p ++ repeat ' ')
                    ++ h
        category :: CmdEntry -> CmdCategory
        category (c,_,_,_,_) = c

quit :: [String] -> Repl Action
quit _ = failure $ "exit"

eval :: [String] -> Repl Action
eval [] = failure $ printf "Expecting tingId or procedure name as a parameter, none given."
eval (p:_) = success $ E.evalProc p

perform :: Action -> Repl ()
perform (Right action) = do
    e <- gets _emulator
    let (r, e') = runState action e
    modify(\s -> s { _emulator = e' })
    case r of
        Right xs -> liftIO . putStrLn . unlines $ xs
        Left msg -> liftIO . putStrLn $ msg
    repl'
perform (Left "exit") = do
    liftIO $ putStrLn "Exiting now..."
perform (Left msg) = do
    liftIO $ putStrLn msg
    repl'

getAction :: String -> Repl Action
getAction [] = do
    lastAction <- gets _lastAction
    case lastAction of
        [] -> return $ Left "Nothing to do. Enter a new command."
        line -> getAction line
getAction line = case match of
    Nothing -> return . Left $ printf "Unknown command '%s'" cmd
    Just (_,_,_,_,action) -> do
        modify (\s -> s { _lastAction = line })
        action params
    where
        (cmd:params) = words line
        match = find (\(_,c,_,_,_) -> c == cmd) commands

repl' :: Repl ()
repl' = do
    line <- liftIO $ prompt "repl> "
    action <- getAction line
    perform action

repl :: Int -> Int -> Program Assembler () -> SoundLib -> IO ()
repl bookId tingBaseId program soundLib = do
    printf "Running REPL for book %d (tingBaseId %d). Type `help` for help.\n" bookId tingBaseId
    _ <- runStateT repl' (ReplState emulator [])
    return ()
    where
        emulator = E.initialState bookId tingBaseId program soundLib


-- ============================================================== Testing

liftEmulator :: Emulator a -> Repl a
liftEmulator action = do
    e <- gets _emulator
    let (r, e') = runState action e
    modify(\s -> s { _emulator = e' })
    return r

runTest :: Bool -> Test Assembler -> Repl Bool
runTest prev (name, _, check) = if prev
    then do
        liftIO $ putStrLn name
        liftEmulator E.clearOutput
        liftEmulator (E.disasm name) >>= \case
            Left msg -> do
                liftIO . putStrLn $ msg
            Right xs -> do
                liftIO . putStrLn . unlines $ xs
        liftEmulator (E.evalProc name) >>= \case
            Left msg -> do
                liftIO . putStrLn $ msg
                return False
            Right xs -> do
                liftIO . putStrLn . unlines $ xs
                r <- liftEmulator check
                liftIO $ printf "%s -> %s\n" name (show r)
                return r

    else 
        return False

runAllTests :: TestBattery Assembler -> Repl Bool
runAllTests tb = do
    liftIO . putStrLn $ "About to run all tests now..."
    liftIO . putStrLn $ "Entering the REPL, type quit to commence the testing."
    repl'
    ok <- foldM runTest True (_tests tb)
    liftIO . putStrLn . unlines $ 
        [ if ok then "All tests passed OK." else "The last test has failed."
        , "Entering the REPL - you can inspect the registers and the call stack now. Type `help` for the list of commands."]
    repl'
    return ok

runTestBattery :: TestBattery Assembler -> IO ()
runTestBattery tb = case tb of
    TestBattery {..} -> do
        let program = map (\(n,p,_) -> (n,TopLevel,p)) _tests ++ _library
        let emulator = E.initialState _bookId _tingIdBase program _soundLib
        _ <- runStateT (runAllTests tb) (ReplState emulator [])
        return ()
        
debugTestBattery :: TestBattery Assembler -> [AsmResult]
debugTestBattery tb = case tb of
    TestBattery {..} -> do
        let program = map (\(n,p,_) -> (n,TopLevel,p)) _tests
        asm2 _tingIdBase program _soundLib
        
