{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 27.07.2016
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

Error Handling
https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling
https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/12-State-Monad
-}

module Ting.Emulator where

import Prelude hiding (and, or, not, return)
import qualified Prelude as P (not)
import Data.Word (Word16)
import qualified Data.Set as S
import Data.List (findIndices, intercalate)
import Data.Maybe (fromJust)
import Data.Bits ((.&.), (.|.), complement, clearBit, setBit, testBit)
import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.Char (toUpper)

import Control.Monad (return, liftM, when)
import Control.Monad.State (State, gets, get, modify)
import Text.Printf (printf)
import Control.Exception (assert)

--import Debug.Trace (trace)

import Ting.Assembler2 (asm, disasmCode, disasmWindow, Assembler, Instruction(..))
import Ting.Operand (Operand(..))
import Ting.Registers (Register, ScratchRegister, maxRegisterIndex, register, scratchRegister, toRegIndex, rlastoid)
import Ting.Instructions (Program, SoundLib)
import qualified Ting.Instructions as I (resolveProcedure, resolveSound)
import Ting.Common (i#, e2i, Result, success, failure)


-- ================================================================= Emulator
type ActionResponse = Result [String]

data Flags = Sign | ZeroF | Stopped | StepOver | Broken deriving (Show, Enum)
data BreakpointFlag = Temporary | Permanent deriving (Show, Enum)
data RegAccess = RegRead | RegWrite deriving (Enum, Show)
data ThingOps = AddThing | RemoveThing deriving (Enum, Show)
type Location = (Word16,Word16)

data Breakpoint = Breakpoint
    { _bpFlags :: Int
    , _bpHitCount :: Int
    }

data Watch = Watch
    { _watchFlags :: Int
    , _watchHitCount :: Int
    }

data Context = Context
    { _bookId :: Word16
    , _tingIdBase :: Word16
    , _assemblage :: [(String, [Instruction])]
    , _program :: Program Assembler ()
    , _soundLib :: SoundLib
    , _currentTingId :: Word16
    , _currentName :: String
    , _currentCode :: [Instruction]
    , _programCounter :: Word16
    , _flags :: Word16
    , _stack :: [(Word16, Word16)]
    , _regs :: V.Vector Word16
    , _soundStream :: [String]
    , _touchedRegs :: S.Set Word16
    , _breakpoints :: Map.Map Location Breakpoint
    , _watchedRegs :: Map.Map Word16 Watch
    , _execResult :: [String]
    }

showFlags :: Word16 -> String
showFlags flags = show . filter (\f -> testBit flags (e2i f)) $ [Sign .. ]

showTouchedRegs :: S.Set Word16 -> V.Vector Word16 -> String
showTouchedRegs touchedRegs regs = unwords . S.foldr showReg [] $ touchedRegs
    where
        showReg :: Word16 -> [String] -> [String]
        showReg r xs = let 
            val = regs V.! (i# r) 
            in (printf "%s=%04x" (show (R r)) val) : xs

instance Show Context where
    show (Context {..}) = 
        unlines 
            [ printf "TID=%05d PC=%04x Flags=%s" _currentTingId _programCounter (showFlags _flags)
            , printf "REGS %s" (showTouchedRegs _touchedRegs _regs)
            , printf "%s" $ unlines $ disasmWindow _currentName _currentCode (i# _currentTingId) (i# _programCounter) 10
            , printf "tingIdBase: %d" _tingIdBase
            , printf "program: %s" (show $ map fst _assemblage)
            , printf "soundLib %s" (show $ map fst _soundLib)
            , printf "soundStream %s" (show _soundStream)
            ]

type Emulator a = State Context a

{- Maps the procedure name to a unique tingId. -}
resolveProcedure :: String -> Emulator (Result (String, Word16))
resolveProcedure proc = do
    tingIdBase <- gets _tingIdBase
    program <- gets _program
    case I.resolveProcedure proc tingIdBase program of
        x@(Right _) -> return x
        Left msg -> case (readMaybe proc :: Maybe Int) of
            Nothing -> failure msg
            Just tingId -> 
                if tingId >= (i# tingIdBase) && tingId < (i# tingIdBase) + length program
                    then do
                        let (name,_,_) = program !! (tingId - i# tingIdBase)
                        success $ (name , i# tingId)
                    else failure $ printf "Number %d out of range, valid range is [%d,%d]" tingId tingIdBase (i# tingIdBase + length program - 1)

{- Maps the sound name to a unique tingId. -}
resolveSound :: String -> Emulator (Result (String, Word16))
resolveSound sound = do
    tingIdBase <- gets _tingIdBase
    program <- gets _program
    soundLib <- gets _soundLib
    case I.resolveSound sound tingIdBase program soundLib of
        x@(Right _) -> return x
        Left msg -> case (readMaybe sound :: Maybe Int) of
            Nothing -> failure msg
            Just tingId -> 
                if tingId >= (i# tingIdBase) + length program && tingId < (i# tingIdBase) + length program  + length soundLib
                    then do
                        let (name,_) = soundLib !! (tingId - length program - i# tingIdBase)
                        success $ (name , i# tingId)
                    else failure $ printf "Number %d out of range, valid range is [%d,%d]" tingId (i# tingIdBase + length program) (i# tingIdBase + length program + length soundLib - 1)

modifyFlag :: Flags -> Bool -> Emulator ()
modifyFlag f v = do
    modify (\s -> s { _flags = op (_flags s) bit })
    where
        op = if v 
            then setBit
            else clearBit
        bit = e2i f

testFlag :: Flags -> Emulator Bool
testFlag f = do
    flags <- gets _flags
    return $ testBit flags bit
    where
        bit = e2i f

runtimeError :: String -> Emulator a
runtimeError = assert False undefined

setPC :: Word16 -> Emulator (Result ())
setPC pc = do
    modify (\s -> s { _programCounter = pc })
    success ()

incPC :: Emulator (Result ())
incPC = do
    modify (\s -> s { _programCounter = _programCounter s + 1 })
    success ()

push :: Emulator ()
push = get >>= \case
    Context {_currentTingId, _programCounter} -> do
        modify (\s -> s { _stack = (_currentTingId, _programCounter) : _stack s })
        testFlag StepOver >>= \case
            False -> return ()
            True -> do
                _ <- insertBreakpoint (_currentTingId, _programCounter) Temporary
                modifyFlag StepOver False

pop :: Emulator (Result ())
pop = do
    gets _stack >>= \case
        [] -> do
            xs <- currentContext
            failure $ unlines ("Stack underflow.":xs)
        ((tingId, programCounter) : xs) -> do
            get >>= \case
                Context {_tingIdBase, _assemblage} -> do
                    let i = tingId - _tingIdBase
                    let (name,code) = _assemblage !! i# i
                    modify (\s -> s { 
                        _stack = xs
                        , _currentTingId = tingId
                        , _currentName = name
                        , _currentCode = code
                        , _programCounter = programCounter 
                        })
                    success ()

zeroAllRegs :: Emulator ()
zeroAllRegs = do
    modify (\s -> s 
        { _regs = V.replicate (maxRegisterIndex+1) 0
        })

modifyWatch :: Word16 -> [RegAccess] -> ThingOps -> Emulator Int
modifyWatch r access op = do
    modify (\s -> s { _watchedRegs = Map.insertWith f r w (_watchedRegs s) })
    ws <- gets _watchedRegs
    return $ Map.findIndex r ws
    where
        f _ (Watch {..}) = Watch { _watchFlags = modifyFlags _watchFlags, _watchHitCount }
        w = Watch { _watchFlags = modifyFlags 0, _watchHitCount = 0}
        modifyFlags f0 = foldr (\x z -> flagOp z (e2i x)) f0 access
        flagOp = case op of
            AddThing -> setBit
            RemoveThing -> clearBit

hitWatch :: Word16 -> Emulator ()
hitWatch r = do
    modifyFlag Broken True
    modify (\s@(Context {_watchedRegs, _execResult}) -> 
        let 
            i = Map.findIndex r _watchedRegs
        in s 
        { _watchedRegs = Map.update f r _watchedRegs
        , _execResult = (printf "Hit watch #%d" i):_execResult
        })
    where
        f w = Just $ w { _watchHitCount = (_watchHitCount w) + 1 }

checkWatches :: Word16 -> RegAccess -> Emulator ()
checkWatches r access = get >>= \case
    Context {_watchedRegs} -> do
        case Map.lookup r _watchedRegs of
            Just (Watch {_watchFlags}) -> when (testBit _watchFlags (e2i access)) $ hitWatch r
            _ -> return ()

readReg :: Word16 -> Emulator Word16
readReg r = do
    checkWatches r RegRead
    regs <- gets _regs  
    return $ regs V.! (i# r)

writeReg :: Word16 -> Word16 -> Emulator (Result ())
writeReg r val = get >>= \case
    Context {_regs, _touchedRegs} -> 
        if r >= 0 && i# r < V.length _regs
            then do
                modify (\s -> 
                    s { _regs = _regs V.// [(i# r, val)]
                    , _touchedRegs = S.insert r _touchedRegs}
                    )
                checkWatches r RegWrite
                success ()
            else do
                failure $ printf "Invalid register r%d" r

regIndex :: Operand ->  Emulator Word16
regIndex (R r) = return r
regIndex op = error $ printf "regIndex: Invalid operand %s" (show op)

valueOf :: Operand ->  Emulator Word16
valueOf (R r) = readReg r
valueOf (I i) = return i
valueOf (A i) = return i
valueOf (P proc) = do
    tingIdBase <- gets _tingIdBase
    program <- gets _assemblage
    w <- case findIndices (\(s, _) -> s == proc) program of
        [] -> error $ "Error: The procedure '" ++ proc ++ "' is undefined."
        [i] -> return $ tingIdBase + i# i
        xs -> error $ printf "Error: Multiple definitions found for the procedure '%s' at the following locations: %s"
            proc (show xs)
    return w
valueOf (S sound) = do
    tingIdBase <- gets _tingIdBase
    program <- gets _assemblage
    soundLib <- gets _soundLib
    w <- case findIndices (\(s, _) -> s == sound) soundLib of
        [] -> error $ "Error: The sound '" ++ sound ++ "' is undefined."
        [i] -> return $ tingIdBase + i# (length program + i)
        xs -> error $ printf "Error: Multiple definitions found for the sound '%s' at the following locations: %s"
            sound (show xs)
    return w

addSaturate :: Word16 -> Word16 -> Word16
addSaturate a b = let s = i# a + i# b :: Int
    in if s > 0xffff 
        then 0xffff 
        else i# s

subSaturate :: Word16 -> Word16 -> Word16
subSaturate a b = let s = i# a - i# b :: Int
    in if s < 0
        then 0 
        else i# s

binaryOp :: (Word16 -> Word16 -> Word16) -> Operand -> Operand -> Emulator (Result ())
binaryOp op op1 op2 = do
    r <- regIndex op1
    val1 <- valueOf op1
    val2 <- valueOf op2
    _ <- writeReg r (val1 `op` val2)
    incPC

unaryOp :: (Word16 -> Word16) -> Operand -> Emulator (Result ())
unaryOp op op1 = do
    r <- regIndex op1
    val1 <- valueOf op1
    _ <- writeReg r (op val1)
    incPC

branchOp :: Bool -> Operand -> Emulator (Result ())
{- 
Non-immediate jumps are not supported. (Sigh)

branchOp c (R r) = if c
    then do
        val1 <- readReg r
        setPC val1
    else incPC
-}
branchOp c (I i) = if c
    then setPC i
    else incPC
branchOp c (A i) = if c
    then setPC i
    else incPC
branchOp _ _ = assert False undefined

eval :: Instruction -> Emulator (Result ())
eval (END) = do
    modifyFlag Stopped True
    incPC
eval (CLEARVER) = do
    zeroAllRegs
    incPC
eval (SET op1 op2) = do
    r <- regIndex op1
    val2 <- valueOf op2
    _ <- writeReg r val2
    incPC
eval (CMP op1 op2) = do
    val1 <- valueOf op1
    val2 <- valueOf op2
    modifyFlag ZeroF (val1 == val2)
    modifyFlag Sign (val1 < val2)
    incPC
eval (AND op1 op2) = do
    binaryOp (.&.) op1 op2
eval (OR op1 op2) = do
    binaryOp (.|.) op1 op2
eval (NOT op1) = do
    unaryOp complement op1

eval (JMP op1) = do
    branchOp True op1
eval (JE op1) = do
    c <- testFlag ZeroF
    branchOp c op1
eval (JNE op1) = do
    c <- liftM P.not (testFlag ZeroF)
    branchOp c op1
eval (JG op1) = do
    cs <- sequence . map testFlag $ [Sign, ZeroF]
    branchOp (all P.not cs) op1
eval (JGE op1) = do
    c <- testFlag Sign
    branchOp (P.not c) op1
eval (JB op1) = do
    c <- testFlag Sign
    branchOp c op1
eval (JBE op1) = do
    cs <- sequence . map testFlag $ [Sign, ZeroF]
    branchOp (any id cs) op1

eval (ADDS op1 op2) = do
    binaryOp addSaturate op1 op2
eval (SUBS op1 op2) = do
    binaryOp subSaturate op1 op2
eval (RETURN) = do
    _ <- incPC
    pop
eval (CALLID op1) = do
    _ <- incPC
    push
    tingId <- valueOf op1
    debugTingId False $ i# tingId
eval (PLAYOID op1) = do
    _ <- incPC
    tingId <- valueOf op1
    evalSound $ i# tingId
eval (PAUSE _) = assert False undefined

-- ============================================ Execution

hitBreakpoint :: Location -> Emulator ()
hitBreakpoint location = do
    modifyFlag Broken True
    modify (\s@(Context {_breakpoints, _execResult}) ->
        let i = Map.findIndex location _breakpoints
        in s
        { _breakpoints = Map.update f location _breakpoints
        , _execResult = (printf "Hit breakpint #%d" i):_execResult
        })
    where
        f bp = Just $ bp { _bpHitCount = (_bpHitCount bp) + 1 }

clearTemporaryBreakpoint :: Location -> Emulator ()
clearTemporaryBreakpoint location = do
    modify (\s -> s { _breakpoints = Map.update f location (_breakpoints s) })
    where
        f bp = Just $ bp { _bpFlags = clearBit (_bpFlags bp) (e2i Temporary) }

deleteBreakpoint :: Location -> Emulator ()
deleteBreakpoint location = do
    modify (\s -> s { _breakpoints = Map.delete location (_breakpoints s) })

insertBreakpoint :: Location -> BreakpointFlag -> Emulator Int
insertBreakpoint location kind = do
    modify (\s -> s { _breakpoints = Map.insertWith f location bp (_breakpoints s) })
    bps <- gets _breakpoints
    return $ Map.findIndex location bps
    where
        f _ (Breakpoint {..}) = Breakpoint { _bpFlags = setBit _bpFlags (e2i kind), _bpHitCount }
        bp = Breakpoint { _bpFlags = setBit 0 (e2i kind), _bpHitCount = 0}

checkCollision :: Emulator ()
checkCollision = get >>= \case
    Context {_breakpoints, _currentTingId, _programCounter} -> do
        let location = (_currentTingId, _programCounter)
        case Map.lookup location _breakpoints of
            Nothing -> return ()
            Just (Breakpoint {_bpFlags}) -> do
                --let location' = trace (printf "hit breakpoint (%d:%04x)" (fst location) (snd location)) location
                hitBreakpoint location
                when (testBit _bpFlags (e2i Temporary)) $ do
                    if clearBit _bpFlags (e2i Temporary) == 0
                        then deleteBreakpoint location
                        else clearTemporaryBreakpoint location

breakpoint :: String -> Int -> ThingOps -> Emulator (Result (Int, String, Word16))
breakpoint proc address op = do
    r <- resolveProcedure proc
    case r of
        Left msg -> failure msg
        Right (name, tingId) -> do
            code <- getCode tingId
            if address >= length code
                then failure $ printf "Address %d out of bounds for %d[%d] %s" address tingId (length code) name
                else case op of
                    AddThing -> do
                        i <- insertBreakpoint (tingId, i# address) Permanent
                        success (i, name, tingId)
                    RemoveThing -> do
                        deleteBreakpoint (tingId, i# address)
                        success (-1, name, tingId)

peek :: Emulator (Maybe (Location, Instruction))
peek = get >>= \case
    Context {_currentCode, _currentTingId, _programCounter} -> do
        return $ if i# _programCounter < length _currentCode 
            then Just $ ((_currentTingId, _programCounter), _currentCode !! i# _programCounter)
            else Nothing

continue :: Emulator (Result ())
continue = step >>= \case
    Left msg -> failure msg
    Right True -> success ()
    Right False -> testFlag StepOver >>= \case
        True -> success ()
        False -> continue

next :: Emulator (Result ())
next = do
    modifyFlag StepOver True
    r <- continue
    modifyFlag StepOver False
    return r

step :: Emulator (Result Bool)
step = testFlag Stopped >>= \case
    True -> do
        modify (\s -> s { _execResult = "The execution has stopped." : _execResult s })
        success True
    False -> peek >>= \case
        Nothing -> do
            modifyFlag Stopped True
            failure $ "Out of range execution. Maybe you forgot to end the procedure with the RETURN or END."
        Just (_, instruction) -> do
            eval instruction >>= \case
                Left msg -> do
                    modifyFlag Stopped True
                    failure msg
                Right _ -> do
                    checkCollision
                    broken <- testFlag Broken
                    success broken


-- ============================================ Miscellanenous

getCode :: Word16 -> Emulator [Instruction]
getCode tingId = do
    program <- gets _assemblage
    tingIdBase <- gets _tingIdBase
    let i = tingId - tingIdBase
    let (_,code) = program !! i# i
    return code

debugTingId :: Bool -> Int -> Emulator (Result ())
debugTingId touchLastOid tingId = get >>= \case
    Context {_tingIdBase, _assemblage, _regs, _touchedRegs} -> do
        let i = tingId - i# _tingIdBase
        if i >= 0 && i < length _assemblage 
            then do
                let (name,code) = _assemblage !! i
                modify (\s -> s { 
                    _currentName = name
                    , _currentCode = code
                    , _currentTingId = i# tingId
                    , _programCounter = 0
                    })
                when touchLastOid $
                    modify (\s -> s { 
                        _regs = _regs V.// [(i# . toRegIndex $ rlastoid, i# tingId)]
                        , _touchedRegs = S.insert (i# . toRegIndex $ rlastoid) _touchedRegs
                        })
                success ()
            else do
                modifyFlag Stopped True
                failure $ printf "Invalid tingId %d" tingId

evalSound :: Int -> Emulator (Result ())
evalSound tingId = do
    tingIdBase <- gets _tingIdBase
    program <- gets _assemblage
    soundLib <- gets _soundLib
    let i = tingId - i# tingIdBase - length program
    if i >= 0 && i < length soundLib 
        then do
            let (sound,_) = soundLib !! i
            modify (\s -> s { _soundStream = _soundStream s ++ [sound] })
            success ()
        else 
            failure $ printf "Invalid tingId %d" tingId

disasmTingId :: Int -> Emulator (Result [String])
disasmTingId tingId = do
    tingIdBase <- gets _tingIdBase
    let i = tingId - i# tingIdBase
    program <- gets _assemblage
    let (name,code) = program !! i
    success $ disasmCode name code tingId 0

initialState :: Int -> Int -> Program Assembler () -> SoundLib -> Context
initialState bookId tingIdBase program soundLib = Context
    { _bookId = i# bookId
    , _tingIdBase = i# tingIdBase
    , _assemblage = zipWith (\(name,_,_) code -> (name,code)) program assemblage
    , _program = program
    , _soundLib = soundLib
    , _currentTingId = 0
    , _currentName = ""
    , _currentCode = []
    , _programCounter = 0
    , _flags = 0
    , _stack = []
    , _regs = V.replicate (maxRegisterIndex+1) 0
    , _touchedRegs = S.empty
    , _soundStream = []
    , _breakpoints = Map.empty
    , _watchedRegs = Map.empty
    , _execResult = []
    }
    where
        assemblage = asm naked tingIdBase program soundLib
        naked = False

callStack :: Emulator [String]
callStack = get >>= \case 
        Context {_stack, _assemblage, _tingIdBase} -> return $ map (go _assemblage _tingIdBase) $ zip [0..] _stack
    where
        go :: [(String, [Instruction])] -> Word16 -> (Int, (Word16, Word16)) -> String
        go program tingIdBase (i, (tingId,pc)) = printf "%d. %5d:%04xh %s" i tingId pc (fst $ program !! i# (tingId - tingIdBase))

disasmCurrent :: Emulator [String]
disasmCurrent = do
    get >>= \case
        Context {..} -> do
            let a = [printf "TID=%05d %s PC=%04x Flags=%s REGS %s OUTPUT %s" _currentTingId _currentName _programCounter (showFlags _flags) (showTouchedRegs _touchedRegs _regs) (show _soundStream)]
            let b = disasmWindow _currentName _currentCode (i# _currentTingId) (i# _programCounter) 5
            return $ a ++ b

currentContext :: Emulator [String]
currentContext = do
    a <- disasmCurrent
    b <- callStack
    return $ a ++ ["Call Stack:"] ++ b

-- ============================================ Interface functions (Actions)

showCallStack :: Emulator ActionResponse
showCallStack = do
    st <- callStack
    success st

here :: Emulator ActionResponse
here = do
    xs <- currentContext
    success xs

(>>>) :: (Num a) => (a -> Emulator (Result b)) -> String -> Emulator (Result b)
action >>> proc = do
    r <- resolveProcedure proc
    case r of
        Left msg -> return . Left $ msg
        Right (_, tingId) -> action (i# tingId)

debug :: String -> Emulator ActionResponse
debug p = do
    modifyFlag Stopped False
    debugTingId True >>> p >>= \case
        Left msg -> failure msg
        Right _ -> here

disasm :: String -> Emulator ActionResponse
disasm p = disasmTingId >>> p

setBreakpoint :: String -> String -> Emulator ActionResponse
setBreakpoint location operation
    | [proc, address'] <- splitOn ":" location 
    , Just address <- readMaybe address' :: Maybe Int
    , Just operation' <- lookup operation [("+", AddThing), ("-", RemoveThing)]
    = do
        Right (i, name, tingId) <- breakpoint proc address operation'
        success $ [printf "%s: Breakpoint %d at %d:%04x %s" (show operation') i tingId address name]
setBreakpoint location operation 
    = failure $ printf "Invalid command '%s %s'. Valid format is 'tingId:address +|-' or 'proc:address +|-'" location operation

showBreakpoints :: Emulator ActionResponse
showBreakpoints = get >>= \case
    Context {_breakpoints, _assemblage, _tingIdBase} -> do
        success $ foldr f [] $ zip [0 ..] (prepare _breakpoints _assemblage _tingIdBase)
    where
        f :: (Int, (Location, String, Breakpoint)) -> [String] -> [String]
        f (i, ((tingId,address), name, Breakpoint {..})) z = (printf "%d. %d:%04x %s %s #%d" i tingId address name (deflag _bpFlags [Temporary ..]) _bpHitCount):z
        deflag :: Int -> [BreakpointFlag] -> String
        deflag w flags = intercalate "|" $ foldr (\flag z -> if testBit w (e2i flag) then (show flag):z else z) [] flags
        prepare breakpoints program tingIdBase = 
            map (\((tingId,address), bp) -> ((tingId,address), name tingId, bp)) $ Map.toList breakpoints
            where
                name tingId = fst $ program !! i# (tingId - tingIdBase)

setWatch :: String -> String -> String -> Emulator ActionResponse
setWatch reg access operation
    | Just (R r) <- resolveRegister reg
    , Just access' <- lookup access [("r", [RegRead]), ("w", [RegWrite]), ("rw", [RegRead, RegWrite])]
    , Just operation' <- lookup operation [("+", AddThing), ("-", RemoveThing)]
    = do
        i <- modifyWatch r access' operation'
        success $ [printf "Watch %d for register %s for %s access: %s." i reg access (show operation')]
setWatch reg access op = failure $ printf "Invalid watch specification '%s %s'. Valid format is 'register r|w|rw +|-'" reg access op

showWatches :: Emulator ActionResponse
showWatches = get >>= \case
    Context {_watchedRegs} -> success $ foldr f [] $ zip [0 ..] (Map.toList _watchedRegs)
    where
        f :: (Int, (Word16, Watch)) -> [String] -> [String]
        f (i, (r, Watch {..})) z = (printf "%d. %s %s #%d" i (show (R r)) (deflag _watchFlags [RegRead ..]) _watchHitCount):z
        deflag :: Int -> [RegAccess] -> String
        deflag w flags = intercalate "|" $ foldr (\flag z -> if testBit w (e2i flag) then (show flag):z else z) [] flags

prepareExecution :: Emulator ()
prepareExecution = do
    modifyFlag Broken False
    modify (\s -> s { _execResult = [] })

executionResult :: Emulator ActionResponse
executionResult = do
    xs <- gets _execResult
    ys <- currentContext
    success $ xs ++ ys

evalProc :: String -> Emulator ActionResponse
evalProc p = do
    modifyFlag Stopped False
    prepareExecution
    debugTingId True >>> p >>= \case
        Left msg -> failure msg
        Right _ -> continue >>= \case
            Left msg -> failure msg
            Right _ -> executionResult

stepOver :: Emulator ActionResponse
stepOver = do
    prepareExecution
    next >>= \case
        Left msg -> failure msg
        Right _ -> executionResult

stepIn :: Emulator ActionResponse
stepIn = do
    prepareExecution
    step >>= \case
        Left msg -> failure msg
        Right _ -> executionResult

continueExecution :: Emulator ActionResponse
continueExecution = do
    prepareExecution
    continue >>= \case
        Left msg -> failure msg
        Right _ -> executionResult

procedures :: Emulator ActionResponse
procedures = do
    get >>= \case 
        Context {_tingIdBase,_assemblage} -> success $ map (go _tingIdBase) $ zip [0..] _assemblage
    where
        go tingIdBase (i,(name,_)) = printf "@%5d 0x%04x %s" (tingIdBase+i) (tingIdBase+i) name

sounds :: Emulator ActionResponse
sounds = do
    get >>= \case 
        Context {_tingIdBase,_assemblage,_soundLib} -> success $ map (go _tingIdBase _assemblage) $ zip [0..] _soundLib
    where
        go tingIdBase program (i,(name,_)) = 
            let offset = i# tingIdBase + length program
            in printf "#%5d 0x%04x %s" (offset+i) (offset+i) name

symbol :: String -> Emulator ActionResponse
symbol sym = do
    resolveProcedure sym >>= \case
        Right (name,tingId) -> success $ [printf "@%5d 0x%04x %s" tingId tingId name]
        Left _ -> resolveSound sym >>= \case
            Right (name,tingId) -> success $ [printf "#%5d 0x%04x %s" tingId tingId name]
            Left msg -> failure msg

resolveRegister :: String -> Maybe Operand
resolveRegister s 
    | Just x <- readMaybe (map toUpper s) :: Maybe Register = Just $ register x
    | Just x <- readMaybe (map toUpper s) :: Maybe ScratchRegister = Just $ scratchRegister x
resolveRegister _ = Nothing

readRegister :: String -> Emulator ActionResponse
readRegister s 
    | Just r <- resolveRegister s = val r
    where 
        val (R r) = get >>= \case
            Context {_regs} -> do
                let v = _regs V.! i# r
                success $ [printf "r%d=%04xh (%d)" r v v]
        val _ = assert False undefined
readRegister s = failure $ printf "Invalid register %s" s

writeRegister :: String -> String -> Emulator ActionResponse
writeRegister dest src
    | Just x1 <- resolveRegister dest
    , Just x2 <- resolveRegister src = set x1 x2
    | Just x1 <- resolveRegister dest
    , Just x2 <- readMaybe src :: Maybe Word16 = set x1 (I x2)
    where 
        val (R r) = get >>= \case
            Context {_regs} -> return $ _regs V.! i# r
        val _ = assert False undefined
        set r1@(R _) r2@(R _) = do
            v <- val r2
            set r1 (I v)
        set (R r) (I v) = get >>= \case
            Context {_regs} -> do
                modify (\s -> s { _regs = _regs V.// [(i# r, v)]})
                success $ [printf "r%d=%04xh (%d)" r v v]
        set _ _ = assert False undefined
writeRegister d s = failure $ printf "Invalid registers %s and/or %s" d s

value' :: Operand -> Emulator (Maybe Word16)
value' (R r') = get >>= \case
    Context {_regs} -> 
        if r >= 0 && r < V.length _regs
            then return . Just $ (_regs V.! r)
            else return $ Nothing
    where
        r = i# r'
value' _ = assert False undefined

value :: Operand -> Emulator Word16
value op = value' op >>= return . fromJust

output :: Emulator [String]
output = get >>= \case
    Context {_soundStream} -> return _soundStream

clearOutput :: Emulator ()
clearOutput = modify (\s -> s { _soundStream = [] })

