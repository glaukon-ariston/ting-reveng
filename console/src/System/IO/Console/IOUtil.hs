{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
Unicode console I/O in Haskell on Windows
https://stackoverflow.com/questions/10779149/unicode-console-i-o-in-haskell-on-windows

It seems rather difficult to get console I/O to work with Unicode characters in Haskell under windows. Here is the tale of woe:

(Preliminary.) Before you even consider doing Unicode I/O in the console under windows, you need to make sure that you're using a console font which can render the characters you want. The raster fonts (the default) have infinitely poor coverage (and don't allow copy pasting of characters they can't represent), and the truetype options MS provides (consolas, lucida console) have not-great coverage (though these will allow copy/pasting of characters they cannot represent). You might consider installing DejaVu Sans Mono (follow the instructions at the bottom here; you may have to reboot before it works). Until this is sorted, no apps will be able to do much Unicode I/O; not just Haskell.
Having done this, you will notice that some apps will be able to do console I/O under windows. But getting it to work remains quite complicated. There are basically two ways to write to the console under windows. (What follows is true for any language, not just Haskell; don't worry, Haskell will enter the picture in a bit!)...
Option A is to use the usual c-library style byte-based i/o functions; the hope is that the OS will interpret these bytes according to some encoding which can encode all the weird and wonderful characters you want. For instance, using the equivalent technique on Mac OS X, where the standard system encoding is usually UTF8, this works great; you send out utf8 output, you see pretty symbols.
On windows, it works less well. The default encoding that windows expects will generally not be an encoding covering all the Unicode symbols. So if you want to see pretty symbols this way, one way or another, you need to change the encoding. One possibility would be for your program to use the SetConsoleCP win32 command. (So then you need to bind to the Win32 library.) Or, if you'd rather not do that, you can expect your program's user to change the code page for you (they would then have to call the chcp command before they run your program).
Option B is to use the Unicode-aware win32 console API commands like WriteConsoleW. Here you send UTF16 direct to windows, which renders it happily: there's no danger of an encoding mismatch because windows always expects UTF16 with these functions.
Unfortunately, neither of these options works very well from Haskell. First, there are no libraries that I know of that use Option B, so that's not very easy. This leaves option A. If you use Haskell's I/O library (putStrLn and so on), this is what the library will do. In modern versions of Haskell, it will carefully ask windows what the current code page is, and output your strings in the proper encoding. There are two problems with this approach:

One is not a showstopper, but is annoying. As mentioned above, the default encoding will almost never encode the characters you want: you are the user need to change to an encoding which does. Thus your user needs to chcp cp65001 before they run your program (you may find it distasteful to force your users to do this). Or you need to bind to SetConsoleCP and do the equivalent inside your program (and then use hSetEncoding so that the Haskell libraries will send output using the new encoding), which means you need to wrap the relevant part of the win32 libraries to make them Haskell-visible.
Much more seriously, there is a bug in windows (resolution: won't fix) which leads to a bug in Haskell which means that if you have selected any code page like cp65001 which can cover all of Unicode, Haskell's I/O routines will malfunction and fail. So essentially, even if you (or your user) set the encoding properly to some encoding which covers all the wonderful Unicode characters, and then 'do everything right' in telling Haskell to output things using that encoding, you still lose.
The bug listed above is still unresolved and listed as low priority; the basic conclusion there is that Option A (in my classification above) is unworkable and one needs to switch to Option B to get reliable results. It is not clear what the timeframe will be for this being resolved, as it looks like some considerable work.

The question is: in the meantime, can anyone suggest a workaround to allow the use of Unicode console I/O in Haskell under windows.

See also this python bug tracker database entry, grappling with the same problem in Python 3 (fix proposed, but not yet accepted into the codebase), and this stackoverflow answer, giving a workaround for this problem in Python (based on 'option B' in my classification).



==Answer==

I thought I would answer my own question, and list as one possible answer, the following, which is what I'm actually doing at the moment. It is quite possible that one can do better, which is why I'm asking the question! But I thought it would make sense to make the following available to people. It's basically a translation from Python to Haskell of this python workaround for the same issue. It uses 'option B' mentioned in the question.

The basic idea is that you create a module IOUtil.hs, with the following content, which you can import into your code:
...
then, you use the I/O functions therein contained instead of the standard library ones. They will detect whether output is redirected; if not (i.e. if we're writing to a 'real' console) then we'll bypass the usual Haskell I/O functions and write directly to the win32 console using WriteConsoleW, the unicode-aware win32 console function. On non-windows platforms, conditional compilation means that the functions here just call the standard-library ones.

If you need to print to stderr, you should use (e.g.) ePutStrLn, not hPutStrLn stderr; we don't define a hPutStrLn. (Defining one is an exercise for the reader!)

-}

module System.IO.Console.IOUtil 
  ( System.IO.Console.IOUtil.interact
  , System.IO.Console.IOUtil.putChar
  , System.IO.Console.IOUtil.putStr
  , System.IO.Console.IOUtil.putStrLn
  , System.IO.Console.IOUtil.print
  , System.IO.Console.IOUtil.getChar
  , System.IO.Console.IOUtil.getLine
  , System.IO.Console.IOUtil.getContents
  , System.IO.Console.IOUtil.readIO
  , System.IO.Console.IOUtil.readLn
  , ePutChar, ePutStr, ePutStrLn, ePrint
  , trace, traceIO
  ) where

#ifdef mingw32_HOST_OS

import System.Win32.Types (BOOL, HANDLE, DWORD, LPDWORD, LPWSTR, LPVOID)
import Foreign.C.Types (CWchar)
import Foreign
import Prelude hiding (getContents, putStr, putStrLn) --(IO, Read, Show, String)
--import qualified System.IO
import qualified System.IO (getContents)
import System.IO hiding (getContents, putStr, putStrLn)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (ord)

 {- <http://msdn.microsoft.com/en-us/library/ms683231(VS.85).aspx>
    HANDLE WINAPI GetStdHandle(DWORD nStdHandle);
    returns INVALID_HANDLE_VALUE, NULL, or a valid handle -}

foreign import stdcall unsafe "GetStdHandle" win32GetStdHandle :: DWORD -> IO (HANDLE)

std_OUTPUT_HANDLE :: DWORD
std_OUTPUT_HANDLE = -11   -- all DWORD arithmetic is performed modulo 2^n

std_ERROR_HANDLE :: DWORD
std_ERROR_HANDLE  = -12

 {- <http://msdn.microsoft.com/en-us/library/aa364960(VS.85).aspx>
    DWORD WINAPI GetFileType(HANDLE hFile); -}

foreign import stdcall unsafe "GetFileType" win32GetFileType :: HANDLE -> IO (DWORD)
_FILE_TYPE_CHAR :: DWORD
_FILE_TYPE_CHAR = 0x0002

_FILE_TYPE_REMOTE :: DWORD
_FILE_TYPE_REMOTE = 0x8000

 {- <http://msdn.microsoft.com/en-us/library/ms683167(VS.85).aspx>
    BOOL WINAPI GetConsoleMode(HANDLE hConsole, LPDWORD lpMode); -}

foreign import stdcall unsafe "GetConsoleMode" win32GetConsoleMode :: HANDLE -> LPDWORD -> IO (BOOL)
_INVALID_HANDLE_VALUE :: HANDLE
_INVALID_HANDLE_VALUE = (intPtrToPtr $ -1)

is_a_console :: HANDLE -> IO (Bool)
is_a_console handle
  = if (handle == _INVALID_HANDLE_VALUE) then return False
      else do ft <- win32GetFileType handle
              if ((ft .&. complement _FILE_TYPE_REMOTE) /= _FILE_TYPE_CHAR) then return False
                else do ptr <- malloc
                        cm  <- win32GetConsoleMode handle ptr
                        free ptr
                        return cm

real_stdout :: IO (Bool)
real_stdout = is_a_console =<< win32GetStdHandle std_OUTPUT_HANDLE

real_stderr :: IO (Bool)
real_stderr = is_a_console =<< win32GetStdHandle std_ERROR_HANDLE

 {- BOOL WINAPI WriteConsoleW(HANDLE hOutput, LPWSTR lpBuffer, DWORD nChars,
                              LPDWORD lpCharsWritten, LPVOID lpReserved); -}

foreign import stdcall unsafe "WriteConsoleW" win32WriteConsoleW
  :: HANDLE -> LPWSTR -> DWORD -> LPDWORD -> LPVOID -> IO (BOOL)

data ConsoleInfo = ConsoleInfo Int (Ptr CWchar) (Ptr DWORD) HANDLE

writeConsole :: ConsoleInfo -> [Char] -> IO ()
writeConsole (ConsoleInfo bufsize buf written handle) string
  = let fillbuf :: Int -> [Char] -> IO ()
        fillbuf i [] = emptybuf buf i []
        fillbuf i remain@(first:rest)
          | i + 1 < bufsize && ordf <= 0xffff = do pokeElemOff buf i asWord
                                                   fillbuf (i+1) rest
          | i + 1 < bufsize && ordf >  0xffff = do pokeElemOff buf i word1
                                                   pokeElemOff buf (i+1) word2
                                                   fillbuf (i+2) rest
          | otherwise                         = emptybuf buf i remain
          where ordf   = ord first
                asWord = fromInteger (toInteger ordf) :: CWchar
                sub    = ordf - 0x10000
                word1' = ((shiftR sub 10) .&. 0x3ff) + 0xD800
                word2' = (sub .&. 0x3FF)             + 0xDC00
                word1  = fromInteger . toInteger $ word1'
                word2  = fromInteger . toInteger $ word2'


        emptybuf :: (Ptr CWchar) -> Int -> [Char] -> IO ()
        emptybuf _ 0 []     = return ()
        emptybuf _ 0 remain = fillbuf 0 remain
        emptybuf ptr nLeft remain
          = do let nLeft'    = fromInteger . toInteger $ nLeft
               ret          <- win32WriteConsoleW handle ptr nLeft' written nullPtr
               nWritten     <- peek written
               let nWritten' = fromInteger . toInteger $ nWritten
               if ret && (nWritten > 0)
                  then emptybuf (ptr `plusPtr` (nWritten' * szWChar)) (nLeft - nWritten') remain
                  else fail "WriteConsoleW failed.\n"

    in  fillbuf 0 string

szWChar :: Int
szWChar = sizeOf (0 :: CWchar)

makeConsoleInfo :: DWORD -> Handle -> IO (Either ConsoleInfo Handle)
makeConsoleInfo nStdHandle fallback
  = do handle     <- win32GetStdHandle nStdHandle
       is_console <- is_a_console handle
       let bufsize = 10000
       if not is_console then return $ Right fallback
         else do buf     <- mallocBytes (szWChar * bufsize)
                 written <- malloc
                 return . Left $ ConsoleInfo bufsize buf written handle

{-# NOINLINE stdoutConsoleInfo #-}
stdoutConsoleInfo :: Either ConsoleInfo Handle
stdoutConsoleInfo = unsafePerformIO $ makeConsoleInfo std_OUTPUT_HANDLE stdout

{-# NOINLINE stderrConsoleInfo #-}
stderrConsoleInfo :: Either ConsoleInfo Handle
stderrConsoleInfo = unsafePerformIO $ makeConsoleInfo std_ERROR_HANDLE stderr

interact     :: (String -> String) -> IO ()
interact f   = do s <- getContents
                  putStr (f s)

conPutChar :: ConsoleInfo -> Char -> IO ()
conPutChar ci  = writeConsole ci . replicate 1

conPutStr :: ConsoleInfo -> [Char] -> IO ()
conPutStr      = writeConsole

conPutStrLn :: ConsoleInfo -> [Char] -> IO ()
conPutStrLn ci = writeConsole ci . ( ++ "\n")

putChar      :: Char -> IO ()
putChar      = (either conPutChar  hPutChar ) stdoutConsoleInfo

putStr       :: String -> IO ()
putStr       = (either conPutStr   hPutStr  ) stdoutConsoleInfo

putStrLn     :: String -> IO ()
putStrLn     = (either conPutStrLn hPutStrLn) stdoutConsoleInfo

print        :: Show a => a -> IO ()
print        = putStrLn . show

getChar :: IO Char
getChar      = System.IO.getChar

getLine :: IO String
getLine      = System.IO.getLine

getContents :: IO String
getContents  = System.IO.getContents

readIO       :: Read a => String -> IO a
readIO       = System.IO.readIO

readLn       :: Read a => IO a
readLn       = System.IO.readLn

ePutChar     :: Char -> IO ()
ePutChar     = (either conPutChar  hPutChar ) stderrConsoleInfo

ePutStr     :: String -> IO ()
ePutStr      = (either conPutStr   hPutStr  ) stderrConsoleInfo

ePutStrLn   :: String -> IO ()
ePutStrLn    = (either conPutStrLn hPutStrLn) stderrConsoleInfo

ePrint       :: Show a => a -> IO ()
ePrint       = ePutStrLn . show

#else

import qualified System.IO
import Prelude (IO, Read, Show, String)

interact     = System.IO.interact
putChar      = System.IO.putChar
putStr       = System.IO.putStr
putStrLn     = System.IO.putStrLn
getChar      = System.IO.getChar
getLine      = System.IO.getLine
getContents  = System.IO.getContents
ePutChar     = System.IO.hPutChar System.IO.stderr
ePutStr      = System.IO.hPutStr System.IO.stderr
ePutStrLn    = System.IO.hPutStrLn System.IO.stderr

print        :: Show a => a -> IO ()
print        = System.IO.print

readIO       :: Read a => String -> IO a
readIO       = System.IO.readIO

readLn       :: Read a => IO a
readLn       = System.IO.readLn

ePrint       :: Show a => a -> IO ()
ePrint       = System.IO.hPrint System.IO.stderr

#endif

trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    traceIO string
    return expr

traceIO :: String -> IO ()
traceIO = ePutStrLn
