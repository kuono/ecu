-- # ECU control programme MyMiniMoni
-- # Version 0.10 by K.Uonon 2017.11.19
-- # before compiling this programme,
--     1) install Haskell
--     2) cabal update
--     3) cabal install serialport
--
-- usage
--  mems dev   ... communicate ECU through dev
--  push any key to end this programme

module Main where

import Lib (runEcuAt , defaultUSBPath, testModeFile )
import System.Environment (getArgs)
import System.IO -- for stdin, Buffering Mode
import qualified Data.ByteString as BS
    
    
import Data.List -- for test
import Data.Word
import Data.List.Split
import System.Directory
import Numeric
     
main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        []           -> do
            hSetBuffering stdin NoBuffering -- set non buffering mode 
            hSetEcho      stdin True
            Prelude.putStr "Test with testModeFile.csv ? (y/N)"
            hFlush stdout
            c <- getChar
            Prelude.putStrLn ""
            if c `elem` "yYfF" then
                runEcuAt testModeFile
            else 
                runEcuAt defaultUSBPath
        [theEcuPort] -> runEcuAt theEcuPort
        _            -> Prelude.putStrLn "error: exactly one arguments needed." 
    hSetBuffering stdin LineBuffering -- set buffering mode
    hSetEcho      stdin True
    Prelude.putStrLn "Thank you. See you again! " 
    return ()
