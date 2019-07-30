-- # ECU Monitor for Rover Mini
-- usage
--  ecu dev   ... communicate ECU through dev
--  push any key to end this programme

module Main where

{-# LANGUAGE TemplateHaskell #-}

import ECU

import System.Environment (getArgs,getEnv,getEnvironment)
import System.IO -- for stdin, Buffering Mode
import qualified Data.ByteString as BS

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))   
import Data.List -- for test
import Data.Word
import Data.List.Split
import System.Directory
import Control.Concurrent
import Numeric

-- | デバイス名が指定されなかった場合に使うパス名　
defaultUSBPathMac         = "/dev/tty.usbserial-DO01OV70" :: FilePath 
defaultUSBPathRaspberryPi = "/dev/ttyUSB0" :: FilePath
oldUSBPath   = "/dev/tty.usbserial-DJ00L8EZ" -- :: FilePath
alterntUSBPath   = "/dev/tty.usbserial-FT90HWC8" -- :: FilePath

currentVersion = "0.5.0"
compiledOn     = "2019.07.25"
-- # Version 0.10     by K.Uono on 2017.11.19
-- #         0.4.1.2  by K.Uono on 2019.07.05
-- #         0.4.2.0  by K.Uono on 2019.07.06

-- data App s e n = App {
--         appDraw         :: s -> [Widget n]
--       , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n) 
--       , appHandleEvent  :: s -> BrickEvent n e -> EventM n (Next s) 
--       , appStartEvent   :: s -> EventM n s 
--       , appAttrMap      :: s -> AttrMap 
--       }

-- -- types
-- type Status = -- [(Integer, Data807d,LocalTime)] 
-- data Event  = Tick | ECUData | UserEvent
-- type Name   = () 
-- app :: App Status Event Name
-- app = App { appDraw = drawUI
--           , appChooseCursor = neverShowCursor
--           , appHandleEvent = handleEvent
--           , appStartEvent = return
--           , appAttrMap = const theMap
--           }
-- -- Handling events
-- handleEvent :: Status -> BrickEvent Name Event -> EventM Name (Next Status)
-- handleEvent d b = undefined
-- -- Drawing
-- drawUI :: ECUData -> [Widget Name]
-- drawUI = undefined
-- theMap :: AttrMap
-- theMap = undefined

main :: IO ()
main = do
    --    initialState = undefined
    -- finalState <- defaultMain app initialState -- Use finalState and exit

    args <- System.Environment.getArgs
    env  <- System.Environment.getEnv "HOME"
    let defaultUSBPath = if env == "/Users/kuono" then defaultUSBPathMac
                         else defaultUSBPathRaspberryPi
    hSetBuffering stdin NoBuffering -- set non buffering mode 
    hSetEcho      stdin False

    -- .      \              '             `
    -- '       .              .             `     ,_ .`
    --  ,_--_,   ,            |              /  /`. +'.'
    -- '. + + ". .===========================w. || = =  '
    -- .= = = :|V-Monitor for Rover Mini MEMS-\\ \.+_+.'
    --  '.- -,"|:--------- ------------ -------:|     |
    -- .   .   \\-Version 0.5.0 on 2019.07.25-//   .  .
    --  . `.`  .^.== +--------------------+ == .`.'.` ,
    -- `-------| |-- |  by Kentaro UONO   |----| |-''' 
    --  ``````  '    +--------------------+     '
    
    Prelude.putStr   $ "\ESC[" ++ show 2 ++ "J"
    Prelude.putStrLn   "    \\              '             `"
    Prelude.putStrLn   "     .              .             `     ,_ .`"
    Prelude.putStrLn   "--_,   ,            |              /  /`. +'.'"
    Prelude.putStrLn   "+ + \". .===========================w. || = =  "
    Prelude.putStrLn   "= = :|V-Monitor for Rover Mini MEMS-\\\\ \\.+_+.'"
    Prelude.putStrLn   "- -,\"|:--------- ------------ -------:|     |"
    Prelude.putStrLn $ ( take 37 ( " .   \\\\-Version " ++ currentVersion ++ " on " ++ compiledOn ++ "-//   .  ." )) 
    Prelude.putStrLn   "`.`  .^.== +--------------------+ == .`.'.` ,"
    Prelude.putStrLn   "-----| |-- |  by Kentaro UONO   |----| |-''' "
    Prelude.putStrLn   "````  '    +--------------------+     '"
    Prelude.putStrLn   ""
    -- Prelude.putStrLn   "all rights are reserved.                "
    threadDelay 1000000 -- 2 sec 

    case args of
        []           -> runEcuAt defaultUSBPath
        -- ["-t"]       -> runEcuAt testModeFile
        ["-d"]       -> runEcuAt defaultUSBPath
        [theEcuPort] -> runEcuAt theEcuPort
        _            -> Prelude.putStrLn "error: exactly one arguments needed." 
    Prelude.putStrLn "Type ESC key to quit or any other key to restart :"
    ch <- getChar
    if ch /= '\ESC'
        then
          main 
        else do 
          Prelude.putStrLn "Thank you. See you again! " 
          hSetBuffering stdin LineBuffering -- set buffering mode
          hSetEcho      stdin True
          return ()
