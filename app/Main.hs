-- # ECU Monitor for Rover Mini
-- usage
--  ecu dev   ... communicate ECU through dev
--  push any key to end this programme
-- {-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import qualified ECU
import UI

import Control.Concurrent
import Control.Concurrent.STM.TChan
-- import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
-- import Data.List -- for test
-- import Data.List.Split
-- import Data.Word
-- import Data.Time.LocalTime
-- import Data.Time.Clock
-- import qualified Data.ByteString as BS
-- import Numeric
-- import System.Directory
-- import System.DiskSpace
import System.Environment (getArgs,getEnv) --,getEnvironment)
-- import System.Hardware.Serialport 
-- import System.IO -- for stdin, Buffering Mode
-- import Text.Printf

import Brick
--   ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
--   , customMain, neverShowCursor
--   , continue, halt
--   , hLimit, vLimit, vBox, hBox
--   , padRight, padLeft, padTop, padAll, Padding(..)
--   , withBorderStyle
--   , str
--   , attrMap, withAttr, emptyWidget, attrName, on, fg
--   , (<+>)
--   )
import Brick.Main
import qualified Brick.BChan as BC
-- import qualified Brick.Widgets.Border as B
-- import qualified Brick.Widgets.Border.Style as BS
-- import qualified Brick.Widgets.Center as C
-- import qualified Brick.Widgets.ProgressBar as BP
import qualified Graphics.Vty as V
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as S
-- import Linear.V2 (V2(..))
-- import Lens.Micro ((^.))   

ecuMonitor :: App Status Event Name
ecuMonitor = App { appDraw = drawPanes             
              , appChooseCursor = neverShowCursor  
              , appHandleEvent = handleEvent      
              , appStartEvent = return            
              , appAttrMap = const theMap        
              }
--
-- | Main function 主関数
main :: IO ()
main = do
    args <- System.Environment.getArgs
    env  <- System.Environment.getEnv "HOME"
    let (path,intestmode) = case (null args,env == "/Users/kuono") of
            (True,True) -> (defaultUSBPathMac,False)
            (True,_   ) -> (defaultUSBPathRaspberryPi,False)
            _           -> if args !! 0 == "-d"
              then (defaultUSBPathMac,True)
              else error "error: exactly one arguments needed."
        buildVty = V.mkVty V.defaultConfig
    -- print $ "start GUI on " ++ path
    iniVty  <- buildVty 
    evntCh  <- BC.newBChan 10 :: IO (BC.BChan Event) -- ^ make an event channel for Brick
    ucmdCh  <- atomically newTChan :: IO (TChan ECU.UCommand) 
    logdCh  <- atomically newTChan :: IO (TChan Event)
    iStatus <- initialState (evntCh,ucmdCh,logdCh,intestmode)
    _ <- forkIO $ runlog logdCh
    _ <- forkIO $ forever $ ECU.run ECU.loop (path,evntCh,ucmdCh,logdCh) -- ^ fork communication thread
    _ <- Brick.Main.customMain iniVty buildVty (Just evntCh) ecuMonitor iStatus
    Prelude.putStrLn "Thank you for using Mini ECU Monitor. See you again!"
    return ()
--
-- |　-- event handlers as an updating model function モデル更新関数群
--
handleEvent :: Status -> BrickEvent Name Event -> EventM Name (Next Status)
-- handleEvent s (AppEvent (_,ECU.MouseDown _ _ _ _)) = continue s
-- --
-- handleEvent s (AppEvent (_,ECU.MouseUp   _ _ _  )) = continue s
-- --
handleEvent s (AppEvent (t,ECU.PortNotFound f)) =
    continue s
      { rdat = (rdat s)  
        { evnt = ( t,ECU.PortNotFound f ) 
        , note = "Port Not Found."
        }
      }
--
handleEvent s (AppEvent (t,ECU.Connected m)) = 
    continue s 
      { model = m
      , rdat = (rdat s)
        { evnt = ( t,ECU.Connected m )
        , note = "Connected."
        }
      }
--
handleEvent s (AppEvent (t,ECU.Error estr)) = 
    continue s { rdat = (rdat s) { 
        evnt = ( t,ECU.Error estr )
      , note = "Error " ++ estr } }
--
handleEvent s (AppEvent (t,ECU.Done m)) = 
    continue s { rdat = (rdat s) { evnt = ( t,ECU.Done m) } }
--
handleEvent s (AppEvent (t,ECU.GotIACPos p)) = 
    continue s 
      { rdat = (rdat s) 
        { evnt = ( t,ECU.GotIACPos p ) 
        }
      , lIacPos = Just p 
      }
--
handleEvent s (AppEvent (t,ECU.OffLined)) = 
    continue s { rdat = (rdat s) { evnt = ( t,ECU.OffLined ) } }
--  
handleEvent s (AppEvent (t,ECU.Tick r)) = do
    let f = ECU.parse r
    continue $ s
      { rdat = DataSet
          { evnt = ( t,ECU.Tick r )
          , gdat = 
                  [ (engspeed,    ECU.engineSpeed  f )
                  , (tposition,   ECU.ithrottlePot f )
                  , (mapsensor,   ECU.mapSensor    f )
                  , (battvoltage, ECU.ibattVoltage f )
                  , (coolanttemp, ECU.coolantTemp  f )
                  ]
          , note = "Tick"
          }
      , dset    = take maxData $ rdat s : dset s
      , lIacPos = Just $ ECU.idleACMP f
      , iCoolT  = case iCoolT s of
          Just _  -> iCoolT s
          Nothing -> Just $ ECU.coolantTemp f  
      }
--
--
-- Event Handlers as a part of UI
--
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') []))
  | inmenu s  = halt s
  | otherwise = continue s
--
handleEvent s (VtyEvent (V.EvKey (V.KChar '1') []))
  | not $ inmenu s = continue s
  | otherwise      = do
        let ch = cchan s
        liftIO $ atomically $ writeTChan ch ECU.Init
        continue s { rdat = (rdat s) { note = "I will initialize mems." }}
--
handleEvent s (VtyEvent (V.EvKey (V.KChar '2') []))
  | not $ testmode s = continue s { rdat = (rdat s) { note = ""}}
  | otherwise        = do
        let ch = cchan s
        liftIO $ atomically $ writeTChan ch ECU.Disconnect
        continue s { rdat = (rdat s) { note = "I will disconnect mems."}}
--
handleEvent s (VtyEvent (V.EvKey (V.KChar '0') []))
  | inmenu s && not (testmode s) = do
        let ch = cchan s
        liftIO $ atomically $ writeTChan ch ECU.ClearFaults
        continue s { rdat = (rdat s) { note = "I will clear faults."}}
  | otherwise = continue s { rdat = (rdat s) { note = "Got clear fault command but do nothing."}}
--
-- handleEvent s (VtyEvent (V.EvKey (V.KChar 'p') []))
--   | inmenu s && not (testmode s) = do
--         let ch = cchan s
--         liftIO $ atomically $ writeTChan ch ECU.GetIACPos
--         continue s { rdat = (rdat s) { note = "I will get IAC Position."}}
--   | otherwise = continue s { rdat = (rdat s) { note = ""}}
--
handleEvent s (VtyEvent (V.EvKey V.KRight []))
  | not $ inmenu s = continue s
  | otherwise = do
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.IncIACPos
            continue s { rdat = (rdat s) { note = "I will increment IAP Pos." }}
--
handleEvent s (VtyEvent (V.EvKey V.KLeft [])) 
  | not $ inmenu s = continue s
  | otherwise = do
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.DecIACPos
            continue s { rdat = (rdat s) { note = "I will decrement IAP Pos." }}
--
handleEvent s (VtyEvent (V.EvKey V.KUp []))
  | not $ inmenu s = continue s
  | otherwise = do
            -- t <- liftIO Lib.currentTime
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.IncIgAd
            continue s { rdat = (rdat s) { note = "I will increment Ignition Ad." }}
--
handleEvent s (VtyEvent (V.EvKey V.KDown [])) 
  | not $ inmenu s = continue s
  | otherwise = do
            -- t <- liftIO Lib.currentTime
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.DecIgAd
            continue s { rdat = (rdat s) { note = "I will decrement Ignition Ad." }}
-- | get dummy data
--   臨時のハンドラー；ECUにつながることがわかったら別のロジックに
--   むりやりOnlineステータスにする。手続き的なECUへの司令を
--   できれば定義的なものに変えたい。状態遷移ブロックへの指示？
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') []))
  | not $ testmode s = continue s
  | otherwise        = do
          t <- liftIO Lib.currentTime
          r <- liftIO ECU.dummyData807d
          let f = ECU.parse r
          continue $ s { 
              rdat = (rdat s) {
                  evnt = ( t,ECU.Tick r )
                , gdat =  [ (engspeed,    ECU.engineSpeed  f )
                          , (tposition,   ECU.ithrottlePot f )
                          , (mapsensor,   ECU.mapSensor    f )
                          , (battvoltage, ECU.ibattVoltage f )
                          ,  (coolanttemp,ECU.coolantTemp  f )
                          ]
            , note    = "I have got dummy data" }
            , dset    = take maxData $ rdat s : dset s
            , lIacPos = Just $ ECU.idleACMP f 
            , iCoolT  = case iCoolT s of
                          Just _  -> iCoolT s
                          Nothing -> Just $ ECU.coolantTemp f
            }
--
handleEvent s (VtyEvent (V.EvKey V.KEsc []))
  | testmode s = continue s -- { inmenu = not $ inmenu s}
  | otherwise  = continue s { inmenu = not $ inmenu s }
--
handleEvent s (VtyEvent (V.EvKey (V.KChar _) [])) = continue s
--
handleEvent s (VtyEvent _ ) = continue s
--
