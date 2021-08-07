{- |
* Module      : Main
* Description : ECU Monitor for Rover Mini
* Copyright   : (c) Kentaro UONO, 2018-2021
* License     : MIT Licence
* Maintainer  : info@kuono.net
* Stability   : experimental
* Portability : macOS Big Sur and RaspberyPi OS buster
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import qualified ECU
import UI

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import System.Environment (getArgs,getEnv) --,getEnvironment)
import System.Process -- for auto exiting on Raspberry Pi 

import Brick
import Brick.Main
-- import Brick.Forms
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V
--
-- * Type definitions
--
-- | App type required by the TUI library Brick.
ecuMonitor :: App Status Event Name
ecuMonitor = App { appDraw         = drawPanes             
                 , appChooseCursor = neverShowCursor  
                 , appHandleEvent  = handleEvent      
                 , appStartEvent   = return            
                 , appAttrMap      = const theMap        
                 }
--
-- | Main function 主関数
main :: IO ()
main = do
    args <- System.Environment.getArgs
    envs <- System.Environment.getEnv "HOME"
    let buildVty = V.mkVty V.defaultConfig
        (os, path,intestmode) = case (null args,envs == "/Users/kuono") of
            (True,True) -> ( MacOS         , defaultUSBPathMac         , False )
            (True,_   ) -> ( RaspberryPiOS , defaultUSBPathRaspberryPi , False )
            _           -> if args !! 0 == "-d"
                               then ( MacOS       , defaultUSBPathMac , True  )
                               else error "error: exactly one arguments needed."
    iniVty  <- buildVty
    
    evntCh  <- BC.newBChan 10 :: IO (BC.BChan Event) -- ^ make an event channel for Brick
    ucmdCh  <- atomically newTChan :: IO (TChan ECU.UCommand) 
    logdCh  <- atomically newTChan :: IO (TChan Event)
    iStatus <- initialState (evntCh,ucmdCh,logdCh,intestmode)
    _ <- forkIO $ runlog logdCh
    _ <- forkIO $ forever $ ECU.run ECU.loop (path,evntCh,ucmdCh,logdCh) -- ^ fork communication thread
    _ <- Brick.Main.customMain iniVty buildVty (Just evntCh) ecuMonitor iStatus
    Prelude.putStrLn "Thank you for using Mini ECU Monitor. See you again!"
    _ <- system $ if os == RaspberryPiOS then "sudo shutdown -h now" else "echo \"\"" -- ":" is a command do nothing on bash
    return ()
--
-- |　-- event handlers as an updating model function モデル更新関数群
--
handleEvent :: Status -> BrickEvent Name Event -> EventM Name (Next Status)
handleEvent s (MouseUp   _ _ _   ) = continue s
handleEvent s (MouseDown _ _ _ _ ) = continue s
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
-- Event Handlers as a part of UI
--
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') []))
  | inmenu s  = halt s
  | otherwise = continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar '1') []))
  | not $ inmenu s = continue s
  | otherwise      = do
        let ch = cchan s
        liftIO $ atomically $ writeTChan ch ECU.Init
        continue s { rdat = (rdat s) { note = "I will initialize mems." }}
handleEvent s (VtyEvent (V.EvKey (V.KChar '2') []))
  | not $ testmode s = continue s { rdat = (rdat s) { note = ""}}
  | otherwise        = do
        let ch = cchan s
        liftIO $ atomically $ writeTChan ch ECU.Disconnect
        continue s { rdat = (rdat s) { note = "I will disconnect mems."}}
handleEvent s (VtyEvent (V.EvKey (V.KChar '0') []))
  | inmenu s && not (testmode s) = do
        let ch = cchan s
        liftIO $ atomically $ writeTChan ch ECU.ClearFaults
        continue s { rdat = (rdat s) { note = "I will clear faults."}}
  | otherwise = continue s { rdat = (rdat s) { note = "Got clear fault command but do nothing."}}
-- | 'p' -> Get IAC Position 
-- handleEvent s (VtyEvent (V.EvKey (V.KChar 'p') []))
--   | inmenu s && not (testmode s) = do
--         let ch = cchan s
--         liftIO $ atomically $ writeTChan ch ECU.GetIACPos
--         continue s { rdat = (rdat s) { note = "I will get IAC Position."}}
--   | otherwise = continue s { rdat = (rdat s) { note = ""}}
handleEvent s (VtyEvent (V.EvKey V.KRight []))
  | not $ inmenu s = continue s
  | otherwise = do
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.IncIACPos
            continue s { rdat = (rdat s) { note = "I will increment IAC Pos." }}
handleEvent s (VtyEvent (V.EvKey V.KLeft [])) 
  | not $ inmenu s = continue s
  | otherwise = do
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.DecIACPos
            continue s { rdat = (rdat s) { note = "I will decrement IAC Pos." }}
handleEvent s (VtyEvent (V.EvKey V.KUp []))
  | not $ inmenu s = continue s
  | otherwise = do
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.IncIgAd
            continue s { rdat = (rdat s) { note = "I will increment Ignition Ad." }}
handleEvent s (VtyEvent (V.EvKey V.KDown [])) 
  | not $ inmenu s = continue s
  | otherwise = do
            -- t <- liftIO Lib.currentTime
            let ch = cchan s
            liftIO $ atomically $ writeTChan ch ECU.DecIgAd
            continue s { rdat = (rdat s) { note = "I will decrement Ignition Ad." }}
-- | 'd' -> get dummy data
--     This is a temporal handler which produce a dummy data
--    If the machine is in a test mode, this function do nothing.
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
                          , (coolanttemp, ECU.coolantTemp  f )
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
  | testmode s = continue s { inmenu = True } -- { inmenu = not $ inmenu s}
  | otherwise  = continue s { inmenu = not $ inmenu s }
--
handleEvent s (VtyEvent (V.EvKey (V.KChar _) [])) = continue s
--
handleEvent s (VtyEvent _ ) = continue s
--
