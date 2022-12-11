{- |
* Module      : Main
* Description : ECU Monitor for Rover Mini
* Copyright   : (c) Kentaro UONO, 2018-2022
* License     : MIT Licence
* Maintainer  : info@kuono.net
* Stability   : experimental
* Portability : macOS Big Sur/Monterey and RaspberyPi OS buster
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
--
import Lib
    ( currentTime,
      defaultUSBPathMac,
      defaultUSBPathRaspberryPi,
      frameTitle,
      localTimetoString,
      AfterAction(Quit, Shutdown, Restart),
      OSEnv(RaspberryPiOS, MacOS) )
import Brick ( customMain )
import qualified Brick.BChan as BC
import qualified ECU
import UI
    ( Status(after, cchan), initialState, ecuMonitor, frametoTable ) 

import Control.Concurrent ( forkIO, forkFinally )
import Control.Concurrent.STM.TChan ( TChan, newTChan, writeTChan, readTChan )
import Control.Exception as Ex ( catch, SomeException )
import Control.Monad ( forever , when)
import Control.Monad.STM ( atomically )
import Data.Fixed ( showFixed )
import Data.Time
    ( LocalTime(LocalTime), TimeOfDay(todSec, todHour, todMin) )
import System.Environment ( getArgs, getEnv ) --,getEnvironment)
import System.IO
    ( Handle,
      hClose,
      hFlush,
      hPutStr,
      hPutStrLn,
      withFile,
      IOMode(WriteMode) )
import System.Process ( system ) -- for auto exiting on Raspberry Pi 
import Text.Printf ( printf ) 

import qualified Graphics.Vty as V
--
--
runlog :: TChan ECU.Event -> IO ()
runlog ech = forever $ do
  l <- logFileName :: IO FilePath
  withFile l WriteMode $
    \h -> do
      hPutStrLn h  $ "Date,Time," ++ frameTitle
      loop h
    where loop :: Handle -> IO ()
          loop h = 
            do
              -- r <- System.DiskSpace.getAvailSpace l
              (t,e) <- atomically $ readTChan ech
              let j  = localTimetoString t
              hPutStr h $ j ++ ","
              hPutStr h $ case e of 
                ECU.Tick r         -> frametoTable $ ECU.parse r
                ECU.GotIACPos p    -> " Got IAC Pos    : " ++ show p
                ECU.PortNotFound f -> " Port Not Found : " ++ f
                ECU.Connected m    -> " Connected      : " ++ show (ECU.model m)
                ECU.OffLined       -> " Off Lined. "
                ECU.Done m         -> " Done           : " ++ show m
                ECU.Error s        -> " Error          : " ++ s
              hPutStrLn h $ case e of 
                ECU.Tick r -> show r
                _          -> ""
              hFlush h
              case e of
                ECU.OffLined -> do  
                                   putStrLn $ "Exception " ++ show (e::Ex.SomeException) ++ "issued."
                                   hClose h  
                _            -> loop h
--
logFileName :: IO FilePath
logFileName = do
    time <- currentTime :: IO LocalTime
    return $ localtimeToFilePath time
    where localtimeToFilePath (LocalTime n t) =  -- to convert constant length string
              let hi     = show n
                  ji     = todHour t
                  hun    = todMin  t 
                  byo    = todSec  t
                  byo'   = take 2 $ if byo >= 10 then showFixed False byo
                                    else '0':showFixed False byo
              in printf "ECULog%10s_%02d.%02d.%2s.csv" hi ji hun byo' -- ex. ECULog2018-10-15_17.27.26.csv
              --  ./log/ECU...としていたが，ディレクトリが存在していないとランタイムエラーを起こすので変更
-- | Main action 主関数
main :: IO ()
main = do
    args <- System.Environment.getArgs
    envs <- System.Environment.getEnv "HOME"
    let buildVty = V.mkVty V.defaultConfig
        (os, path,intestmode) = case (null args,envs == "/Users/kuono") of
            (True,True) -> ( MacOS         , defaultUSBPathMac         , False )
            (True,_   ) -> ( RaspberryPiOS , defaultUSBPathRaspberryPi , False )
            _           -> if length args == 1 
                               then ( os , head args , True )
                               else error "error: exactly one arguments needed."
    iniVty  <- buildVty    
    evntCh  <- BC.newBChan 10      :: IO (BC.BChan ECU.Event    )            
    ucmdCh  <- atomically newTChan :: IO (TChan    ECU.UCommand ) 
    logdCh  <- atomically newTChan :: IO (TChan    ECU.Event    )

    iStatus <- initialState
    -- | ここの処理が原始的すぎ。
    _ <- forkIO $ runlog logdCh 
    _ <- forkIO $ forever $ ECU.run (path,evntCh,ucmdCh,logdCh) 

    finalState <- Brick.customMain iniVty buildVty (Just evntCh) ecuMonitor iStatus

    let cmdchan = cchan finalState
    _ <- atomically $ writeTChan cmdchan ECU.Disconnect   -- これで各スレッドは落としている 

    Prelude.putStrLn "Thank you for using Mini ECU Monitor. See you again!"
    -- killThread ecuT -- いきなり kill すると支障が出るか，要調査
    -- killThread logT -- = throw ecuT ThreadKilled <- hCloseしている
    _ <- system $ if os /= RaspberryPiOS then
                    "echo \"\""  -- ":" is a command do nothing on bash
                  else
                    case after finalState of
                      Shutdown ->  "sudo shutdown -h now" 
                      Restart  ->  "sudo shutdown -r now"
                      Quit     ->  "echo \"\""  -- ":" is a command do nothing on bash
    return ()
