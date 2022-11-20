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
import qualified ECU
import qualified UI
import Lib
    ( currentTime,
      defaultUSBPathMac,
      defaultUSBPathRaspberryPi,
      frameTitle,
      localTimetoString,
      logFileName,
      AfterAction(Quit, Shutdown, Restart),
      OSEnv(RaspberryPiOS, MacOS) )
-- * framework 
import Brick ( customMain )
import qualified Brick.BChan as BC
import qualified Graphics.Vty as V
-- * general purpose library
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM.TChan ( TChan, newTChan, writeTChan, readTChan )
import Control.Exception as Ex ( catch, SomeException )
import Control.Monad ( forever , when)
import Control.Monad.STM ( atomically )
import Data.Fixed ( showFixed )
import Data.Time
    ( LocalTime(LocalTime), TimeOfDay(todSec, todHour, todMin) )
import qualified System.Environment as Sys ( getArgs, getEnv ) --,getEnvironment)
import System.IO ( Handle,IOMode(WriteMode),
      hClose, hFlush, hPutStr, hPutStrLn, withFile )
import System.Process ( system ) -- for auto exiting on Raspberry Pi 
import Text.Printf (printf)
import Control.Exception.Base (throwIO)
import Control.Exception (AsyncException(ThreadKilled))
import Control.Concurrent (throwTo)
--
-- | Main action 主関数
main :: IO ()
main = do
    args <- Sys.getArgs
    envs <- Sys.getEnv "HOME"
    let buildVty = V.mkVty V.defaultConfig
        (os, path) = case (null args,envs == "/Users/kuono") of
            (True,True) -> ( Lib.MacOS         , Lib.defaultUSBPathMac         )
            (True,_   ) -> ( Lib.RaspberryPiOS , Lib.defaultUSBPathRaspberryPi )
            _           -> if length args == 1 
                               then ( os , head args )
                               else error "error: exactly one arguments needed."
    iniVty  <- buildVty
    evntCh  <- BC.newBChan 10      :: IO (BC.BChan ECU.Event    ) -- ^ brick で使うイベントチャネルの初期化      
    ucmdCh  <- atomically newTChan :: IO (TChan    ECU.UCommand ) -- ^ ECUドライバへの司令チャネル。TQueue化予定
    logCh   <- atomically newTChan :: IO (TChan    ECU.Event    ) -- ^ 
    --
    iStatus <- UI.initialState
    -- | ここから並列処理が始まるが，プロセスの処理が原始的すぎ。
    --   現時点では ecu はごく少数かつ増加しないプロセスによる素朴な並列処理アーキテクチャを
    --   採用しているため大きな問題は起こさないが，より多くのデバイスの追加などに対応できない。
    --   階層化eDSLを使った一般化をすべきだろう。
    logT <- forkIO $ runlog logCh
    _ <- forkIO $ forever $ ECU.run (path,evntCh,ucmdCh,logCh) -- ^ デバイスドライバの起動 

    finalState <- Brick.customMain iniVty buildVty (Just evntCh) UI.ecuMonitor iStatus

    let cmdchan = UI.cchan finalState
    atomically $ writeTChan cmdchan ECU.Disconnect   -- これで各スレッドは落としている 
    throwTo logT ThreadKilled
    Prelude.putStrLn "Thank you for using Mini ECU Monitor. See you again!"
    -- killThread ecuT -- いきなり kill すると支障が出るか，要調査
    -- killThread logT -- = throw ecuT ThreadKilled <- hCloseしている
    _ <- system $ if os /= Lib.RaspberryPiOS then
                    "echo \"\""  -- ":" is a command do nothing on bash
                  else
                    case UI.after finalState of
                      Lib.Shutdown ->  "sudo shutdown -h now" 
                      Lib.Restart  ->  "sudo shutdown -r now"
                      Lib.Quit     ->  "echo \"\""  -- ":" is a command do nothing on bash
    return ()
--
-- | logger 
runlog :: TChan ECU.Event -> IO ()
runlog ech = forever $ do
  l <- Lib.logFileName :: IO FilePath
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
                ECU.Tick r         -> ECU.frametoTable $ ECU.parse r
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
              when (e /= ECU.OffLined) $ loop h
            `Ex.catch`
              \e -> do
                putStrLn $ "Exception " ++ show (e::Ex.SomeException) ++ "issued."
                hClose h  
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
