{- |
* Module      : Lib
* Description : Ecu Communication Library for Rover Mini which includes common functions
* Copyright   : (c) Kentaro UONO, 2018-2022
* License     : MIT Licence
* Maintainer  : info@kuono.net
* Stability   : experimental
* Portability : 
-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where 
--
-- import Brick 
-- import qualified Brick.BChan as BC
-- import Brick.Main
--     ( customMain, halt, continueWithoutRedraw, neverShowCursor, App(..) )
-- import Brick.Forms
--
import Data.Version.Package as D ( packageVersionStringTH )
import Data.Fixed ( showFixed )
import Data.Time.LocalTime
    ( LocalTime(LocalTime), utcToLocalTime, getCurrentTimeZone,
      TimeOfDay(todHour, todMin, todSec) )
import Data.Time.Clock ( getCurrentTime )
import Text.Printf ( printf )
--
ver   :: String
ver   = $$(packageVersionStringTH "ecu.cabal")
--
-- | Enviroment
data OSEnv = MacOS | RaspberryPiOS | UnsupportedOS deriving Eq
data UIEnv = Brix | Line | GUI
data AfterAction
  = Shutdown
  | Restart
  | Quit
  -- | Ask       -- ^ ループから抜け，その後の動作を尋ねる（1分放置すると再起動）
-- 
-- global constants　グローパル定数
-- 
maxData :: Int
maxData = 30

frameTitle  :: String
frameTitle  = "E Speed,coolant T,ambient T,intakeAir T,fuel T,map Sensor,btVolt,throtle Pot,idle Byte,0B,p/n switch,0D,0E,0F,10,11,iACMP,iSDev,15,ignAd,coil T,19,1A,1B,lmdvt,clsdl,fuelt"
frameFmt    :: String
frameFmt    = "%5d,%3d,%3d,%3d,%3d,%3d,%6.2f,%6.2f,%02X,%02X,%3d,%02X,%02X,%02X,%02X,%02X,%3d,%6d,%02X,%5.1f,%5.1f,%02X,%02X,%02X,%5d,%3d,%5d"

-- black    = "\ESC[30m" 
-- red      = "\ESC[31m" 
-- green    = "\ESC[32m"  
-- yellow   = "\ESC[33m"
-- blue     = "\ESC[34m"
-- magenta  = "\ESC[35m"
-- cyan     = "\ESC[36m"
-- white    = "\ESC[37m"

-- -- | グラフ描画用データセット ; でもいまは使っていない
-- graphData :: [GraphItem]
-- graphData = [engspeed,tposition,mapsensor,battvoltage,coolanttemp]
-- engspeed, tposition, mapsensor, battvoltage, coolanttemp :: GraphItem
-- engspeed    = GraphItem { name = "Engine Speed(rpm)",
--   chr = '*',pwr = 0, minl = 0,   maxl = 24000,ul = 4000,ll = 700} 
-- tposition   = GraphItem { name = "Throttle Pot( V )",
--   chr = 'T',pwr = 2, minl = 0,   maxl = 500,  ul = 300, ll = 0  }
-- mapsensor   = GraphItem { name = "MAP Sensor  (kPa)",
--   chr = 'M',pwr = 0, minl = 0,   maxl = 110,  ul = 70,  ll = 20 }
-- battvoltage = GraphItem { name = "Battery Volt( V )",
--   chr = 'V',pwr = 1, minl = 0,   maxl = 170,  ul = 150, ll = 100} 
-- coolanttemp = GraphItem { name = "Coolant Temp(\'C )",
--   chr = 'C',pwr = 1, minl = -55, maxl = 120,  ul =98,   ll = -20}

-- | デバイス名が指定されなかった場合に使うパス名　
defaultUSBPathMac         :: FilePath 
defaultUSBPathMac         = "/dev/tty.usbserial-DO01OV70"
defaultUSBPathRaspberryPi :: FilePath
defaultUSBPathRaspberryPi = "/dev/ttyUSB0"
-- oldUSBPath                = "/dev/tty.usbserial-DJ00L8EZ" -- :: FilePath
-- alterntUSBPath            = "/dev/tty.usbserial-FT90HWC8" -- :: FilePath
--
-- | 現在時刻を得る
currentTime :: IO LocalTime
currentTime = do
    timezone <- getCurrentTimeZone
    utcToLocalTime timezone <$> getCurrentTime
--
-- | to convert constant length string
localTimetoString :: LocalTime -> String
localTimetoString (LocalTime n t) = 
    let hi     = show    n
        ji     = todHour t
        hun    = todMin  t
        byo    = todSec  t
        byo'   = take 9 $ if byo >= 10 then showFixed False byo
                              else '0':showFixed False byo 
    in printf "%10s,%02d:%02d:%5s " hi ji hun byo'
-- |
ratio:: (Real a) => a -> a -> a -> Float 
ratio ll ul v = fromRational ((toRational v - toRational ll ) / (toRational ul - toRational ll))
-- |
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
