{- |
Module      : Lib
Description : Ecu Communication Library for Rover Mini
Copyright   : (c) Kentaro UONO, 2018-2019
License     : N/A
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}

module Lib where 

import Control.Concurrent.STM.TBQueue
import qualified Brick.BChan as BC

import qualified ECU
import System.Directory
import System.Environment
import qualified Data.ByteString   as BS
import System.Hardware.Serialport
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Graphics.Vty as V
import Text.Printf
import Data.Fixed

--
-- types
--
-- | For App Status on Brick
data DataSet = DataSet { -- time :: LocalTime, stat :: ECU.Status, edat :: ECU.Frame,
    evnt :: !ECU.Event
  , gdat :: !ChartData
  , note :: !String }
data Status = Status {
    rdat  :: DataSet   -- ^ rdat  : 直近のデータセット
  , dset  :: [DataSet] -- ^ dset  : 直前までのデータセット（最大 maxData個）
  , cset  :: (BC.BChan Event,TBQueue ECU.UCommand)
  , menu  :: MenuTree
}
-- GUI (Brick specific)
data Name  = StatusPane | DataPane | GraphPane | FaultCodePane deriving (Eq,Ord,Show) -- MenuPane | StatusPane | CommandSelectPane deriving (Eq,Ord)
type Event = ECU.Event 
-- Menu
type MenuTree = [Menu]
type Menu  = [MenuItem]
data MenuItem = MenuItem
  { mstring :: !String
  , checked :: !Bool
  , scutchr :: !Char
  }
basicMenu = 
    [ [ MenuItem {mstring = "F:File",checked = False, scutchr = 'F'}
      , MenuItem {mstring = " Q:Quit",checked = False, scutchr = 'Q'}
      ]
    , [ MenuItem {mstring = "E:Edit",checked = False, scutchr = 'E'}
      , MenuItem {mstring = " O:Option",checked = False, scutchr = 'O'}
      ]
    , [ MenuItem {mstring = "Command", checked = False, scutchr = 'G'}
      , MenuItem {mstring = " 0:ClearFault", checked = False, scutchr = '0'}
      ]
    ]
-- Graph
type ChartData = [(GraphItem,Int)]
data GraphItem = GraphItem {
    name  :: !String         -- ^ for note
  , chr   :: !Char           -- ^ character used to chart
  , pwr   :: !Int            -- ^ powor num for 10 to get real data 
  , minl  :: !Int            -- ^ minimum data limit when running
  , maxl  :: !Int            -- ^ maximun data limit when running
  , ul    :: !Int            -- ^ upper limit for checking conditions
  , ll    :: !Int            -- ^ lower limit for checking conditions
}

-- 
-- global constants　グローパル定数
-- 
maxData = 60 :: Int

frameTitle  = "E Speed,coolant T,ambient T,intakeAir T,fuel T,map Sensor,btVolt,throtle Pot,idle Swch,0B,p/n switch, CTS E,IATS E, FPC E, TPC E,0F,10,11,iACMP,iSDev,15,ignAd,coil T,19,1A,1B,lmdvt,clsdl,fuelt"
frameFmt    = "%5d,%3d,%3d,%3d,%3d,%3d,%6.2f,%6.2f,%c,%02X,%3d,%c,%c,%c,%c,%02X,%02X,%02X,%3d,%6d,%02X,%5.1f,%5.1f,%02X,%02X,%02X,%5d,%3d,%5d"


-- | グラフ描画用データセット 
graphData = [engspeed,tposition,mapsensor,battvoltage,coolanttemp]
engspeed    = GraphItem { name = "Engine Speed(rpm)",
  chr = '*',pwr = 0, minl = 0,   maxl = 24000,ul = 4000,ll = 700} 
tposition   = GraphItem { name = "Throttle Pot( V )",
  chr = 'T',pwr = 2, minl = 0,   maxl = 500,  ul = 300, ll = 0  }
mapsensor   = GraphItem { name = "MAP Sensor  (kPa)",
  chr = 'M',pwr = 0, minl = 0,   maxl = 110,  ul = 70,  ll = 20 }
battvoltage = GraphItem { name = "Battery Volt( V )",
  chr = 'V',pwr = 1, minl = 0,   maxl = 170,  ul = 150, ll = 100} 
coolanttemp = GraphItem { name = "Coolant Temp(\'C )",
  chr = 'C',pwr = 1, minl = -55, maxl = 120,  ul =98,   ll = -20}

-- | デバイス名が指定されなかった場合に使うパス名　
defaultUSBPathMac         = "/dev/tty.usbserial-DO01OV70" :: FilePath 
defaultUSBPathRaspberryPi = "/dev/ttyUSB0" :: FilePath
oldUSBPath                = "/dev/tty.usbserial-DJ00L8EZ" -- :: FilePath
alterntUSBPath            = "/dev/tty.usbserial-FT90HWC8" -- :: FilePath

initialState :: (BC.BChan Event,TBQueue ECU.UCommand) -> IO Status
initialState c = do
  t <- currentTime
  a <- System.Environment.getArgs
  e <- System.Environment.getEnv "HOME"
  let p = case a of
            []    ->  if e == "/Users/kuono" then defaultUSBPathMac
                                     else defaultUSBPathRaspberryPi
            [opt] -> opt   
            _     -> error "error: exactly one arguments needed."
  exist <- doesFileExist p
  return $  if not exist then
              Status { 
                rdat = DataSet {
                    evnt = (t,ECU.PortNotFound p)
                  , gdat = []
                  , note = "Checked Path" }
              , dset = []
              , cset = c
              , menu = basicMenu }
            else 
              Status {
                rdat = DataSet {
                    evnt = (t,ECU.OffLined)
                  , gdat = []
                  , note = "Port Found" }
              , dset = []
              , cset = c
              , menu = basicMenu
              }
--
frametoTable :: ECU.Frame -> String
frametoTable f = {-# SCC "frametoTable" #-}
    printf frameFmt
      (ECU.engineSpeed f ) -- ::Int
      (ECU.coolantTemp f ) -- ::Int
      (ECU.ambientTemp f ) -- ::Int
      (ECU.intakeATemp f ) -- ::Int
      (ECU.fuelTemp    f ) -- ::Int
      (ECU.mapSensor   f ) -- ::Int
      (ECU.battVoltage f ) -- ::Float
      (ECU.throttlePot f ) -- ::Float
      (tf (ECU.idleSwitch  f )) -- ::Bool ,-- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
      (ECU.unknown0B       f )  -- ::Word8,-- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
      (ECU.pnClosed    f ) -- 0x0C ::Park/neutral switch. Zero is closed, nonzero is open.
      (tf (ECU.faultCode1  f )) -- 0x0D * Bit 0: Coolant temp sensor fault (Code 1)
      (tf (ECU.faultCode2  f )) --      * Bit 1: Inlet air temp sensor fault (Code 2)
      (tf (ECU.faultCode10 f )) -- 0x0E * Bit 1: Fuel pump circuit fault (Code 10)
      (tf (ECU.faultCode16 f )) --      * Bit 7: Throttle pot circuit fault (Code 16)
      (ECU.unknown0F   f ) -- :: Word8, 0x0F
      (ECU.unknown10   f ) -- :: Word8, 0x10
      (ECU.unknown11   f ) -- :: Word8, 0x11
      (ECU.idleACMP    f ) -- :: Int  ,-- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
      (ECU.idleSpdDev  f ) -- :: Int  ,-- 0x13-14	Idle speed deviation (16 bits)
      (ECU.unknown15   f ) -- :: Word8, 0x15
      (ECU.ignitionAd  f ) -- :: Float,-- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
      (ECU.coilTime    f ) -- :: Float,-- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
      (ECU.unknown19   f ) -- :: Word8, 0x19
      (ECU.unknown1A   f ) -- :: Word8, 0x1A
      (ECU.unknown1B   f ) -- :: Word8, 0x1B
      (ECU.lambda_voltage f) -- :: Int
      (ECU.closed_loop'   f) -- :: Int
      (ECU.fuel_trim'     f) -- :: Int 
    where tf c = if c then 'T' else 'F'
  
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
-- | get dummy status with random data
getDummyStatus :: Status -> IO Status
getDummyStatus s = do
  t <- currentTime
  r <- ECU.dummyData807d
  let f = ECU.parse r
  return Status {
      rdat = DataSet {
        evnt = (t,ECU.Tick r),
        gdat = [
          (engspeed,    ECU.engineSpeed  f),
          (tposition,   ECU.ithrottlePot f),
          (mapsensor,   ECU.mapSensor    f),
          (battvoltage, ECU.ibattVoltage f),
          (coolanttemp, ECU.coolantTemp  f)
        ],
        note = "dummy"
      }
    , dset = rdat s : dset s
    , cset = cset s
    , menu = basicMenu
    }

currentTime :: IO LocalTime
currentTime = do
    timezone <- getCurrentTimeZone
    utcToLocalTime timezone <$> getCurrentTime
  
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

ratio:: (Real a) => a -> a -> (ECU.Frame -> a) -> Status -> Float 
ratio min max f s = case (d' <= min,d' >= max) of
  (True , _ )  -> 0.0
  (_ , True )  -> 1.0
  _            -> fromRational ((toRational d' - toRational min ) / (toRational max - toRational min))
  where d' = case snd . evnt $ rdat s of
               ECU.Tick r -> f $ ECU.parse r
               _          -> min

-- System.DispSpace.getAvailSpace :: FilePath -> IO Integer
-- ^ A convenience function that directly returns the diskAvail field from the result of getDiskUsage. If a large amount of data is to be written in a directory, calling this function for that directory can be used to determine whether the operation will fail because of insufficient disk space.
--   Disk usage information. All fields are in bytes.