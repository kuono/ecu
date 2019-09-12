{- |
Module      : ECU
Description : Ecu Communication Library for Rover Mini
Copyright   : (c) Kentaro UONO, 2018-2019
License     : N/A
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}

module ECU  ( Event(..), RData(..),Frame(..), EvContents(..), UCommand (..)
            , ECU.run,ECU.loop,parse,get807d
              -- moveIAC,readActPos,testAct,getIACPos,
              -- ptcRelayOn,ptcRelayOff,acRelayOn,acRelayOff,
              -- testInjectors,fireCoil,openIAC,closeIAC,
            , emptyD7d,emptyD80,emptyData807d
            , dummyData807d
) where

import System.Hardware.Serialport 
import System.Directory 
import System.IO -- for stdin, Buffering Mode
import System.Random
import qualified System.Timeout as TM
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.STM
--import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TBQueue
import qualified Data.ByteString   as BS
import Data.Typeable
import Data.Char
import Data.Word
import Data.List.Split
import Data.Bits
-- import Data.Bits.Extras
import Data.Fixed
import qualified Data.Vector as V
import Numeric
import qualified System.Posix.Unistd as U
import Data.Time.LocalTime
import Data.Time.Clock
import Text.Printf
import qualified Brick.BChan as BC

--
-- definitions for export
--
-- data EngineStatus = Stoped | Running | NA deriving Show
type Data807d   = BS.ByteString
type Event      = (LocalTime, EvContents)
data EvContents = PortNotFound FilePath | Connected FilePath | OffLined | Tick RData | Done | Error String
-- | MEMS Commands
data UCommand = Disconnect | Init | Get807d | ClearFaults | RunFuelPump | StopFuelPump 
              | GetIACPos | SetIACPos | TestActuator deriving (Eq,Show)
type RData    = Data807d
-- | Ecu model and its identical data
data ModelDataSet = ModelDataSet { name :: !String, d8size :: !Int, d7size :: !Int}
-- instance Show ModelDataSet where show  = show . name 
type Models = [(BS.ByteString , ModelDataSet)]
data ActuatorCmdDataSet = DefMinMax { def:: !Int,min:: !Int,max:: !Int} -- ^ (Default value, Minimum, Maximum)
fuelTrimDftMinMax  = DefMinMax 0x8a 0x00 0xfe  -- for MNE101170
idleDecayDftMinMax = DefMinMax 0x23 0x0a 0x3c  -- for MNE101170
idleSpeedDftMinMax = DefMinMax 0x80 0x78 0x88  -- for MNE101170
ignAdvOffDftMinMax = DefMinMax 0x80 0x74 0x8c  -- for MNE101170
-- | MEMS Monad related
data Env  = Env 
    { path  :: !FilePath
    , port  :: !SerialPort
    , model :: !ModelDataSet
    , dch   :: BC.BChan Event   -- ^ channel to inject original events for brick
    , cch   :: TBQueue UCommand -- ^ inlet channel to get user command for mems
    }
type MEMS = ReaderT Env IO 
--
run ::  MEMS a -> (FilePath,BC.BChan Event,TBQueue UCommand) -> IO ()
run c r = Ex.bracket {- :: IO a	-> (a -> IO b) -> (a -> IO c)	-> IO c	 -}
  (ECU.init r)
  (\env -> case env of 
     Nothing -> return () -- fail "Some error occured while running ecu."
     Just e' -> closeSerial $ port e' )
  (\env -> case env of
     Nothing -> return () -- fail "Initialization failed."
     Just e' -> do { runReaderT c e' ; return () } )
--
loop :: MEMS ()
loop = do
    e <- ask
    let inlet  = cch e
        outlet = dch e
    t <- lift currentTime
    c <- lift $ atomically $ tryReadTBQueue inlet
    s <- currentStage
    if s `accept` c
        then do r <- res c ; lift $ BC.writeBChan outlet (t,r) ; loop
        else do
            lift $ BC.writeBChan outlet (t,res' c) 
            unless (c == Just Disconnect) loop
--
data Stage = Disconnected | Communicating
currentStage :: MEMS Stage
currentStage = do
    e <- ask
    let f = path e
    exist <- lift $ doesFileExist f
    return Communicating -- because we are still in debug mode
    -- return $ if exist then Connected else Disconnected 
--
accept :: Stage -> Maybe UCommand -> Bool
accept Disconnected  c = (c == Just Init)
accept Communicating c = (c /= Just Init)
-- | User Command achievement
res :: Maybe UCommand -> MEMS EvContents
res Nothing             = get807d
res (Just Disconnect)   = offline
res (Just ClearFaults)  = clearFaults
res (Just RunFuelPump)  = runFuelPump
res (Just StopFuelPump) = stopFuelPump
--
res' :: Maybe UCommand -> EvContents
res' c = Error (show c) 
init ::(FilePath,BC.BChan Event,TBQueue UCommand) -> IO (Maybe Env)
init (f,dc,cc)= do 
    exist <- doesFileExist f
    j <- currentTime
    if not exist 
        then do
            BC.writeBChan dc (j,PortNotFound f)
            return Nothing
        else do
            sp <- openSerial f defaultSerialSettings {timeout= 1, flowControl = Software }
            send sp $ BS.singleton 0xca  -- 202 'ha no hankaku' 
            send sp $ BS.singleton 0x75  -- 117 'u' 
            send sp $ BS.singleton $ fst htbt  -- 244 f4 -> f4 00
            recv sp 1
            send sp $ BS.singleton 0xd0  -- 208 'mi no hankaku'
            m <- tryRecv4Bytes sp -- ^ recv p 4 だと取りこぼす（（0.5秒間位の連続リレー音）
            case ( m == BS.empty , BS.length m /= 4 ) of
                (True, _)  -> return Nothing
                (_,True )  -> return Nothing
                _          -> case lookup m models of
                    Just m' -> return $ Just Env { path = f, port = sp, model = m' , dch = dc , cch = cc } 
                    _       -> return Nothing
  where
    -- | read 4 byets from ecu
    tryRecv4Bytes :: SerialPort -> IO BS.ByteString
    tryRecv4Bytes = tryRecv1Byte 4
    -- | repeat n times to read 1 byte from ecu
    tryRecv1Byte :: Int -> SerialPort -> IO BS.ByteString
    tryRecv1Byte n pt
        | n <= 0    = return BS.empty
        | otherwise = do
            r <- recv pt 1
            if r /= BS.empty then return r else do { threadDelay 1000; tryRecv1Byte (n-1) pt }
--
get807d :: MEMS EvContents
get807d =  do
  r8 <- sndCmd80
  r7 <- sndCmd7d
  return $ Tick $ BS.concat [r8,r7]

data Frame = Frame
  { d80size     :: !Int
  , engineSpeed :: !Int   -- 0x01-2	Engine speed in RPM (16 bits)
  , coolantTemp :: !Int   -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
  , ambientTemp :: !Int   -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
  , intakeATemp :: !Int   -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
  , fuelTemp    :: !Int   -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
  , mapSensor   :: !Int   -- 0x07	MAP sensor value in kilopascals
  , battVoltage :: !Float -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
  , ibattVoltage:: !Int
  , throttlePot :: !Float -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
  , ithrottlePot:: !Int
  , idleSwitch  :: !Bool  -- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
  , unknown0B   :: !Word8 -- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
  , pnClosed    :: !Int   -- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
                         -- Fault codes. On the Mini SPi, only two bits in this location are checked:             
  , faultCode1  :: !Bool  -- 0x0D  * Bit 0: Coolant temp sensor fault (Code 1)
  , faultCode2  :: !Bool  --       * Bit 1: Inlet air temp sensor fault (Code 2)
  , faultCode10 :: !Bool  -- 0x0E  * Bit 1: Fuel pump circuit fault (Code 10)
  , faultCode16 :: !Bool  --       * Bit 7: Throttle pot circuit fault (Code 16)
  , unknown0F   :: !Word8 -- 0x0F	Unknown
  , unknown10   :: !Word8 -- 0x10	Unknown
  , unknown11   :: !Word8 -- 0x11	Unknown
  , idleACMP    :: !Int   -- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
  , idleSpdDev  :: !Int   -- 0x13-14	Idle speed deviation (16 bits)
  , unknown15   :: !Word8 -- 0x15	Unknown
  , ignitionAd  :: !Float   -- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
  , coilTime    :: !Float   -- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
  , unknown19   :: !Word8  -- 0x19	Unknown
  , unknown1A   :: !Word8  -- 0x1A	Unknown
  , unknown1B   :: !Word8  -- 0x1B	Unknown
  , d7dsize     :: !Int
  , lambda_voltage:: !Int  -- This lambda value is a calculated value (if it is the same as the British emissions test).     And a value of, say, 1.05, suggests it is 5% too lean.   But, if your oxygen (and CO and HC) readings are all good, then it suggests your high lambda reading is because of a leak in the exhaust wgich pulls in fresh air (and oxygen).     You could try starting your car when it is cold and put your hand over the exhaust pipe and look underneath to see if water is leaking from any if the joints. 
  , closed_loop'  :: !Int  -- 0 : Open Loop, others : Closed Loop  
  , fuel_trim'    :: !Int  
  }
--
-- internal library
--
-- | ECU Commands
type Command  = (Word8,String) -- ^ ECU returns echo and one result byte. Command byte (send to ECU), Num of Response following bytes from ECU
type Command' = (Word8,String) -- ^ ECU returns only echo byte.
opnfp = (0x01,"Open Fuel Pump relay =stop"):: Command -- Open fuel pump relay (stop fuel pump) 
opnpr = (0x02,"Open PTC Relay")            :: Command -- Open PTC relay (inlet manifold heater)
opnac = (0x03,"Open A/C Relay")            :: Command -- Open air conditioning relay 
clspv = (0x08,"Close purge valve?")        :: Command -- Close purge valve ?
opnO2 = (0x09,"Open O2 heater relay?")     :: Command -- Open O2 heater relay ?
clsfp = (0x11,"Close Fuel Pump relay =run"):: Command -- Close fuel pump relay (run fuel pump)
clspr = (0x12,"Close PTC Relay")           :: Command -- Close PTC Relay (inlet manifold heater)
clsac = (0x13,"Close A/C Relay")           :: Command -- Close air conditioning relay
opnpv = (0x18,"Open purge valve?")         :: Command -- Open purge valve ?)
clsO2 = (0x19,"Close O2 heater relay ?")   :: Command -- Close O2 heater relay ?
clsf1 = (0x1d,"Close Fan 1 relay?")        :: Command' -- Close Fan 1 relay ? 
clsf2 = (0x1e,"Close Fan 2 relay?")        :: Command' -- Close Fan 2 relay ?
icrft = (0x79,"Increment Fuel Trim")       :: Command -- Increments fuel trim setting and returns the current value
dcrft = (0x7a,"Decrement Fuel Trim")       :: Command -- Decrements fuel trim setting and returns the current value
icrft'= (0x7b,"Increment Fuel Trim-2")     :: Command -- Increments fuel trim setting and returns the current value
dcrft'= (0x7c,"Decrement Fuel Trim-2")     :: Command -- Decrements fuel trim setting and returns the current value 
req7d = (0x7d,"Request data frame/7D")     :: Command -- get data for frame7d - followed by 32-byte data frame; 125
req80 = (0x80,"Request data frame/80")     :: Command -- get data for frame80 - followed by 28-byte data frame; 128
incid = (0x89,"Increments idle decay")     :: Command -- Increments idle decay setting and returns the current value
decid = (0x8a,"Decrements idle decay")     :: Command -- Decrements idle decay setting and returns the current value
incis = (0x91,"Increments idle speed")     :: Command -- Increments idle speed setting and returns the current value
decil = (0x92,"Decrements idle speed")     :: Command -- Decrements idle speed setting and returns the current value
incia = (0x93,"Increments ignition ad")    :: Command -- Increments ignition advance offset and returns the current value
decia = (0x94,"Decrements ignition ad")    :: Command -- Decrements ignition advance offset and returns the current value
clrft = (0xcc,"Clear fault code")          :: Command -- 204, Clear fault codes	CC 00
htbt  = (0xf4,"NOP/heartbeat?")            :: Command -- 0xf4 244 NOP / heartbeat? Sent continuously by handheld diagnostic tools to verify serial link.
actfi = (0xf7,"Actuate fuel incejtor")     :: Command -- F7 03 (SPI?)
figcl = (0xf8,"Fire ignition coil")        :: Command -- F8 02 
reqip = (0xfb,"Request IAC position")      :: Command -- FB xx where second byte represents the IAC position
opiac = (0xfd,"Open IAC one and get pos")  :: Command -- FD xx, where the second byte represents the IAC position
cliac = (0xfe,"Close IAC one and get pos") :: Command
rqiac = (0xff,"Request current IAC pos?")  :: Command
--
-- Actuator Command
-- 
-- From [Bearinghead.com](http://www.bearinghead.com/car_stuff/mems_interface/)
--   Although some of the actuators have pairs of on/off commands to drive them,
--   I've found that the system fitted to the Mini SPi will actually turn off the
--   AC relay, PTC relay, and fuel pump relay automatically after a short time 
--   (i.e. without requiring the 'off' command). The 'off' command is acknowledged
--   by the ECU, but apparently has no effect.
fuelPumpOn     = (0x11,"Fuel Pump on")   :: Command -- 11 00
fuelPumpOff    = (0x01,"Fuel Pump off")  :: Command -- 01 00
ptcRelayOn     = (0x12,"ptc Relay on")   :: Command -- 12 00
ptcRelayOff    = (0x02,"ptc Relay off")  :: Command -- 02 00
acRelayOn      = (0x13,"a/c Relay on")   :: Command -- 13 00
acRelayOff     = (0x03,"a/c Relay off")  :: Command -- 03 00
testInjectors  = (0xf7,"test injectors") :: Command -- f7 03 (SPi ?)
fireCoil       = (0xf8,"fire coil")      :: Command -- f8 02
openIac        = (0xfd,"open IAC")       :: Command -- fd xx where the second byte represents the IAC position
closeIac       = (0xfe,"close IAC")      :: Command -- fe xx where the second byte represents the IAC position
-- cfd = 0xcc ::Word8  -- 204
sndCmd80 :: MEMS BS.ByteString
sndCmd80 = do
  e <- ask
  let p = port e
  sendCommandAndGet1Byte p $ fst req80
--
sndCmd7d :: MEMS BS.ByteString
sndCmd7d = do
  e <- ask
  let p = port e
  sendCommandAndGet1Byte p $ fst req7d
--
offline :: MEMS EvContents
offline = do
    e <- ask
    let p = port e
    lift $ closeSerial p
    return OffLined
--
clearFaults :: MEMS EvContents
clearFaults = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst clrft
  return $ if r /= BS.empty then Done else Error "Some error occured for clear faults."
--
runFuelPump :: MEMS EvContents
runFuelPump = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst clsfp
  return $ if r /= BS.empty then Done else Error "Some error occured for Opening Fuel Pump Relay (Run Fuel Pump)."
--
stopFuelPump :: MEMS EvContents
stopFuelPump = do
  e <- ask
  let p = port e
  r <- sendCommandAndGet1Byte p $ fst opnfp
  return $ if r /= BS.empty then Done else Error "Some error occured for Opening Fuel Pump Relay (Stop Fuel Pump)."
--
currentTime :: IO LocalTime
currentTime = do
  zone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ utcToLocalTime zone time
-- | dummy frame data
dummyData807d :: IO Data807d
dummyData807d = do
  g <- newStdGen
  let r = randoms g 
      r1 = take 27 r 
      r2 = take 31 $ drop 28 r 
  return $ BS.pack $ 28:r1 ++ 32:r2-- random807d :: IO ECU.Data807d
dummyFrameData :: IO Frame
dummyFrameData = do
  g <- newStdGen
  let r = randoms g 
      r1 = take 27 r 
      r2 = take 31 $ drop 28 r 
  return $ parse $ BS.pack $ 28:r1 ++ 32:r2-- random807d :: IO ECU.Data807d
-- random807d = do
--   t <- currentTime
--   g <- newStdGen
--   let r = randoms g 
--       r1 = take 27 r 
--       r2 = take 31 $ drop 28 r 
--   return $ BS.pack $ 28:r1 ++ 32:r2
--
-- constants
--
emptyFrame = Frame 
  { d80size     = 28, d7dsize = 32, engineSpeed = 0 , coolantTemp = 0 , ambientTemp = 0 , intakeATemp = 0
  , fuelTemp    = 0 , mapSensor   = 0 , battVoltage = 0.0 , ibattVoltage = 0
  , throttlePot = 0.0 , ithrottlePot = 0 , idleSwitch  = False
  , unknown0B   = 0 , pnClosed    = 0 , faultCode1  = False , faultCode2  = False
  , faultCode10 = False , faultCode16 = False , unknown0F   = 0 , unknown10   = 0
  , unknown11   = 0 , idleACMP    = 0 , idleSpdDev  = 0 , unknown15   = 0 , ignitionAd  = 0.0
  , coilTime    = 0.0 , unknown19   = 0 , unknown1A   = 0 , unknown1B   = 0
  , lambda_voltage = 0 , closed_loop'   = 0 , fuel_trim'     = 0
  }
-- mdata      = show . BS.unpack . mdb -- mapM (printf " %02X") . BS.unpack . mdb 
models = [ mneUnknown, mneAuto, mne10078, mne101070, mne101170 ]
mneUnknown = (BS.empty, 
              ModelDataSet { name = "unknown                      ", d8size = 28 , d7size = 14 })
mneAuto    = (BS.pack [0x3a, 0x00, 0x02, 0x14] {- 58,0,2,20 -}    ,
              ModelDataSet { name = "Japanese Automatic           ", d8size = 28 , d7size = 14 })
mne10078   = (BS.pack [0x39, 0x00, 0x00, 0x5c] {-  57, 0, 0, 92 -},
              ModelDataSet { name = "MNE10078  M/SPI Japan Cooper ", d8size = 28 , d7size = 14 })
mne101070  = (BS.pack [0x99, 0x00, 0x02, 0x03] {- 153, 0, 2,  3 -}, 
              ModelDataSet { name = "MNE101070 M/SPI Cooper       ", d8size = 28 , d7size = 32 })
mne101170  = (BS.pack [0x99, 0x00, 0x03, 0x03] {- 153, 0, 3,  3 -},
              ModelDataSet { name = "MNE101170 M/SPI Except Cooper", d8size = 28 , d7size = 32 })
    -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx
--
-- library functions
--
parse :: Data807d -> Frame
parse d =  {-# SCC "parse" #-} 
  -- if d == BS.empty || ( (toInteger (BS.length d) ) /= ( (toInteger $ BS.index d 0 ) ) ) then
  --   emptyFrame -- 時々取りこぼしが発生するようなので、フィルタリング
  -- else 
  let d8l = fromIntegral (BS.index d 0)
      d7l = fromIntegral (BS.head (BS.drop d8l d))
      d8 = BS.take d8l d
      d7 = BS.drop d8l d
      iv = fromIntegral ( BS.index d8 8 )
      it = 2 * fromIntegral ( BS.index d8 9 )
  in  Frame
      { d80size     = d8l
      , d7dsize     = d7l
      , engineSpeed = 256 * fromIntegral (BS.index d8 1) + fromIntegral (BS.index d 2)
      , coolantTemp = -55 + fromIntegral (BS.index d8 3)  -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
      , ambientTemp = -55 + fromIntegral (BS.index d8 4)  -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
      , intakeATemp = -55 + fromIntegral (BS.index d8 5)  -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
      , fuelTemp    = -55 + fromIntegral (BS.index d8 6)  -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
      , mapSensor   = fromIntegral ( BS.index d8 7 )      -- 0x07	MAP sensor value in kilopascals
      , ibattVoltage = iv 
      , battVoltage  = 0.1 * fromIntegral iv           -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
      , ithrottlePot = it
      , throttlePot  = 0.01 * fromIntegral it    -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
      , idleSwitch  = testBit (BS.index d8 10) 4   -- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
      , unknown0B   = BS.index d8 11               -- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
      , pnClosed    = fromIntegral ( BS.index d8 12 ) -- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
      , faultCode1  = testBit (BS.index d8 13) 0   -- Coolant temp sensor
      , faultCode2  = testBit (BS.index d8 13) 1   -- Air temp sensor 
      , faultCode10 = testBit (BS.index d8 14) 1   -- Fules pump cirkit
      , faultCode16 = testBit (BS.index d8 14) 7   -- Throttle position sensor
      , unknown0F   = BS.index d8 15               -- 0x0F	Unknown
      , unknown10   = BS.index d8 16               -- 0x10	Unknown
      , unknown11   = BS.index d8 17               -- 0x11	Unknown
      , idleACMP    = fromIntegral ( BS.index d8 18 ) -- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
      , idleSpdDev  = 256 * fromIntegral (BS.index d8 19) + fromIntegral (BS.index d 20)  -- 0x13-14	Idle speed deviation (16 bits)
      , unknown15   = BS.index d8 21               -- 0x15	Unknown
      , ignitionAd  = -24.0 + 0.5 * fromIntegral  (BS.index d8 22)  -- 0x16	Ignition  0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
      , coilTime    = 0.02 * ( 256 * fromIntegral  (BS.index d8 23) + fromIntegral ( BS.index d 24 ) ) -- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
      , unknown19   = BS.index d8 25               -- 0x19	Unknown
      , unknown1A   = BS.index d8 26               -- 0x1A	Unknown
      , unknown1B   = BS.index d8 27               -- 0x1B	Unknown
      , lambda_voltage = 5 * fromIntegral  ( BS.index d7 0x06 )
      , closed_loop'   = fromIntegral  ( BS.index d7 0x0a )
      , fuel_trim'     = fromIntegral  ( BS.index d7 0x0c )
      } 
-- -- |Receive bytes, given the maximum number
-- recv :: SerialPort -> Int -> IO B.ByteString
-- -- |Send bytes
-- send :: SerialPort -> B.ByteString -> IO Int  -- ^ return Number of bytes actually sent
-- | an action to get 1 byte ECU response as a result of an ECU command. 
sendCommandAndGet1Byte :: SerialPort -> Word8 -> MEMS BS.ByteString
sendCommandAndGet1Byte p c = do
  s <- lift $ send p $ BS.singleton c
  if s == 0 then
    fail $ "Command ( " ++ show c ++ " ) was not received."
  else
    do
      r <- lift $ tryIO 5 $ recv p 1
      if r == BS.empty then
        fail $ "ECU did not responsed while command ( " ++ show c ++ " ) was sent."
      else 
        pure r
-- | an action to try (IO BS.ByteString) Int times and returns BS.empty when some error occured. 
tryIO ::    Int              -- ^ number of times to try to do the action 
         -> IO BS.ByteString -- ^ the action which returns BS.empty when error occured
         -> IO BS.ByteString 
tryIO n a -- try n times action
  | n <= 0    = return BS.empty
  | otherwise = do
      r <- a 
      if r == BS.empty then do
        threadDelay 10000
        tryIO (n-1) a 
      else
        return r
--
-- Lirary
--
emptyD80 = BS.pack [0x1c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 28バイト
emptyD7d = BS.pack [0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 32バイト
emptyData807d = BS.concat [ emptyD80, emptyD7d ]

-- data Loop        = OpenLoop | ClosedLoop deriving (Show)

-- -- -------------------------
-- -- -- | Logger  
-- -- -------------------------

-- frameTitle  = "E Speed,coolant T,ambient T,intakeAir T,fuel T,map Sensor,btVolt,throtle Pot,idle Swch,0B,p/n switch, CTS E,IATS E, FPC E, TPC E,0F,10,11,iACMP,iSDev,15,ignAd,coil T,19,1A,1B,lmdvt,clsdl,fuelt"
-- frameFmt    = "%5d,%3d,%3d,%3d,%3d,%3d,%6.2f,%6.2f,%c,%02X,%3d,%c,%c,%c,%c,%02X,%02X,%02X,%3d,%6d,%02X,%5.1f,%5.1f,%02X,%02X,%02X,%5d,%3d,%5d"

-- logger :: Chan Model -> IO()
-- logger modelCh = do
--   logfn <- logFileName :: IO FilePath
--   withFile logfn WriteMode
--     $ \logfh -> do
--         hPutStrLn logfh  $ "Date,Time," ++ frameTitle
--         forever $ do
--           d' <- readChan modelCh
--           let d = dat d'
--           jikoku <- currentTime
--           case status d of
--             GotData d807d    -> 
--               let j  = localTimetoString $ at d
--                   l  = frametoTable $ parse d807d
--               in hPutStrLn logfh  $ j ++ "," ++ l
--             Connected model  -> hPutStrLn logfh $ "Connected" ++ ',': show model ++ ",at" ++ show jikoku
--             CommandIssued m  -> hPutStrLn logfh $ "CMD:" ++ show m ++ " at " ++ show jikoku
--             DataError msg    -> hPutStrLn logfh  msg
--             NotConnected msg -> hPutStrLn logfh  msg
--     `Ex.catches`
--       [ Ex.Handler (\e -> case (e::UserCommand) of 
--           Quit -> do
--             ima <- currentTime
--             hPutStrLn logfh $ "At " ++ show ima ++ " , logger terminated because of quit msg."
--             hFlush logfh
--             hClose logfh
--           Reconnect -> do
--             ima <- currentTime
--             hPutStrLn logfh $ "At " ++ show ima ++ " , try to reconnect ECU."
--             hFlush logfh
--             hClose logfh
--             logger modelCh
--           _  -> Ex.throwIO e)
--       , Ex.Handler (\e -> case (e::Ex.IOException) of
--           _  -> Ex.throwIO e -- print e 
--           )
--       ]

-- frametoTable :: Frame -> String
-- frametoTable f = {-# SCC "frametoTable" #-}
--   printf frameFmt
--     (engineSpeed f ) -- ::Int
--     (coolantTemp f ) -- ::Int
--     (ambientTemp f ) -- ::Int
--     (intakeATemp f ) -- ::Int
--     (fuelTemp    f ) -- ::Int
--     (mapSensor   f ) -- ::Int
--     (battVoltage f ) -- ::Float
--     (throttlePot f ) -- ::Float
--     (tf (idleSwitch  f )) -- ::Bool ,-- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
--     (unknown0B       f )  -- ::Word8,-- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
--     (pnClosed    f ) -- 0x0C ::Park/neutral switch. Zero is closed, nonzero is open.
--     (tf (faultCode1  f )) -- 0x0D * Bit 0: Coolant temp sensor fault (Code 1)
--     (tf (faultCode2  f )) --      * Bit 1: Inlet air temp sensor fault (Code 2)
--     (tf (faultCode10 f )) -- 0x0E * Bit 1: Fuel pump circuit fault (Code 10)
--     (tf (faultCode16 f )) --      * Bit 7: Throttle pot circuit fault (Code 16)
--     (unknown0F   f ) -- :: Word8, 0x0F
--     (unknown10   f ) -- :: Word8, 0x10
--     (unknown11   f ) -- :: Word8, 0x11
--     (idleACMP    f ) -- :: Int  ,-- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
--     (idleSpdDev  f ) -- :: Int  ,-- 0x13-14	Idle speed deviation (16 bits)
--     (unknown15   f ) -- :: Word8, 0x15
--     (ignitionAd  f ) -- :: Float,-- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
--     (coilTime    f ) -- :: Float,-- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
--     (unknown19   f ) -- :: Word8, 0x19
--     (unknown1A   f ) -- :: Word8, 0x1A
--     (unknown1B   f ) -- :: Word8, 0x1B
--     (lambda_voltage f) -- :: Int
--     (closed_loop'   f) -- :: Int
--     (fuel_trim'     f) -- :: Int 
--   where tf c = if c then 'T' else 'F'
      

-- -- -- | ログファイル名の自動生成関数
-- -- -- 
-- -- -- 呼び出された時刻に応じ、以下のような形式のファイル名を生成する：ECULog2018-10-15_17.27.26.csv

-- -- --
-- -- Utility functions
-- -- 
-- toInt = fromIntegral . toInteger

-- -- | only for ecu commands
-- size::BS.ByteString -> Int
-- size = fromIntegral . BS.head

-- currentTime :: IO LocalTime
-- currentTime = do
--     timezone <- getCurrentTimeZone
--     utcToLocalTime timezone <$> getCurrentTime
  
-- logFileName :: IO FilePath
-- logFileName = do
--     time <- currentTime :: IO LocalTime
--     return $ localtimeToFilePath time
--     where localtimeToFilePath (LocalTime n t) =  -- to convert constant length string
--               let hi     = show n
--                   ji     = todHour t
--                   hun    = todMin  t 
--                   byo    = todSec  t
--                   byo'   = take 2 $ if byo >= 10 then showFixed False byo
--                                     else '0':showFixed False byo
--               in printf "ECULog%10s_%02d.%02d.%2s.csv" hi ji hun byo' -- ex. ECULog2018-10-15_17.27.26.csv
--               --  ./log/ECU...としていたが，ディレクトリが存在していないとランタイムエラーを起こすので変更
-- -- | to convert constant length string
-- localTimetoString :: LocalTime -> String
-- localTimetoString (LocalTime n t) = 
--     let hi     = show    n
--         ji     = todHour t
--         hun    = todMin  t
--         byo    = todSec  t
--         byo'   = take 9 $ if byo >= 10 then showFixed False byo
--                               else '0':showFixed False byo 
--     in printf "%10s,%02d:%02d:%5s " hi ji hun byo'