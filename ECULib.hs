{- |
Module      : ECULib
Description : Ecu Communication Library for Rover Mini
Copyright   : (c) Kentaro UONO, 2018-2019
License     : BSD3
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}

-- {-# LANGUAGE BangPatterns #-}
module ECULib  ( runEcuAt, ECUResponse ) where

import System.Hardware.Serialport 
import System.Directory
import System.IO -- for stdin, Buffering Mode
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Data.ByteString   as BS
import Data.Typeable
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Char
import Data.Word
import Data.List.Split
import Data.Bits
import Data.Fixed
import Text.Printf
import Numeric
import qualified System.Posix.Unistd as U

---------------------------
-- type definition       --
---------------------------
data    Environment   = Env { port :: SerialPort, modl :: ModelData }
newtype ECUResponse a = ECUResponse { ioEither :: IO ( Either String a  ) }
instance Functor ECUResponse where
  -- fmap::( a -> b ) -> ECUResponse a -> ECUResponse b
  fmap f action = ECUResponse $ do
    x <- ioEither action
    case x of 
      Left  l -> pure $ Left  l
      Right r -> pure $ Right $ f r
instance Applicative ECUResponse where  
  -- pure::a -> ECUResponse a
  pure r = ECUResponse (pure (Right r))
  -- (<*>)::ECUResponse (a->b) -> ECUResponse a -> ECUResponse b
  lf <*> re = ECUResponse $ do
    func <- ioEither lf
    case func of
      Left  l     -> pure $ Left l
      Right func' -> do
        r <- ioEither re
        case r of 
          Left  l' -> pure $ Left  l'
          Right r' -> pure $ Right (func' r')
instance Monad ECUResponse where
  -- (>>=)::ECUResponse a -> (a->ECUResponse b)->ECUResponse b
  fail l    = ECUResponse $ pure (Left  l)
  le >>= rf = ECUResponse $ do
    e <- ioEither le
    case e of
      Left  l -> pure $ Left l
      Right r -> ioEither (rf r)

data Frame80  = Frame80 {
                      size_80     :: Int , -- 0x00	Size of data frame, including this byte. This should be 0x1C (28 bytes) for the frame described here.
                      engineSpeed :: Int , -- 0x01-2	Engine speed in RPM (16 bits)
                      coolantTemp :: Int , -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
                      ambientTemp :: Int , -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
                      intakeATemp :: Int , -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
                      fuelTemp    :: Int , -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
                      mapSensor   :: Int , -- 0x07	MAP sensor value in kilopascals
                      battVoltage :: Float , -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
                      throttlePot :: Float , -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
                      idleSwitch  :: Bool ,-- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
                      unknown0B   :: Word8,-- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
                      pnClosed    :: Bool ,-- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
                      -- Fault codes. On the Mini SPi, only two bits in this location are checked:             
                      faultCode1  :: Bool ,-- 0x0D  * Bit 0: Coolant temp sensor fault (Code 1)
                      faultCode2  :: Bool ,--       * Bit 1: Inlet air temp sensor fault (Code 2)
                      faultCode10 :: Bool ,-- 0x0E  * Bit 1: Fuel pump circuit fault (Code 10)
                      faultCode16 :: Bool ,--       * Bit 7: Throttle pot circuit fault (Code 16)
                      unknown0F   :: Word8,-- 0x0F	Unknown
                      unknown10   :: Word8,-- 0x10	Unknown
                      unknown11   :: Word8,-- 0x11	Unknown
                      idleACMP    :: Int  ,-- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
                      idleSpdDev  :: Int  ,-- 0x13-14	Idle speed deviation (16 bits)
                      unknown15   :: Word8,-- 0x15	Unknown
                      ignitionAd  :: Float,-- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
                      coilTime    :: Float,-- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
                      unknown19   :: Word8,-- 0x19	Unknown
                      unknown1A   :: Word8,-- 0x1A	Unknown
                      unknown1B   :: Word8 -- 0x1B	Unknown
    } deriving Show
    -- thePseduData80 = BS.pack [0x1c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
    
data Frame7d  = Frame7d {                -- Byte position, Field
  size_7d       :: Int,  -- 0x00, size of data frame, including this byte
  lambda_voltage:: Int,  -- This lambda value is a calculated value (if it is the same as the British emissions test).     And a value of, say, 1.05, suggests it is 5% too lean.   But, if your oxygen (and CO and HC) readings are all good, then it suggests your high lambda reading is because of a leak in the exhaust wgich pulls in fresh air (and oxygen).     You could try starting your car when it is cold and put your hand over the exhaust pipe and look underneath to see if water is leaking from any if the joints. 
  closed_loop'  :: Int,  
  fuel_trim'    :: Int 
  -- idle_base_pos :: Int
  } deriving Show
    
type Data80      = BS.ByteString
type Data7d      = BS.ByteString

data ECUCommand     = MoveIAC Int | TestActuator ActCmd | ClearFaults deriving (Show, Typeable)
type ActCmd = BS.ByteString
data UserCommand = Reconnect | ECUCommand { command::ECUCommand} | Quit deriving (Show, Typeable) 
instance Ex.Exception UserCommand
      
data ECUData     = ECUData { status :: ECUStatus, at :: LocalTime } 
instance Show ECUData where
  show d = case status d of 
    Connected     model   -> show ( at d ) ++ show model 
    CommandIssued st      -> show ( at d ) ++ show st
    GotData       dt      -> show ( at d ) ++ show dt
    NotConnected  message -> show ( at d ) ++ message

data ECUStatus   = Connected ModelData | CommandIssued String | GotData ECUData807d | DataError String | NotConnected String 
instance Show ECUStatus where
  show d = case d of 
    Connected    mdl -> show mdl
    CommandIssued st -> show st
    NotConnected msg -> show msg
data ECUData807d = ECUData807d { d80 :: Data80 , d7d :: Data7d }
instance Show ECUData807d where
  show d = show (d80 d) ++  show (d7d d)  
data Model  = Model { num  :: Integer    -- ^  num : 直前までの累積データ数, pdat: 直前のデータ, 
                    , dat  :: ECUData
                    , log  :: [String]    -- ^ log  : 各イベントのログ
                    } 

initModel      =  do
  ima <- currentTime
  return $ Model { num = 0
                 , dat = ECUData { status = NotConnected "Not initialized." , at = ima }
                 , ECULib.log = [""] }
    
type Res           = (BS.ByteString,LocalTime) 
type SetCmd        = Word8
type SData         = BS.ByteString
type SDataSet      = (SetCmd, SData)

type DataCmd       = Word8
type ActuatorCmd   = Word8
reqData7d      = 0x7d :: DataCmd -- get data for frame80 - 128
reqData80      = 0x80 :: DataCmd -- get data for frame7d - 125
clearFaults    = 0xcc :: DataCmd -- 204, Clear fault codes	CC 00
heartBeat      = 0xf4 :: DataCmd -- 0xf4 244
getIACPosition = 0xfb :: DataCmd 

fuelPumpOn     = 0x11 :: ActuatorCmd
fuelPumpOff    = 0x01 :: ActuatorCmd
ptcRelayOn     = 0x12 :: ActuatorCmd
ptcRelayOff    = 0x02 :: ActuatorCmd
acRelayOn      = 0x13 :: ActuatorCmd
acRelayOff     = 0x03 :: ActuatorCmd
testInjectiors = 0xf7 :: ActuatorCmd
fireCoil       = 0xf8 :: ActuatorCmd
openIAC        = 0xfd :: ActuatorCmd
closeIAC       = 0xfe :: ActuatorCmd
-- cfd = 0xcc ::Word8  -- 204

-- | 主関数
runEcuAt :: String  -- ^ ECUにつながったシリアルポート（USB経由で可）のパス名 
         -> IO ()
runEcuAt path = do 
  exist <- doesFileExist path
  if not exist then
    do { Prelude.putStrLn "Device does not exist. Check USB plug and/or serial cable connection." ; return () } 
  else
    do
      datach  <- newChan :: IO (Chan Model) -- outlet chanel for sending the Model
      datach' <- dupChan datach
      lid <- forkIO $ logger    datach  --  fork logger thread 
      did <- forkIO $ displayer datach' --  fork displayer thread
      cid <- forkIO $ communicateWith path datach --  fork communication thread

      hSetBuffering stdin NoBuffering -- set non buffering mode 
      hSetEcho      stdin False

      loopECUwith (lid, did, cid, datach )     --  event driven style main commander loop as follows
  `Ex.catch` \e -> case (e::UserCommand) of
    Quit       -> return ()
    Reconnect  -> runEcuAt path
    otherwise  -> Ex.throw e
  where
        loopECUwith  (lid, did, cid, datach ) = 
          forever $ do -- 引数でのデータの引き回しは早急になくしたい
            ch <- getChar
            case ch of
              '\ESC'    -> do { Ex.throwTo cid Quit ; Ex.throwTo lid (Quit) ; Ex.throwTo did Quit ; Ex.throw Quit }
              'q'       -> do { Ex.throwTo cid Quit ; Ex.throwTo lid (Quit) ; Ex.throwTo did Quit ; Ex.throw Quit }
              '\''      -> do { Ex.throwTo cid Quit ; Ex.throwTo lid (Quit) ; Ex.throwTo did Quit ; Ex.throw Quit }
              '0'       -> do { Ex.throwTo cid $ ECUCommand { command = ClearFaults } }
              -- quit command issued. single quote key on dvorak layout is located at 'q' location of qwerty keyboard. 
              ' '       -> do -- {killThread cid ; cid' <- forkIO $ communicateWith path dch; loopECUwith dch (lid,did,cid') } 
                            Ex.throwTo cid Quit
                            cid' <- forkIO $ communicateWith path datach
                            loopECUwith (lid, did, cid', datach )
              '\n'      -> do 
                            Ex.throwTo cid Quit -- writeTChan cmdch Quit
                            cid' <- forkIO $ communicateWith path datach
                            loopECUwith (lid, did, cid', datach )
              otherwise -> do { return () }
-- -------------------------
-- -- | Core Function  
-- -------------------------
communicateWith :: FilePath -> Chan Model -> IO ()
communicateWith dev outlet =
  do
    withECU dev  -- withSerialのECUResponse 版　； time out = 0.5sec ; 実質的なElmアーキテクチャの core
        --  withSerialはbracketを使っているので例外発生時もポートを閉じてくれる。
        --  ポート開閉時以外は非同期例外を受け付けてしまうので、新たなラッピング関数 wethECU を導入。2019.03.15 
        --  withECU も実態は bracket なので、非同期例外発生時もポートを閉じる。
        --  また、bracketは例外を呼び出し側に投げ返す。→ catchは外側に
        $ \(ecu, ecumodel,firstmodel) -> Ex.mask $ \unmask -> do
              writeChan outlet firstmodel 
              loop firstmodel ecu unmask -- いくらなんでも unmask 関数を引き回すのは邪道だよね
                where 
                  loop prevModel ecu unmask =
                    do  --  内外からの例外はこのループ内で発生または受信し， withECU (実態はbracket) で処理（ポート閉鎖）の上，catch部で再接続のため再起される
                      jikoku  <- currentTime
                      r8      <- ioEither $ sndCmd80to' ecu
                      r7      <- ioEither $ sndCmd7dto' ecu
                      
                      let newModel = case (r8,r7) of
                              (Left  m8 , _       ) -> 
                                Model { 
                                    num = num prevModel +1  
                                  , dat = ECUData { status = DataError m8 , at = jikoku }
                                  , ECULib.log = ( "E8:" ++ m8 ) : ECULib.log prevModel }
                              (_        , Left m7 ) -> 
                                Model { 
                                    num = num prevModel + 1
                                  , dat = ECUData { status = DataError m7 , at = jikoku }
                                  , ECULib.log = ( "E7:" ++ m7 ) : ECULib.log prevModel }
                              (Right (r8',t1),Right (r7',t2) )-> 
                                Model {
                                    num = num prevModel + 1 
                                  , dat = ECUData { status = GotData ECUData807d { d80 = r8', d7d = r7' } , at  = jikoku }
                                  , ECULib.log = ECULib.log prevModel }
                      writeChan outlet newModel
                      case (r8,r7) of
                        (Right _,Right _) -> do { return () }
                        otherwile  -> do  Ex.throwIO Reconnect
                      unmask $ do { threadDelay 10000  ; return () } -- we need return () to prevent getting Unexpected do block in function application error
                      loop newModel ecu unmask
                    `Ex.catch`  
                          \e -> case (e::UserCommand) of
                              Reconnect    -> do 
                                flush ecu
                                tryECU 64 "read out data" $ ecuRecv ecu 1
                                closeSerial ecu 
                                threadDelay 1000000
                                jikokuc <- currentTime
                                writeChan outlet $ Model { 
                                    num = num prevModel + 1 
                                  , dat = ECUData { status = NotConnected ( "ECU port closed for reconnection at " ++ show jikokuc ) , at  = jikokuc }
                                  , ECULib.log = "Try reconnect" : ECULib.log prevModel }
                                communicateWith dev outlet
                                Ex.throwIO Reconnect -- ECUが停止した後にReconnectが発行された場合の例外を抑止
                              Quit         -> do { return () }
                              ECUCommand c -> case (c::ECUCommand) of
                                ClearFaults -> do
                                                ioEither $ clearECUFaults ecu
                                                jikoku <- currentTime
                                                writeChan outlet $ Model {
                                                    num = num prevModel + 1 
                                                  , dat = ECUData { status = CommandIssued "ClearFaults" , at = jikoku }
                                                  , ECULib.log = ( "Clear Fault command issued. at " ++ show jikoku ) : ECULib.log prevModel }
                                                communicateWith dev outlet
                              otherwise    -> do { Ex.throwIO e }

withECU :: FilePath -> ( (SerialPort,ModelData,Model) -> IO () ) -> IO ()
withECU dev = Ex.bracket
  ( do 
      jikoku <- currentTime
      result <- tryECU 3 "initialization" $ initialize dev
      case result of
        Left e             -> do
          threadDelay 3000000
          Ex.throw Reconnect
          -- fail "ECU Not connected."
        Right (port, model) -> do
          let firstmodel = Model { num = 1, dat = ECUData { status = Connected model, at = jikoku } , ECULib.log = ["ECU Initialized at " ++ show jikoku ] } 
          pure (port,model,firstmodel)
    )
  ( \(port,_,_) -> do { ioEither $ ecuFlush port ; closeSerial port } )
--
-- |
--  モデル更新関数   
renew :: Model -> ECUData -> Model
renew currentModel theData = Model { num = newNumber, dat = newECUData, ECULib.log = newLog }
  where
    newNumber  = num currentModel + 1
    newECUData = theData
    newLog     = case status theData of
      Connected model   -> ( "Connected with " ++ show model ) : ECULib.log currentModel
      CommandIssued m   -> ( "Command (" ++ m ++ ") issued " ) : ECULib.log currentModel
      DataError message -> message : ECULib.log currentModel
      NotConnected msg  -> msg : ECULib.log currentModel
      GotData d807d     -> ECULib.log currentModel  

tryECU :: Int -> String -> ECUResponse a -> IO (Either String a)
tryECU n s a
  | n <= 0    = do { jikoku <- currentTime ; return $ Left $ "count cousumed while trying " ++ s ++ " at " ++ show jikoku }
  | otherwise = do
          r <- ioEither a
          case r of 
            Left _  -> do
              threadDelay 10000
              tryECU (n-1) s a 
            Right _ -> return r

logger :: Chan Model -> IO()
logger modelCh = do
  logfn <- logFileName :: IO (FilePath)
  withFile logfn WriteMode
    $ \logfh -> do
        hPutStrLn logfh  $ "Date,Time," ++ frame80Title ++ "," ++ frame7DTitle
        forever $ do
          d' <- readChan modelCh
          let d = dat d'
          jikoku <- currentTime
          case status d of 
            Connected model  -> {- Ex.mask $ \unmask -> -} do
              hPutStrLn logfh $ "Connected : " ++ show model ++ " at " ++ show jikoku
            CommandIssued m  -> do
              hPutStrLn logfh $ "CMD:" ++ show m ++ " at " ++ show jikoku
            GotData d807d    -> do
              let j  = localTimetoString $ at d
                  d8 = frame80toTable.parse80 $ d80 d807d
                  d7 = frame7DtoTable.parse7d $ d7d d807d
              hPutStrLn logfh  $ j ++ "," ++ d8 ++ "," ++ d7
            DataError msg    -> do
              hPutStrLn logfh  msg
            NotConnected msg -> do
              hPutStrLn logfh  msg
    `Ex.catch`(\e -> case (e::UserCommand) of 
      Quit -> do
        ima <- currentTime
        hPutStrLn logfh $ "At " ++ show ima ++ " , logger terminated because of quit msg."
        hFlush logfh
        hClose logfh
      Reconnect -> do
        ima <- currentTime
        hPutStrLn logfh $ "At " ++ show ima ++ " , try to reconnect ECU."
        hFlush logfh
        hClose logfh
        logger modelCh
      otherwise -> Ex.throw e
        )
    
-- -- | ログファイル名の自動生成関数
-- -- 
-- -- 呼び出された時刻に応じ、以下のような形式のファイル名を生成する：ECULog2018-10-15_17.27.26.csv
logFileName :: IO(FilePath)
logFileName = do
      time <- currentTime :: IO LocalTime
      return $ localtimeToFilePath time
      where localtimeToFilePath (LocalTime n t) =  -- to convert constant length string
              let hi     = show n
                  ji     = todHour t
                  hun    = todMin  t 
                  byo    = todSec  t
                  byo'   = take 2 $ if byo >= 10 then showFixed False byo
                                    else '0':(showFixed False byo) 
              in printf "ECULog%10s_%02d.%02d.%2s.csv" hi ji hun byo' -- ex. ECULog2018-10-15_17.27.26.csv
              --  ./log/ECU...としていたが，ディレクトリが存在していないとランタイムエラーを起こすので変更

-- -- | 画面表示モジュール
displayer :: Chan Model -> IO ()
displayer modelCh = do
  Prelude.putStr $ vt100cr 2    -- 画面全体消去   -- Prelude.putStrLn $ "displayer Started."
  forever $ do
    model <- readChan modelCh
    tableView model
  `Ex.catch` (\e -> case (e::UserCommand) of
    Quit      -> do { Prelude.putStr $ vt100mv 41 0 ; return () }
    Reconnect -> displayer modelCh
    otherwise -> Ex.throw e )

-- | 実際の表示関数
--   新たに数値データを得たとき以外（エラー、初期化等）は、前の数値データ等をそのまま表示しておく
tableView::Model -> IO()
tableView model  = do
  -- ima <- currentTime
  Prelude.putStrLn $ vt100mv 0 0
  Prelude.putStrLn   "-----------------------------------------------"
  Prelude.putStrLn   " MyEcu: Copyright (C)2018-2019 by Kentaro UONO "
  Prelude.putStrLn   "-----------------------------------------------"
  Prelude.putStrLn   statusString
  Prelude.putStrLn   "-------------- time ---------------------------"
  Prelude.putStrLn $ (printf " %6s " numl) ++ (printf " %6s " numr) ++ ": " ++ j
  case status $ dat model of 
    GotData theData -> do
      Prelude.putStrLn   "-------------- 80 data ------------------------"
      Prelude.putStrLn $ (printf "   Engine Speed(rpm):  %5d"   (engineSpeed d8 )) ++ ( bar   0  4000 ( engineSpeed d8 ))
      Prelude.putStrLn $ (printf "throttle Potent( V ):  %5.2f" (throttlePot d8 )) ++ ( bar   0    50 ( truncate ( 10 * (throttlePot d8 ))))
      Prelude.putStrLn $ (printf "   Coolant Temp(dgC):    %3d" (coolantTemp d8 )) ++ ( bar (-55) 100 ( coolantTemp d8 ))
      Prelude.putStrLn $ (printf "   ambient Temp(dgC):    %3d" (ambientTemp d8 )) ++ ( bar (-55) 100 ( ambientTemp d8 ))
      Prelude.putStrLn $ (printf "intake Air Temp(dgC):    %3d" (intakeATemp d8 )) ++ ( bar (-55) 100 ( intakeATemp d8 ))
      Prelude.putStrLn $ (printf "     map Sensor(kPa):    %3d" (mapSensor   d8 )) ++ ( bar   0   130 ( mapSensor   d8 ))
      Prelude.putStrLn $ volt                                                 ++ ( bar   0   200 ( truncate ( 10 * ( battVoltage d8 ))))
      Prelude.putStrLn $ (printf "    idle switch     :  %5s"   (closedorclear (idleSwitch  d8 )))
      Prelude.putStrLn $ (printf "     park or neutral:  %5s"   (parkorneutral (pnClosed    d8 )))
      Prelude.putStrLn $ (printf "idl Air Ctl M P(C/O):    %3d" (idleACMP    d8 )) ++ (bar   0  180 ( idleACMP   d8 ))
      Prelude.putStrLn $ (printf "idl Spd deviatn     :  %5d"   (idleSpdDev  d8 )) ++ (bar   0 1000 ( idleSpdDev d8 ))
      Prelude.putStrLn $ (printf "ignition advnce(deg): %6.2f"  (ignitionAd  d8 )) ++ (bar   0   20 ( truncate (ignitionAd d8 )))
      Prelude.putStrLn $ (printf "      coil Time(msc):  %5.2f" (coilTime    d8 )) ++ (bar   0   20 ( truncate (coilTime   d8 )))
      Prelude.putStrLn   "-------------- 7D data ------------------------"
      Prelude.putStrLn $ (printf " lambda voltage( mV):    %3d  " (lambda_voltage d7 )) ++ ( richorlean ( lambda_voltage d7 ))
      Prelude.putStrLn $ (printf "    closed loop(C/O):    %3d" (closed_loop'   d7 )) -- ++ (printf "   %3d    %3d" (closed_loop'   d7x) (closed_loop'   d7m) )
      Prelude.putStrLn $ (printf "      fuel trim( %% ):    %3d" (fuel_trim'     d7 )) ++ (bar  0 500 ( fuel_trim' d7 )) --(printf "   %3d    %3d" (fuel_trim'     d7x) (fuel_trim'     d7m) )
      Prelude.putStrLn $ "- Fault Code ----------------------------------"
      Prelude.putStrLn $ " (01) Coolant temp Sensor      | " ++ e01
      Prelude.putStrLn $ " (02) Air temp sensor          | " ++ e02
      Prelude.putStrLn $ " (10) Fuel pump circuit        | " ++ e10
      Prelude.putStrLn $ " (16) Throttle position sensor | " ++ e16
      Prelude.putStrLn $ "  Press '0' to clear fault codes "
      where 
        theData = case status ( dat model ) of 
          GotData data807d -> data807d 
          otherwise        -> ECUData807d { d80 = emptyD80, d7d = emptyD7d }
        d8   = parse80.d80 $ theData
        d7   = parse7d.d7d $ theData
        b    = battVoltage d8
        bs   = printf "battery Voltage( V ):  %5.2f" b
        bell = if (b > 15.0 || b < 9.0 ) then "" else "" -- \BEL\BEL\BEL" else "\BEL"  
        volt = if (b > 15.0 || b < 9.0 ) then red ++ bs ++ reset else bs
        e01 = if faultCode1 d8  then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (01) Coolant temp Sensor 
        e02 = if faultCode2 d8  then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (02) Air temp sensor 
        e10 = if faultCode10 d8 then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (10) Fuel pump cirkit 
        e16 = if faultCode16 d8 then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (16) Throttle position sensor 
        bar::Int -> Int -> Int -> String
        bar min max x = printf "  %-10s" $ take (fromIntegral (10 * x `div` (max-min))) "**********"
        tf c = if c then green ++ "True " ++ reset else red ++ "False" ++ reset
        closedorclear::Bool -> String
        closedorclear b = if b then " Closed" else " Other "
        parkorneutral::Bool -> String -- 0 is closed
        parkorneutral b = if b then " Closed" else " Open  "
        richorlean::Int -> String
        richorlean v = if v >= 450 then bred ++ green ++ " rich     " ++ reset else bgreen ++ yellow ++ " lean     " ++ reset
    otherwise -> return ()
  Prelude.putStrLn $ ( vt100mv 32 0 ) ++ "----------------- Log -------------------------"
  mapM_ Prelude.putStrLn $ if length logs >= 5 then  take 5 logs else logs
  Prelude.putStrLn $ ( vt100mv 38 0 ) ++ "-----------------------------------------------"
  where 
        statusString =  case status $ dat model of 
          Connected    model    -> bgreen ++ yellow ++ " Connected       " ++ reset ++ show model ++ (vt100lc 0)
          CommandIssued msg     -> bgreen ++ yellow ++ " Cmd Issued      " ++ reset ++ show msg ++ (vt100lc 0)
          NotConnected message  -> bred   ++ yellow ++ " Not Connected : " ++ message ++ reset ++ (vt100lc 0)
          DataError message     -> bred   ++ yellow ++ " DataError     : " ++ message ++ reset ++ (vt100lc 0)
          GotData _             -> bgreen ++ yellow ++ " Connected       " ++ reset ++ (vt100lc 0)   
        nums = printf "%6s" ( show $ num model )
        logs = ECULib.log model
        numr = if odd $ num model       then (bwhite ++ black ++ nums ++ reset ) else nums
        numl = if not . odd $ num model then (bwhite ++ black ++ nums ++ reset ) else nums
        j    = show . at $ dat model

frame80Title  = "E Speed,coolant T,ambient T,intakeAir T,fuel T,map Sensor,btVolt,throtle Pot,idle Swch,0B,p/n switch, CTS E,IATS E, FPC E, TPC E,0F,10,11,iACMP,iSDev,15,ignAd,coil T,19,1A,1B"
frame80Fmt    = "%5d,%3d,%3d,%3d,%3d,%3d,%6.2f,%6.2f,%c,%02X,%c,%c,%c,%c,%c,%02X,%02X,%02X,%3d,%6d,%02X,%5.1f,%5.1f,%02X,%02X,%02X"
-- | Escape sequence in VT100
reset    = "\ESC[0m"
brev     = "\ESC[7m"  --  set reverse
-- | Escape sequence in VT100 / set text foreground colour
black    = "\ESC[30m" 
red      = "\ESC[31m" 
green    = "\ESC[32m" 
yellow   = "\ESC[33m"
blue     = "\ESC[34m"
magenta  = "\ESC[35m"
cyan     = "\ESC[36m"
white    = "\ESC[37m"
-- | Escape sequence in VT100 / set text background Colours
bblack   = "\ESC[40m"--	Black
bred     = "\ESC[41m"--	Red
bgreen   = "\ESC[42m"--	Green
byellow  = "\ESC[43m"--	Yellow
bblue    = "\ESC[44m"--	Blue
bmagenta = "\ESC[45m"--	Magenta
bcyan    = "\ESC[46m"--	Cyan
bwhite   = "\ESC[47m"--	White

frame80toTable :: Frame80 -> String
frame80toTable f = {-# SCC "frame80toTable" #-}
  printf frame80Fmt
    (engineSpeed f ) -- ::Int
    (coolantTemp f ) -- ::Int
    (ambientTemp f ) -- ::Int
    (intakeATemp f ) -- ::Int
    (fuelTemp    f ) -- ::Int
    (mapSensor   f ) -- ::Int
    (battVoltage f ) -- ::Float
    (throttlePot f ) -- ::Float
    (tf (idleSwitch  f )) -- ::Bool ,-- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
    (unknown0B       f )  -- ::Word8,-- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
    (tf (pnClosed    f )) -- 0x0C ::Park/neutral switch. Zero is closed, nonzero is open.
    (tf (faultCode1  f )) -- 0x0D * Bit 0: Coolant temp sensor fault (Code 1)
    (tf (faultCode2  f )) --      * Bit 1: Inlet air temp sensor fault (Code 2)
    (tf (faultCode10 f )) -- 0x0E * Bit 1: Fuel pump circuit fault (Code 10)
    (tf (faultCode16 f )) --      * Bit 7: Throttle pot circuit fault (Code 16)
    (unknown0F   f ) -- :: Word8, 0x0F
    (unknown10   f ) -- :: Word8, 0x10
    (unknown11   f ) -- :: Word8, 0x11
    (idleACMP    f ) -- :: Int  ,-- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
    (idleSpdDev  f ) -- :: Int  ,-- 0x13-14	Idle speed deviation (16 bits)
    (unknown15   f ) -- :: Word8, 0x15
    (ignitionAd  f ) -- :: Float,-- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
    (coilTime    f ) -- :: Float,-- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
    (unknown19   f ) -- :: Word8, 0x19
    (unknown1A   f ) -- :: Word8, 0x1A
    (unknown1B   f ) -- :: Word8, 0x1B
    where tf c = if c then 'T' else 'F'

frame7DTitle = "lmdvt,clsdl,fuelt"
frame7DFmt   = "%5d,%5d,%5d"

frame7DtoTable :: Frame7d -> String
frame7DtoTable f =  {-# SCC "frame7DtoTable" #-}
    printf frame7DFmt
      (lambda_voltage f) -- :: Int
      (closed_loop'   f) -- :: Int
      (fuel_trim'     f) -- :: Int 

-- ---------------------------
-- -- constants definition  --
-- ---------------------------
-- -- USB-serial related
-- ---------------------------

sndCmd80to':: SerialPort -> ECUResponse Res
sndCmd80to' port = {-# SCC "send80'" #-} do
  rc <- sendCommand port reqData80
  rs <- tryIO 5 " Waiting 80 response." $ recv port 1
  rd <- repeatIO (size rs -1) (BS.pack []) ( tryIO 5  " Waiting 80 data " ( recv port 1 ) )
  t  <- ima
  if ((toInt $ BS.index rs 0 ) - 1) == (BS.length rd) then
    return $ ( BS.concat [rs,rd] , t )
  else 
    fail $ "E8S: response size error -> " ++ ( concat . map (printf " %02X") $ BS.unpack $ BS.concat [rs,rd] )


parse80 :: BS.ByteString -> Frame80
parse80 d =  {-# SCC "parse80" #-} 
  if d == BS.empty || ( (toInteger (BS.length d) ) /= ( (toInteger $ BS.index d 0 ) ) ) then
    emptyFrame80 -- 時々取りこぼしが発せするようなので、フィルタリング
  else 
    Frame80 {
          size_80     = toInt $ BS.index d 0,
          engineSpeed = 256 * toInt (BS.index d 1) + toInt (BS.index d 2),
          coolantTemp = -55 + toInt (BS.index d 3), -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
          ambientTemp = -55 + toInt (BS.index d 4), -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
          intakeATemp = -55 + toInt (BS.index d 5), -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
          fuelTemp    = -55 + toInt (BS.index d 6), -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
          mapSensor   = toInt $ BS.index d 7,       -- 0x07	MAP sensor value in kilopascals
          battVoltage = 0.1  * (fromIntegral $ BS.index d 8)::Float,      -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
          throttlePot = 0.02 * (fromIntegral $ BS.index d 9)::Float,      -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
          idleSwitch  = testBit (BS.index d 10) 4, -- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
          unknown0B   = BS.index d 11, -- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
          pnClosed    = BS.index d 12 == 0, -- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
          faultCode1  = testBit (BS.index d 13) 0, -- Coolant temp sensor
          faultCode2  = testBit (BS.index d 13) 1, -- Air temp sensor 
          faultCode10 = testBit (BS.index d 14) 1, -- Fules pump cirkit
          faultCode16 = testBit (BS.index d 14) 7, -- Throttle position sensor
          unknown0F   = BS.index d 15, -- 0x0F	Unknown
          unknown10   = BS.index d 16, -- 0x10	Unknown
          unknown11   = BS.index d 17, -- 0x11	Unknown
          idleACMP    = toInt $ BS.index d 18, -- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
          idleSpdDev  = 256 * (toInt (BS.index d 19)) + (toInt(BS.index d 20)),  -- 0x13-14	Idle speed deviation (16 bits)
          unknown15   = BS.index d 21, -- 0x15	Unknown
          ignitionAd  = -24 + 0.5 * (fromIntegral $ BS.index d 22)::Float, -- 0x16	Ignition  0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
          coilTime    = 0.002 * fromIntegral (256 * (toInt (BS.index d 23))+ (toInt ( BS.index d 24))), -- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
          unknown19   = BS.index d 25, -- 0x19	Unknown
          unknown1A   = BS.index d 26, -- 0x1A	Unknown
          unknown1B   = BS.index d 27  -- 0x1B	Unknown
          } 
  
sndCmd7dto':: SerialPort -> ECUResponse Res
sndCmd7dto' port =  {-# SCC "send7D'" #-} do
  rc <- sendCommand port reqData7d
  rs <- tryIO 5 "Sending 7d" $ recv port 1
  rd <- repeatIO (size rs -1) (BS.pack []) ( tryIO 5 " Waiting 80 data " $ recv port 1 )
  t  <- ima
  if ((toInt $ BS.index rs 0 ) - 1) == (BS.length rd) then
    return $ ( BS.concat [rs,rd] , t )
  else 
    fail $ "E7S: response size error -> " ++ ( concat . map (printf " %02X") $ BS.unpack $ BS.concat [rs,rd] )
      
parse7d :: BS.ByteString -> Frame7d
parse7d d =  {-# SCC "parse7D" #-} 
  if d == BS.empty || ( (toInteger (BS.length d) ) /= ( (toInteger $ BS.index d 0 ) ) ) then
    emptyFrame7d -- 時々取りこぼしが発せするようなので、フィルタリング
  else 
    Frame7d {
              size_7d        = toInt $ BS.index d 0x00,
              lambda_voltage = 5 * (toInt $ BS.index d 0x06),
              closed_loop'   = toInt $ BS.index d 0x0a,
              fuel_trim'     = toInt $ BS.index d 0x0c -- ,
              }

ecuSend:: SerialPort -> Word8 -> ECUResponse Int
ecuSend port cmd = do
  bytesRcvd <- ECUResponse ( Right `liftM` ( send port (BS.singleton cmd) ) )
  if  bytesRcvd == 0 then
    fail "E01: sending data error."
  else 
    pure bytesRcvd

ecuRecv:: SerialPort -> Int -> ECUResponse Res
ecuRecv port num = do
  result <- ECUResponse ( Right `liftM` ( recv port num ) ) 
  jikoku <- ima
  pure ( result , jikoku ) 

ecuFlush:: SerialPort -> ECUResponse Res
ecuFlush port    = do
  _       <- ECUResponse ( Right `liftM` ( flush port    ))
  jikoku  <- ima
  pure ( BS.empty , jikoku )

sendCommand :: SerialPort -> Word8 -> ECUResponse Res
sendCommand port cmd = do
  jikokus   <- ima
  _         <- ecuFlush port
  byteSend  <- ecuSend port cmd 
  jikokur   <- ima
  if byteSend <= 0 then
    fail $ " E00: ECU did not recieve the command:" ++ show cmd ++ " sent:" ++ show jikokus ++ " echd:" ++ show jikokur
  else do
    (byteRcvd,_)  <- tryIOwith1msecDelay 5 ( "sending command (" ++ show cmd ++ ") and waiting an echo byte started at " ++ show jikokus ++ " and waited untl " ++ show jikokur ) $ recv port 1 -- try io 3 times withbetween 1msec waiting 
    jikokur'      <- ima
    case (byteRcvd == BS.empty, byteRcvd /= (BS.singleton cmd)) of
      (True, _) -> fail $ " E01: ECU did not echoed the command of " ++ show cmd ++ " sent:" ++ show jikokus ++ " echd:" ++ show jikokur ++ " rcvd:" ++ show jikokur'
      (_, True) -> fail $ " E02: ECU echoed wrong character as " ++ show byteRcvd ++ " for " ++ show cmd ++ " sent:" ++ show jikokus ++ " echd:" ++ show jikokur ++ " rcvd:" ++ show jikokur'
      (_,_)     -> pure (byteRcvd, jikokur')

sendCommandAndGetNByteRes:: SerialPort -> Word8 -> Int -> ECUResponse (BS.ByteString,LocalTime)
sendCommandAndGetNByteRes port cmd count = do
  sendCommand port cmd
  byteRcvd  <- repeatIO count BS.empty (tryIO 3 "Getting response" $ recv port 1 )
  jikoku    <- ima
  if byteRcvd == BS.empty then
    fail $ " E11: ECU echoed but did not replied for (" ++ show cmd ++ ") at " ++ show jikoku
  else 
    pure (byteRcvd, jikoku)
      
tryIO :: Int -> String -> IO BS.ByteString -> ECUResponse BS.ByteString
tryIO n s a
  | n <= 0    = fail $ "Count cousumed while trying " ++ s
  | otherwise = do
          r <- ECUResponse ( Right `liftM` a )
          if r == BS.empty then do
            ECUResponse ( Right `liftM` threadDelay 10000 )
            -- jikokus <- ima
            tryIO (n-1) s a 
          else
            pure r

tryIOwith1msecDelay :: Int -> String -> IO BS.ByteString -> ECUResponse Res
tryIOwith1msecDelay n s a
  | n <= 0    = fail $ " Count consumed while trying delay IO of " ++ s 
  | otherwise = do
    r       <- ECUResponse ( Right `liftM` a )
    jikokue <- ima
    if r == BS.empty then do
      ECUResponse ( Right `liftM` (threadDelay 1000))-- 1 msec
      tryIOwith1msecDelay (n-1) s a
    else
      pure (r,jikokue)

repeatIO :: Int -> BS.ByteString -> ECUResponse BS.ByteString -> ECUResponse BS.ByteString
repeatIO c r a 
  |c <= 0    = if BS.length r >= 0 then pure r else fail " RepeatIO Error" 
  |otherwise = do { r'<- a ; repeatIO (c-1) (BS.append r r') a}

getECUResponse :: SerialPort -> ECUResponse ECUData
getECUResponse p = do
  (r8,t8) <- sndCmd80to' p
  (r7,t7) <- sndCmd7dto' p
  return ECUData { status = GotData ECUData807d { d80 = r8, d7d = r7 } , at = t7 }

initialize ::FilePath -> ECUResponse (SerialPort, ModelData)
initialize ecupath = do {- 100msec Timeout when receiving a char in tenth of seconds = timeout * 0.1 sec -}
  pt <- ECUResponse ( Right `liftM` ( openSerial ecupath defaultSerialSettings {timeout= 1, flowControl = Software } ))
  ecuFlush pt 
  sendCommand pt 0xca  -- 202 'ha no hankaku' 
  sendCommand pt 0x75  -- 117 'u' 
  sendHeartBt pt                      -- 244       - 0xf4 
  sendCommand pt 0xd0  -- 208 'mi no hankaku'
  -- recv p 4 だと取りこぼしが出る。何度もECUにリセットがかかっている様子（0.5秒間くらいのリレー音が繰り返される）-> repeatIO に
  -- md <- ECUResponse ( Right `liftM` recv port 4 ) -- IO ByteString -- recv :: SerialPort -> Int -> IO BS.ByteString
  md <- repeatIO 4 BS.empty (tryIO 5 " Reading Model Data " $ recv pt 1)
  if (BS.length md /= 4 ) then
    fail $  "E4: Initialization error. model data is not 4 bytes:" ++ show md 
  else
    case data2model md of
      Nothing       -> fail $ "EM: Unknown model. the data is : " ++ show md
      Just theModel -> pure $ (pt , theModel)

sendHeartBt :: SerialPort -> ECUResponse Res
sendHeartBt port = sendCommandAndGetNByteRes port heartBeat 1 --  It should be 00 (\NUL)

readECUActuatorPos::SerialPort -> ECUResponse Res
readECUActuatorPos port = sendCommandAndGetNByteRes port getIACPosition 1

testActuator::SerialPort -> Word8 -> ECUResponse Res
testActuator port cmd = sendCommandAndGetNByteRes port cmd 1

clearECUFaults::SerialPort -> ECUResponse Res
clearECUFaults port = sendCommandAndGetNByteRes port clearFaults 1 -- 0xcc

-- --
-- Utility functions
-- 
ima :: ECUResponse LocalTime
ima = ECUResponse ( Right `liftM` currentTime )

currentTime :: IO LocalTime
currentTime = do
  timezone <- getCurrentTimeZone
  utctime  <- getCurrentTime
  return $ utcToLocalTime timezone utctime

-- | to convert constant length string
localTimetoString :: LocalTime -> String
localTimetoString (LocalTime n t) = 
  let hi     = show    n
      ji     = todHour t
      hun    = todMin  t
      byo    = todSec  t
      byo'   = take 9 $ if byo >= 10 then showFixed False byo
                            else '0':(showFixed False byo) 
  in printf "%10s,%02d:%02d:%5s " hi ji hun byo'
  
-- | VT100 カーソル移動 エスケープシーケンス　行　桁
vt100mv :: Int -> Int -> String 
vt100mv l c = "\ESC[" ++ (show l) ++ ";" ++ (show c) ++ "H"
-- | VT100 画面消去エスケープシーケンス ; m = 0 ... カーソルから, 1 ... カーソルまで, 2 ... 画面全体
vt100cr :: Int -> String
vt100cr m = "\ESC[" ++ (show m) ++ "J"
-- | VT100 行消去エスケープシーケンス ; 00（or省略）...カーソルより後ろ, 1...カーソルより前, 2...行全体
vt100lc :: Int -> String
vt100lc m = "\ESC["++ show m ++ "K"
dispHex :: String -> BS.ByteString -> IO ()
dispHex t d = do
  Prelude.putStr t
  mapM_ (printf " %02X") $ BS.unpack d

dispHexLn :: String -> BS.ByteString -> IO ()
dispHexLn t d = dispHex t d >> Prelude.putStrLn ""

toInt = fromIntegral . toInteger

-- | only for ecu commands
size::BS.ByteString -> Int
size = fromIntegral . BS.head

-- | Ecu model and its identical data
data ModelData = ModelData { name :: String, mdb :: BS.ByteString , d8size ::Int , d7size ::Int}
instance Show ModelData where 
      show model = show $ name model
mne00000  = ModelData { name = "N/A", mdb = BS.empty , d8size = 0, d7size = 0 }
mne10078  = ModelData { 
  name = "MNE10078  Manual SPI Japan Cooper        - 60487", 
  mdb = BS.pack [0x39, 0x00, 0x00, 0x5c] {-  57, 0, 0, 92 -}, 
  d8size = 28 , d7size = 14 {- =0x0E -} } 
mne101070 = ModelData { 
  name = "MNE101070 Manual SPI Cooper         69493-103112", 
  mdb = BS.pack [0x99, 0x00, 0x02, 0x03] {- 153, 0, 2,  3 -}, 
  d8size = 28 , d7size = 32 } 
mne101170 = ModelData { 
  name = "MNE101170 Manual SPI Except Cooper 103113-134454", 
  mdb = BS.pack [0x99, 0x00, 0x03, 0x03] {- 153, 0, 3,  3 -}, 
  d8size = 28 , d7size = 32 }
    -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx

data2model :: BS.ByteString -> Maybe ModelData
data2model key
  | key == mdb mne10078  = Just mne10078
  | key == mdb mne101070 = Just mne101070
  | key == mdb mne101170 = Just mne101170
  | otherwise            = Nothing
--
data ControlLimit = ControlLimit { ucl :: UCLSet, lcl :: LCLSet }
controlLimits     = ControlLimit {
                         ucl = UCLSet { ucl80 = Frame80 {
                             size_80     = 0x1c  , engineSpeed = 6000  , coolantTemp = 100
                           , ambientTemp = 70    , intakeATemp = 70    , fuelTemp    = 70
                           , mapSensor   = 255   , battVoltage = 30.0  , throttlePot = 7.0
                           , idleSwitch  = True  , unknown0B   = 0xff  , pnClosed    = True
                           , faultCode1  = True  , faultCode2  = True  , faultCode10 = True
                           , faultCode16 = True  , unknown0F   = 0xff  , unknown10   = 0xff
                           , unknown11   = 0xff  , idleACMP    = 190   , idleSpdDev  = 0xffff
                           , unknown15   = 0xff  , ignitionAd  = 104.0 , coilTime    = 0.002 * 0xffff
                           , unknown19   = 0xff  , unknown1A   = 0xff  , unknown1B   = 0xff
                          } , ucl7d = Frame7d {
                             size_7d        = 0x20 , lambda_voltage = 1000
                           , closed_loop'   = 1    , fuel_trim'     = 105 }
                          }
                        , lcl = LCLSet { lcl80 = Frame80 {
                            size_80     = 0x1c  , engineSpeed = 200   , coolantTemp = 25
                          , ambientTemp = 25    , intakeATemp = 25    , fuelTemp    = 25
                          , mapSensor   = 0     , battVoltage = 0.0   , throttlePot = 0.0
                          , idleSwitch  = True  , unknown0B   = 0x00  , pnClosed    = True
                          , faultCode1  = True  , faultCode2  = True  , faultCode10 = True
                          , faultCode16 = True  , unknown0F   = 0x00  , unknown10   = 0x00
                          , unknown11   = 0x00  , idleACMP    = 0     , idleSpdDev  = 0x0000
                          , unknown15   = 0x00  , ignitionAd  = -24   , coilTime    = 0.0
                          , unknown19   = 0x00  , unknown1A   = 0x00  , unknown1B   = 0x00
                         } , lcl7d = Frame7d {
                            size_7d        = 0x20 , lambda_voltage = 1000
                          , closed_loop'   = 1    , fuel_trim'     = 105 }
                         }
    }
data UCLSet       = UCLSet { ucl80 ::Frame80, ucl7d ::Frame7d}
data LCLSet       = LCLSet { lcl80 ::Frame80, lcl7d ::Frame7d }  


emptyFrame80 :: Frame80
emptyFrame80 = Frame80 { 
      size_80  = 0 , engineSpeed = 0 , coolantTemp = 0 , ambientTemp = 0 , intakeATemp = 0 ,
      fuelTemp = 0 , mapSensor   = 0 , battVoltage = 0.0 , throttlePot = 0.0 , idleSwitch  = False ,
      unknown0B   = 0 , pnClosed    = False , faultCode1  = False , faultCode2  = False ,
      faultCode10 = False , faultCode16 = False , unknown0F   = 0 , unknown10   = 0 ,
      unknown11   = 0 , idleACMP    = 0 , idleSpdDev  = 0 , unknown15   = 0 , ignitionAd  = 0.0 ,
      coilTime    = 0.0 , unknown19   = 0 , unknown1A   = 0 , unknown1B   = 0
    }
emptyFrame7d :: Frame7d
emptyFrame7d = Frame7d {
      size_7d        = 0 , lambda_voltage = 0 , closed_loop'   = 0 , fuel_trim'     = 0
    }  
emptyD80 = BS.pack [0x1c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00] -- 28バイト
emptyD7d = BS.pack [0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 32バイト
--
-- test corner 
--
-- let firstModel = initModel
-- message <- ioEither $ getECUResponse ecu :: ECUResponse ECUData
-- loop firstModel message
-- where
--   loop :: Model -> Either Message -> Model
--   loop currentModel message = do
--     let newModel = renew currentModel message
--     writeChan outlet newModel
--     newMessage <- ioEither $ getECUResponse ecu
--     loop newModel newMessage
-- 
-- | Test main function
--
-- ecuCore :: IO ()
-- ecuCore = withSocketsDo do
--   server <- newServer
--   sock   <- listenOn (PortNumber (fromIntegral Port))
--   forever $ do
--      (handle, host, port ) <- accept sock
--      forkFinally 
--        (talk handle server)
--        (\_ -> hClose handle)

