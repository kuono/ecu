{- |
Module      : ECU
Description : Ecu Communication Library for Rover Mini
Copyright   : (c) Kentaro UONO, 2018-2019
License     : N/A
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}

{-# LANGUAGE BangPatterns #-}
module ECU  ( runEcuAt, ECUResponse, ECUData ) where

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
-- import Data.Bits.Extras
import Data.Fixed
import qualified Data.Vector as V
import Text.Printf
import Numeric
import qualified System.Posix.Unistd as U

---------------------------
-- type definition       --
---------------------------
-- | ECUResponse is a Monad that communicate withe ECU in the real world.
-- | You can combine some raw command like read80 or read7d. If some 
-- | error occur while doing ECUResponse action(s), error message will 
-- | be returned as a result of ECUResponse. 
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

data Frame = Frame
  { d80size     :: Int
  , engineSpeed :: Int   -- 0x01-2	Engine speed in RPM (16 bits)
  , coolantTemp :: Int   -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
  , ambientTemp :: Int   -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
  , intakeATemp :: Int   -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
  , fuelTemp    :: Int   -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
  , mapSensor   :: Int   -- 0x07	MAP sensor value in kilopascals
  , battVoltage :: Float -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
  , throttlePot :: Float -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
  , idleSwitch  :: Bool  -- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
  , unknown0B   :: Word8 -- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
  , pnClosed    :: Int   -- 0x0C	Park/neutral switch. Zero is closed, nonzero is open.
                         -- Fault codes. On the Mini SPi, only two bits in this location are checked:             
  , faultCode1  :: Bool  -- 0x0D  * Bit 0: Coolant temp sensor fault (Code 1)
  , faultCode2  :: Bool  --       * Bit 1: Inlet air temp sensor fault (Code 2)
  , faultCode10 :: Bool  -- 0x0E  * Bit 1: Fuel pump circuit fault (Code 10)
  , faultCode16 :: Bool  --       * Bit 7: Throttle pot circuit fault (Code 16)
  , unknown0F   :: Word8 -- 0x0F	Unknown
  , unknown10   :: Word8 -- 0x10	Unknown
  , unknown11   :: Word8 -- 0x11	Unknown
  , idleACMP    :: Int   -- 0x12	Idle air control motor position. On the Mini SPi's A-series engine, 0 is closed, and 180 is wide open.
  , idleSpdDev  :: Int   -- 0x13-14	Idle speed deviation (16 bits)
  , unknown15   :: Word8 -- 0x15	Unknown
  , ignitionAd  :: Float   -- 0x16	Ignition advance, 0.5 degrees per LSB with range of -24 deg (0x00) to 103.5 deg (0xFF)
  , coilTime    :: Float   -- 0x17-18	Coil time, 0.002 milliseconds per LSB (16 bits)
  , unknown19   :: Word8  -- 0x19	Unknown
  , unknown1A   :: Word8  -- 0x1A	Unknown
  , unknown1B   :: Word8  -- 0x1B	Unknown
  , d7dsize     :: Int
  , lambda_voltage:: Int  -- This lambda value is a calculated value (if it is the same as the British emissions test).     And a value of, say, 1.05, suggests it is 5% too lean.   But, if your oxygen (and CO and HC) readings are all good, then it suggests your high lambda reading is because of a leak in the exhaust wgich pulls in fresh air (and oxygen).     You could try starting your car when it is cold and put your hand over the exhaust pipe and look underneath to see if water is leaking from any if the joints. 
  , closed_loop'  :: Int  -- 0 : Open Loop, others : Closed Loop  
  , fuel_trim'    :: Int  
  }

emptyFrame = Frame 
  { d80size     = 28, d7dsize = 32, engineSpeed = 0 , coolantTemp = 0 , ambientTemp = 0 , intakeATemp = 0
  , fuelTemp    = 0 , mapSensor   = 0 , battVoltage = 0.0 , throttlePot = 0.0 , idleSwitch  = False
  , unknown0B   = 0 , pnClosed    = 0 , faultCode1  = False , faultCode2  = False
  , faultCode10 = False , faultCode16 = False , unknown0F   = 0 , unknown10   = 0
  , unknown11   = 0 , idleACMP    = 0 , idleSpdDev  = 0 , unknown15   = 0 , ignitionAd  = 0.0
  , coilTime    = 0.0 , unknown19   = 0 , unknown1A   = 0 , unknown1B   = 0
  , lambda_voltage = 0 , closed_loop'   = 0 , fuel_trim'     = 0
  }

-- | Ecu model and its identical data
data ModelData = ModelData { name :: String, mdb :: BS.ByteString , d8size ::Int , d7size ::Int}
instance Show ModelData where 
      show model = show $ name model ++ mdata model
mdata :: ModelData -> String
mdata      = show . BS.unpack . mdb -- mapM (printf " %02X") . BS.unpack . mdb 
mne00000   = ModelData { name = "N/A ", mdb = BS.empty , d8size = 0, d7size = 0 }
mneUnknown = ModelData { name = "unknown ", mdb = BS.empty , d8size = 28, d7size = 14 }
mneAuto    = ModelData { name = "Japanese Automatic ", 
  mdb = BS.pack [0x3a, 0x00, 0x02, 0x14 ]{- 58,0,2,20 -},
  d8size = 28, d7size = 14 }
mne10078   = ModelData { 
  name = "MNE10078  Manual SPI Japan Cooper        - 60487 ", 
  mdb = BS.pack [0x39, 0x00, 0x00, 0x5c] {-  57, 0, 0, 92 -}, 
  d8size = 28 , d7size = 14 {- =0x0E -} } 
mne101070 = ModelData { 
  name = "MNE101070 Manual SPI Cooper         69493-103112 ", 
  mdb = BS.pack [0x99, 0x00, 0x02, 0x03] {- 153, 0, 2,  3 -}, 
  d8size = 28 , d7size = 32 } 
mne101170 = ModelData { 
  name = "MNE101170 Manual SPI Except Cooper 103113-134454 ", 
  mdb = BS.pack [0x99, 0x00, 0x03, 0x03] {- 153, 0, 3,  3 -}, 
  d8size = 28 , d7size = 32 } 
    -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx

data2model :: BS.ByteString -> Maybe ModelData
data2model key
  | key == mdb mne10078  = Just mne10078
  | key == mdb mne101070 = Just mne101070
  | key == mdb mne101170 = Just mne101170
  | key == mdb mneAuto   = Just mneAuto
  | otherwise            = Just mneUnknown { mdb = key } -- Nothing
--

emptyD80 = BS.pack [0x1c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 28バイト
emptyD7d = BS.pack [0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 32バイト
emptyD87 = BS.concat [ emptyD80, emptyD7d ]

data Loop        = OpenLoop | ClosedLoop deriving (Show)
type Data80      = BS.ByteString
type Data7d      = BS.ByteString

data ECUCommand  = MoveIAC Int | TestActuator ActuatorCmd | ClearFaults | GetIACPosition | ContinueLoop deriving (Show, Typeable)
type ActuatorCmd = Word8
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

data UserCommand = Reconnect | ECUCommand { command::ECUCommand} | Quit deriving (Show, Typeable) 
instance Ex.Exception UserCommand

data Model  = Model {
    num  :: Integer   -- ^ num  : 直前までの累積データ数 
  , dat  :: ECUData   -- ^ dat  : 最新データ
  -- , dats :: [ECUData] -- ^ dats : 直前までのデータ（最大 maxBinData個） 
  , log  :: [String] 
  } 
data ECUData = ECUData { status :: ECUStatus, at :: LocalTime } 
data ECUStatus = Connected ModelData | CommandIssued String | GotData ECUData807d | DataError String | NotConnected String 
type ECUData807d = BS.ByteString

type GraphData = [[Dots]]
type Dots = (Char,Char)
data GraphRange = OverUL | Over75 | InBetween7550 | InBetween5025 | Under25 | UnderLL deriving (Eq,Ord)

maxGraphLength = 40
maxBinData     = 40

getInitModel      =  do
  ima <- currentTime
  return $ Model
    { num  = 0
    , dat  = ECUData { status = NotConnected "Not initialized." , at = ima }
    -- , dats = [] 
    , ECU.log = [""]
    }
getFirstData     = do
  ima <- currentTime
  return [ECUData {status = NotConnected "Not Initialized." , at = ima}]

type Res           = (BS.ByteString,LocalTime) 
type SetCmd        = Word8
type SData         = BS.ByteString
type SDataSet      = (SetCmd, SData)

type DataCmd       = Word8
reqData7d      = 0x7d :: DataCmd -- get data for frame80 - 128
reqData80      = 0x80 :: DataCmd -- get data for frame7d - 125
clearFaults    = 0xcc :: DataCmd -- 204, Clear fault codes	CC 00
heartBeat      = 0xf4 :: DataCmd -- 0xf4 244
getIACPosition = 0xfb :: DataCmd 

-- | 主関数
runEcuAt :: String  -- ^ ECUにつながったシリアルポート（USB経由で可）のパス名 
         -> IO ()
runEcuAt path = do 
  exist <- doesFileExist path
  if not exist then
    do { Prelude.putStrLn $ "Device does not exist. Check USB plug and/or serial cable connection at :" ++ path ; return () } 
  else
    do
      Prelude.putStr $ vt100cr 3 -- clear screen
      datach  <- newChan :: IO (Chan Model) -- outlet chanel for sending the Model
      datach' <- dupChan datach
      lid <- forkIO $ logger    datach  --  fork logger thread 
      did <- forkIO $ displayer datach' --  fork displayer thread
      cid <- forkIO $ communicateWith path datach --  fork communication thread

      hSetBuffering stdin NoBuffering -- set non buffering mode 
      hSetEcho      stdin False

      mainLoop (lid, did, cid, datach )  --  event driven style main commander loop as follows

  `Ex.catches` 
    [ Ex.Handler ( \e ->
        case (e::UserCommand) of
          Quit       -> return () 
          Reconnect  -> do { print "Recannect command issued." ; runEcuAt path }
          _          -> Ex.throwIO e -- ECUCommand 
        )
    , Ex.Handler ( \e -> do -- Prelude.putStrLn $ vt100mv 31 0 ++ show (e::Ex.IOException) 
        print (e::Ex.IOException) 
        runEcuAt path
        )
    ]
  where
        mainLoop :: (ThreadId,ThreadId,ThreadId, Chan Model ) -> IO()
        mainLoop  (lid, did, cid, datach ) = -- 引数でのデータの引き回しは早急になくしたい
          do 
            ch <- getChar
            case ch of
              '\ESC'    -> commandLoop (cid,datach) -- do { Ex.throwTo cid Quit ; Ex.throwTo lid Quit ; Ex.throwTo did Quit ; Ex.throwIO Quit }
              'q'       -> do { Ex.throwTo cid Quit ; Ex.throwTo lid Quit ; Ex.throwTo did Quit ; Ex.throwIO Quit }
              '\''      -> do { Ex.throwTo cid Quit ; Ex.throwTo lid Quit ; Ex.throwTo did Quit ; Ex.throwIO Quit }
              ' '       -> do { Ex.throwTo cid Quit ; killThread cid ; cid' <- forkIO $ communicateWith path datach ; mainLoop (lid,did,cid',datach) } 
              '\n'      -> Ex.throwTo cid Reconnect
              _         -> mainLoop  (lid, did, cid, datach ) 
        commandLoop :: (ThreadId,Chan Model) -> IO()
        commandLoop (cid,datach) = do
          Prelude.putStr $ bred ++ yellow ++ "Command:" ++ reset
          let cmd = selectCommand [
                  ("0 :Clear Faults", ClearFaults)
                , ("a :Read Actuator Position", GetIACPosition )
                -- , ("t :Test Actuator",TestActuator)
                , ("c :Continue Loop",ContinueLoop)
                -- , ("r :Reconnect",Reconnect)
                ]
          -- Ex.throwTo cid $ ECUCommand { command = cmd }
          return ()
        selectCommand :: [(String,ECUCommand)] -> ECUCommand
        selectCommand = return ContinueLoop

-- -------------------------
-- -- | Core Function  
-- -------------------------
communicateWith :: FilePath -> Chan Model -> IO ()
communicateWith dev outlet = 
    -- Ex.handle print
    withECU dev  -- withSerialのECUResponse 版　； time out = 0.5sec ; 実質的なElmアーキテクチャの core
        --  withSerialはbracketを使っているので例外発生時もポートを閉じてくれる。
        --  ポート開閉時以外は非同期例外を受け付けてしまうので、新たなラッピング関数 withECU を導入。2019.03.15 
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
                                    num  = num prevModel + 1  
                                  , dat  = ECUData { status = DataError m8 , at = jikoku }
                                  -- , dats = dats prevModel
                                  , ECU.log = ( "E8:" ++ m8 ) : ECU.log prevModel }
                              (_        , Left m7 ) -> 
                                Model { 
                                    num  = num prevModel + 1
                                  , dat  = ECUData { status = DataError m7 , at = jikoku }
                                  -- , dats = dats prevModel 
                                  , ECU.log = ( "E7:" ++ m7 ) : ECU.log prevModel }
                              (Right (r8',t1),Right (r7',t2) )-> 
                                Model {
                                    num  = num prevModel + 1 
                                  , dat  = d
                                  -- , dats = take maxBinData $ d:(dats prevModel)
                                  , ECU.log = ECU.log prevModel }
                                  where d = ECUData { status = GotData (BS.concat [r8', r7']) , at  = jikoku }
                      writeChan outlet newModel
                      case (r8,r7) of
                        (Right _,Right _) -> unmask $ do { threadDelay 10000  ; return () }
                        -- ^ ECUからのデータ読み込みループで正常にデータが読み込めた場合のコマンド割り込み受付用待機
                        _                 -> Ex.throwIO Reconnect
                        -- ^ ECUからのデータ読み込みループで異常が起こった際のcommunication Thread からの脱出

                      loop newModel ecu unmask

                    `Ex.catch`  
                          \e -> do -- ^ UserCommand Exception raised
                            flush ecu
                            case (e::UserCommand) of
                              Reconnect    -> do 
                                -- flush ecu
                                tryECU 64 "read out data" $ ecuRecv ecu 1
                                closeSerial ecu 
                                threadDelay 1000000
                                jikokuc <- currentTime
                                writeChan outlet $ Model
                                  { num  = num prevModel + 1 
                                  , dat  = ECUData { status = NotConnected ( "ECU port closed for reconnection at " ++ show jikokuc ) , at  = jikokuc }
                                  , ECU.log = "Try reconnect" : ECU.log prevModel
                                  }
                                communicateWith dev outlet
                                -- Ex.throwIO Reconnect -- ECUが停止した後にReconnectが発行された場合の例外を抑止
                              Quit         -> do 
                                -- flush ecu
                                tryECU 64 "read out data" $ ecuRecv ecu 1
                                closeSerial ecu 
                                threadDelay 1000000
                                jikokuc <- currentTime
                                writeChan outlet $ Model
                                  { num  = num prevModel + 1 
                                  , dat  = ECUData { status = NotConnected ( "ECU port closed for quit at " ++ show jikokuc ) , at  = jikokuc }
                                  , ECU.log = "Closed Port to quit" : ECU.log prevModel
                                  }
                                communicateWith dev outlet
                                Ex.throwIO Quit
                                return ()
                              ECUCommand c -> case (c::ECUCommand) of
                                ClearFaults -> do
                                  ioEither $ clearECUFaults ecu
                                  jikoku <- currentTime
                                  writeChan outlet $ Model
                                    { num  = num prevModel + 1 
                                    , dat  = ECUData { status = CommandIssued "ClearFaults" , at = jikoku }
                                    , ECU.log = ( "Clear Fault command issued. at " ++ show jikoku ) : ECU.log prevModel
                                    }
                                  communicateWith dev outlet

withECU :: FilePath -> ( (SerialPort,ModelData,Model) -> IO () ) -> IO ()
withECU dev = Ex.bracket
  ( do 
      jikoku <- currentTime
      result <- tryECU 3 "initialization" $ initialize dev
      case result of
        Left e             -> do
          -- Prelude.putStrLn $ "Ecu did not respond ( " ++ e ++ " ) .I will try to reconnect."
          threadDelay 3000
          -- Ex.throw Reconnect 
          fail $ "ECU Not connected." ++ e
        Right (port, model) -> do
          let firstmodel = Model {
                    num  = 1
                  , dat  = d
                  , ECU.log = ["ECU Initialized at " ++ show jikoku ] 
                  } 
          pure (port,model,firstmodel)
          where d = ECUData { status = Connected model, at = jikoku }
    )
  ( \(port,_,_) -> do
          ioEither $ ecuFlush port
          closeSerial port -- 以下，付け加えてみた。2019.06.28
          -- Prelude.putStrLn $ vt100mv 31 0 ++ yellow ++ bred ++ "Communication failed. I'll try again." ++ reset
          threadDelay 1000
          Ex.throw Reconnect 
          )
--
-- |
--  モデル更新関数   
renew :: Model -> ECUData -> Model
renew currentModel theData =  
    Model { num  = case status theData of
              GotData _   -> num currentModel + 1
              Connected _ -> num currentModel + 1
              DataError _ -> num currentModel + 1 
              _           -> num currentModel   
          , dat  = theData 
          -- , dats = case status theData of  
          --     GotData d807d -> take maxBinData $ theData:(dats currentModel)
          --     Connected _   -> take maxBinData $ theData:(dats currentModel)
          --     _             -> dats currentModel
          , ECU.log = case status theData of
              Connected model   -> ( "Connected with " ++ show model ) : ECU.log currentModel
              CommandIssued m   -> ( "Command (" ++ m ++ ") issued " ) : ECU.log currentModel
              DataError message -> message : ECU.log currentModel
              NotConnected msg  -> msg : ECU.log currentModel
              GotData d807d     -> ECU.log currentModel  
          }

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
  logfn <- logFileName :: IO FilePath
  withFile logfn WriteMode
    $ \logfh -> do
        hPutStrLn logfh  $ "Date,Time," ++ frameTitle
        forever $ do
          d' <- readChan modelCh
          let d = dat d'
          jikoku <- currentTime
          case status d of
            GotData d807d    -> 
              let j  = localTimetoString $ at d
                  l  = frametoTable $ parse d807d
              in hPutStrLn logfh  $ j ++ "," ++ l
            Connected model  -> hPutStrLn logfh $ "Connected" ++ ',': show model ++ ",at" ++ show jikoku
            CommandIssued m  -> hPutStrLn logfh $ "CMD:" ++ show m ++ " at " ++ show jikoku
            DataError msg    -> hPutStrLn logfh  msg
            NotConnected msg -> hPutStrLn logfh  msg
    `Ex.catches`
      [ Ex.Handler (\e -> case (e::UserCommand) of 
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
          _  -> Ex.throwIO e)
      , Ex.Handler (\e -> case (e::Ex.IOException) of
          _  -> Ex.throwIO e -- print e 
          )
      ]
    
-- -- | ログファイル名の自動生成関数
-- -- 
-- -- 呼び出された時刻に応じ、以下のような形式のファイル名を生成する：ECULog2018-10-15_17.27.26.csv
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

-- -- | display module
displayer :: Chan Model -> IO ()
displayer modelCh =
  do
    Prelude.putStr $ vt100cr 2    -- clear all screen   -- Prelude.putStrLn $ "displayer Started."
    initModel <- getInitModel
    firstData <- getFirstData
    tableView initModel firstData
  `Ex.catch` (\e -> case (e::UserCommand) of
    Quit      -> do { Prelude.putStr $ vt100mv 41 0 ; return () }
    Reconnect -> displayer modelCh
    _         -> Ex.throwIO e )
  where
  -- | actual display function
  --   previous data remain displayed while not recieving any new numeric data
  --   e.g. error or initialization command issued.
  tableView :: Model      -- ^ new data 
            -> [ECUData]  -- ^ old recorded data
            -> IO()       
  tableView model dats =
    do
      tuiDisplay
      newModel <- readChan modelCh
      let newDats = take 60 $ dat newModel:dats
      tableView newModel newDats
    where
      tuiDisplay :: IO ()
      tuiDisplay =
        do
          Prelude.putStr   $ vt100mv 0 0
          Prelude.putStrLn $ "-----------------------------------------------" ++ vt100cr 0
          Prelude.putStrLn $ " MyEcu: Copyright (C)2018-2019 by Kentaro UONO " ++ vt100cr 0
          Prelude.putStrLn $ "-------------- time ---------------------------" ++ vt100cr 0
          Prelude.putStrLn $ printf  " %6s " numl ++ printf " %6s " numr ++ ": " ++ j ++ " " ++ statusString ++ vt100cr 0
          Prelude.putStrLn $ "-------------- 80 data ------------------------" ++ " (" ++ show l8 ++ " bytes ) " ++ reset ++ vt100lc 0
          draw80
          drawDataLine coolantTemp "   Coolant Temp (dgC) :    %3d"  (-55)    150
          drawDataLine ambientTemp "   ambient Temp (dgC) :    %3d"  (-55)    150
          drawDataLine intakeATemp "intake Air Temp (dgC) :    %3d"  (-55)    150
          Prelude.putStrLn $ printf "    idle switch       :%5s" (closedorclear (idleSwitch  d87 ))
          Prelude.putStrLn $ printf " park or neutral? A/C?:%5s" (parkorneutral (pnClosed    d87 ))   ++ ' ':aconoff (pnClosed d87) ++ reset ++ vt100lc 0
          drawDataLine idleACMP    "idl Air Ctl M P(C/O)  :    %3d"     0     180
          drawDataLine idleSpdDev  "idl Spd deviatn       :  %5d"       0    1000 
          drawDataLine ignitionAd  "ignition advnce (deg) : %6.2f"   (-24.0) 207.0 
          drawDataLine coilTime    "      coil Time (msc) :  %5.2f"     0.0  132.0 
          Prelude.putStrLn $         " 0B 0F 10 11 15 19 1A 1B" ++ reset ++ vt100lc 0
          Prelude.putStrLn $ printf " %2x %2x %2x %2x %2x %2x %2x %2x" (unknown0B d87) (unknown0F d87) (unknown10 d87) (unknown11 d87) (unknown15 d87) (unknown19 d87) (unknown1A d87) (unknown1B d87) ++ reset ++ vt100lc 0
  
          Prelude.putStrLn $ "-------------- 7D data ------------------------" ++ " (" ++ show l7 ++ " bytes ) " ++ reset ++ vt100lc 0
          Prelude.putStrLn $ printf  " lambda voltage ( mV) :    %3d  "  ( lambda_voltage d87 ) ++ richorlean ( lambda_voltage d87 )
          Prelude.putStrLn $ printf  "      closed loop     :    %3d  "  ( closed_loop'   d87 ) ++ openorclosed ( closed_loop' d87 )
          drawDataLine fuel_trim'    "      fuel trim ( %% ) :    %3d "  0 500
          Prelude.putStrLn $ "- Fault Code ----------------------------------" ++ reset ++ vt100lc 0
          Prelude.putStrLn $ " (01) Coolant temp Sensor      | " ++ e01 ++ " (02) Air temp sensor          | " ++ e02 ++ reset ++ vt100lc 0
          Prelude.putStrLn $ " (10) Fuel pump circuit        | " ++ e10 ++ " (16) Throttle position sensor | " ++ e16 ++ reset ++ vt100lc 0
          Prelude.putStrLn $ "  Press '0' to clear fault codes " ++ reset ++ vt100lc 0
          Prelude.putStrLn $ vt100mv 30 0  ++ "----------------- Log -------------------------"
          mapM_ (Prelude.putStrLn . take 40 ) (if length logs >= 4 then take 4 logs else logs)
          Prelude.putStrLn $ vt100mv 36 0  ++ "-----------------------------------------------" ++ vt100mv 3 0
          return ()
      d   = dat  model
      (d8,d7,l8,l7,d87)  = case status d of 
          GotData d' ->
            let d8' = BS.take (fromIntegral (BS.head d')) d'
                d8l = fromIntegral (BS.head d8') ::Int
                d7' = BS.drop d8l d'
                d7l = fromIntegral (BS.head (BS.drop d8l d')) ::Int
                d87 = parse d'
            in (d8',d7',d8l,d7l,d87)
          _          -> (emptyD80,emptyD7d,28,32,parse emptyD87)
      d87s = map analyse dats
      analyse d' = case status d' of
        GotData d'' -> parse d''
        _           -> parse emptyD87
      draw80 :: IO()
      draw80 = do
        drawDataLine engineSpeed "   Engine Speed (rpm) :  %5d"       0    3500
        drawDataLine throttlePot "throttle Potent ( V ) :  %5.2f"     0.0     4.0 -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
        drawDataLine mapSensor   "     map Sensor (kPa) :    %3d"     0     130
        drawDataLine battVoltage "battery Voltage ( V ) :  %5.2f"    12.3    15.0
      draw80':: IO()
      draw80' = drawGraph . makeGraph $ dat model:dats 
      makeGraph::[ECUData] -> GraphData 
      makeGraph d = dots ( dat model ): map dots dats
      dots :: ECUData -> [Dots]
      dots d = case status d of
        GotData d' -> let d'' = parse d'
                          e = range    0 3500 $ engineSpeed d''
                          t = range  0.0  4.0 $ throttlePot d''
                          m = range    0  130 $ mapSensor   d''
                          v = range 12.3 15.0 $ battVoltage d''
                      in toDots ((e,t),(m,v))
        _ -> Prelude.replicate 4 ('-','-')
        where
          c = [Over75,InBetween7550,InBetween5025,Under25]
          toDots ::((GraphRange, GraphRange),(GraphRange , GraphRange )) -> [Dots]
          toDots (d1,d2) =  Prelude.zip (toChars d1) (toChars d2)
          toChars:: (GraphRange,GraphRange) -> [Char]
          toChars r = Prelude.map (toChar r) c
          toChar::(GraphRange,GraphRange) -> GraphRange -> Char
          toChar (r1,r2) r = case (r1 == r, r2 == r) of 
            (True,True) -> ':'
            (True,_   ) -> '\''
            (_   ,True) -> '.'
            _           -> ' '
          range ::  Real a =>  a -> a -> a -> GraphRange
          range min max x
            | x <= min  = OverUL
            | x >= max  = UnderLL
            | otherwise = rangestr !! truncate ( toRational ( length rangestr -1 ) * ( toRational x - toRational min ) / toRational (max-min)) 
          rangestr = [Over75,InBetween7550,InBetween5025,Under25]
      drawGraph :: [[Dots]] -> IO()
      drawGraph g = do
        Prelude.putStrLn $ printf "   Engine Speed (rpm) :  %5d"   ( engineSpeed d87 ) ++ " " ++ pickDotsLine g 0
        Prelude.putStrLn $ printf "throttle Potent ( V ) :  %5.2f" ( throttlePot d87 ) ++ " " ++ pickDotsLine g 1
        Prelude.putStrLn $ printf "     map Sensor (kPa) :    %3d" ( mapSensor d87 )   ++ " " ++ pickDotsLine g 2
        Prelude.putStrLn $ printf "battery Voltage ( V ) :  %5.2f" ( battVoltage d87 ) ++ " " ++ pickDotsLine g 3
      pickDotsLine :: [[Dots]] -> Int -> String
      pickDotsLine d l = Prelude.concat $ [ Prelude.map (fst . (!! l)) d , Prelude.map (snd . (!! l)) d ]
      drawDataLine :: (PrintfArg a , Real a) => (Frame -> a) -> String -> a -> a -> IO ()
      drawDataLine f str min max = Prelude.putStrLn $ printf str ( f d87 ) ++ " " ++ graph min max f
      statusString =  case status $ dat model of
          Connected    model    -> bgreen ++ yellow ++ " Connected           " ++ reset ++ show model ++ vt100lc 0
          CommandIssued msg     -> bgreen ++ yellow ++ " Cmd Issued          " ++ reset ++ show msg ++ vt100lc 0
          NotConnected message  -> bred   ++ yellow ++ " Not Connected     : " ++ take 20 message ++ reset ++ vt100lc 0
          DataError message     -> bred   ++ yellow ++ " DataError         : " ++ take 20 message ++ reset ++ vt100lc 0
          GotData _             -> bgreen ++ yellow ++ printf " Connected (%3d,%3d)" l8 l7 ++ reset ++ vt100lc 0   
      nums = printf "%6s" ( show $ num model )
      logs = ECU.log model
      numr = if odd  $ num model then bwhite ++ black ++ nums ++ reset else nums
      numl = if even $ num model then bwhite ++ black ++ nums ++ reset else nums
      j    = take 26 $ (show . at $ dat model) ++ "000"
      e01 = errorornoerror $ faultCode1  d87 -- (01) Coolant temp Sensor 
      e02 = errorornoerror $ faultCode2  d87 -- (02) Air temp sensor 
      e10 = errorornoerror $ faultCode10 d87 -- (10) Fuel pump cirkit 
      e16 = errorornoerror $ faultCode16 d87 -- (16) Throttle position sensor 
      errorornoerror :: Bool -> String
      errorornoerror b = if b then bred ++ yellow ++ " E R R O R " ++ reset else bgreen ++ yellow ++ " No ERROR  " ++ reset

      graph :: (Real a) => a -> a -> (Frame -> a) -> String
      graph min max f =  " " ++ bar  min max f d : " " ++ map ( bar  min max f ) dats ++ reset ++ vt100lc 0

      bar::(Real a )=> a -> a -> (Frame -> a) -> ECUData -> Char
      bar min max f !x =　case status x of
          Connected     _ -> '+'
          CommandIssued _ -> 'O'
          NotConnected  _ -> 'X'
          DataError     _ -> 'E'
          GotData       d'
            | x' <= min  -> 'L'
            | x' >= max  -> 'U'
            | otherwise -> gs !! truncate (  toRational ( length gs -1 )  *   ( toRational x' - toRational min ) / toRational (max-min))
            where
              gs = " ▁▂▃▄▅▆▇█"
              x' = f $ parse d'

  tf c = if c then green ++ "True " ++ reset else red ++ "False" ++ reset
  closedorclear::Bool -> String
  closedorclear b = if b then " Closed" else " Other "
  parkorneutral::Int -> String -- 0 is closed
  parkorneutral b = if b == 0 then " Closed" else " Open  "
  aconoff::Int -> String
  aconoff       d = if d == 0 then bblue ++ yellow ++ " a/c on   " ++ reset else bgreen ++ yellow ++ " a/c off  " ++ reset
  richorlean::Int -> String
  richorlean v   = if v >= 450 then bred ++ green  ++ " rich     " ++ reset else bgreen ++ yellow ++ " lean     " ++ reset
  openorclosed::Int -> String
  openorclosed d = if d == 0   then bred ++ green  ++ "Crl wt FDt" ++ reset else bgreen ++ yellow ++ "Crl wt O2d" ++ reset

frameTitle  = "E Speed,coolant T,ambient T,intakeAir T,fuel T,map Sensor,btVolt,throtle Pot,idle Swch,0B,p/n switch, CTS E,IATS E, FPC E, TPC E,0F,10,11,iACMP,iSDev,15,ignAd,coil T,19,1A,1B,lmdvt,clsdl,fuelt"
frameFmt    = "%5d,%3d,%3d,%3d,%3d,%3d,%6.2f,%6.2f,%c,%02X,%3d,%c,%c,%c,%c,%02X,%02X,%02X,%3d,%6d,%02X,%5.1f,%5.1f,%02X,%02X,%02X,%5d,%3d,%5d"
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

frametoTable :: Frame -> String
frametoTable f = {-# SCC "frametoTable" #-}
  printf frameFmt
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
    (pnClosed    f ) -- 0x0C ::Park/neutral switch. Zero is closed, nonzero is open.
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
    (lambda_voltage f) -- :: Int
    (closed_loop'   f) -- :: Int
    (fuel_trim'     f) -- :: Int 
  where tf c = if c then 'T' else 'F'

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
  if fromIntegral ( BS.index rs 0 ) - 1 == BS.length rd then
    return ( BS.concat [rs,rd] , t )
  else 
    fail $ "E8S: response size error -> " ++ concatMap (printf " %02X") ( BS.unpack $ BS.concat [rs,rd] )

parse :: ECUData807d -> Frame
parse d =  {-# SCC "parse" #-} 
  -- if d == BS.empty || ( (toInteger (BS.length d) ) /= ( (toInteger $ BS.index d 0 ) ) ) then
  --   emptyFrame -- 時々取りこぼしが発生するようなので、フィルタリング
  -- else 
  let d8l = fromIntegral (BS.index d 0)
      d7l = fromIntegral (BS.head (BS.drop d8l d))
      d8 = BS.take d8l d
      d7 = BS.drop d8l d
  in  Frame
      { d80size     = d8l
      , d7dsize     = d7l
      , engineSpeed = 256 * fromIntegral (BS.index d8 1) + fromIntegral (BS.index d 2)
      , coolantTemp = -55 + fromIntegral (BS.index d8 3)  -- 0x03	Coolant temperature in degrees C with +55 offset and 8-bit wrap
      , ambientTemp = -55 + fromIntegral (BS.index d8 4)  -- 0x04	Computed ambient temperature in degrees C with +55 offset and 8-bit wrap
      , intakeATemp = -55 + fromIntegral (BS.index d8 5)  -- 0x05	Intake air temperature in degrees C with +55 offset and 8-bit wrap
      , fuelTemp    = -55 + fromIntegral (BS.index d8 6)  -- 0x06	Fuel temperature in degrees C with +55 offset and 8-bit wrap. This is not supported on the Mini SPi, and always appears as 0xFF.
      , mapSensor   = fromIntegral ( BS.index d8 7 )      -- 0x07	MAP sensor value in kilopascals
      , battVoltage = 0.1  * fromIntegral ( BS.index d8 8 ) -- 0x08	Battery voltage, 0.1V per LSB (e.g. 0x7B == 12.3V)
      , throttlePot = 0.02 * fromIntegral ( BS.index d8 9 ) -- 0x09	Throttle pot voltage, 0.02V per LSB. WOT should probably be close to 0xFA or 5.0V.
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

sndCmd7dto':: SerialPort -> ECUResponse Res
sndCmd7dto' port =  {-# SCC "send7D'" #-} do
  rc <- sendCommand port reqData7d
  rs <- tryIO 5 "Sending 7d" $ recv port 1
  rd <- repeatIO (size rs -1) (BS.pack []) ( tryIO 5 " Waiting 80 data " $ recv port 1 )
  t  <- ima
  if fromIntegral (BS.index rs 0 ) - 1 == BS.length rd then
    return ( BS.concat [rs,rd] , t )
  else 
    fail $ "E7S: response size error -> " ++ concatMap (printf " %02X") ( BS.unpack $ BS.concat [rs,rd] )

ecuSend:: SerialPort -> Word8 -> ECUResponse Int
ecuSend port cmd = do
  bytesRcvd <- ECUResponse ( Right `liftM` send port (BS.singleton cmd) )
  if  bytesRcvd == 0 then
    fail "E01: sending data error."
  else 
    pure bytesRcvd

ecuRecv:: SerialPort -> Int -> ECUResponse Res
ecuRecv port num = do
  result <- ECUResponse ( Right `liftM` recv port num ) 
  jikoku <- ima
  pure ( result , jikoku ) 

ecuFlush:: SerialPort -> ECUResponse Res
ecuFlush port    = do
  _       <- ECUResponse ( Right `liftM` flush port )
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
    case (byteRcvd == BS.empty, byteRcvd /= BS.singleton cmd ) of
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
        tryIO (n-1) s a 
      else
        pure r

tryIOwith1msecDelay :: Int -> String -> IO BS.ByteString -> ECUResponse Res
tryIOwith1msecDelay n s a
  | n <= 0    = fail $ "Count consumed while trying delay IO of " ++ s 
  | otherwise = do
      r       <- ECUResponse ( Right `liftM` a )
      jikokue <- ima
      if r == BS.empty then do
        ECUResponse (Right `liftM` threadDelay 1000)-- 1 msec
        tryIOwith1msecDelay (n-1) s a
      else
        pure (r,jikokue)

repeatIO :: Int -> BS.ByteString -> ECUResponse BS.ByteString -> ECUResponse BS.ByteString
repeatIO c r a 
  | c <= 0    = if BS.length r >= 0 then pure r else fail " RepeatIO Error" 
  | otherwise = do { r'<- a ; repeatIO (c-1) (BS.append r r') a}

getECUResponse :: SerialPort -> ECUResponse ECUData
getECUResponse p = do
  (r8,t8) <- sndCmd80to' p
  (r7,t7) <- sndCmd7dto' p
  return ECUData { status = GotData ( BS.concat [ r8, r7 ]) , at = t7 }

initialize ::FilePath -> ECUResponse (SerialPort, ModelData)
initialize ecupath = do {- 100msec Timeout when receiving a char in tenth of seconds = timeout * 0.1 sec -}
  pt <- ECUResponse ( Right `liftM` openSerial ecupath defaultSerialSettings {timeout= 1, flowControl = Software } )
  ecuFlush pt 
  sendCommand pt 0xca  -- 202 'ha no hankaku' 
  sendCommand pt 0x75  -- 117 'u' 
  sendHeartBt pt                      -- 244       - 0xf4 
  sendCommand pt 0xd0  -- 208 'mi no hankaku'
  -- recv p 4 だと取りこぼしが出る。何度もECUにリセットがかかっている様子（0.5秒間くらいのリレー音が繰り返される）-> repeatIO に
  -- md <- ECUResponse ( Right `liftM` recv port 4 ) -- IO ByteString -- recv :: SerialPort -> Int -> IO BS.ByteString
  md <- repeatIO 4 BS.empty (tryIO 5 " Reading Model Data " $ recv pt 1)
  if BS.length md /= 4 then
    fail $ "E4: Initialization error. model data is not 4 bytes:" ++ show md 
  else
    case data2model md of
      Nothing       -> fail $ "EM: Unknown model. the data is : " ++ show md
      Just theModel -> pure (pt , theModel )

sendHeartBt :: SerialPort -> ECUResponse Res
sendHeartBt port = sendCommandAndGetNByteRes port heartBeat 1 --  It should be 00 (\NUL)

-- moveECUIAC ::SerialPort 
--            -> Int             -- ^ desired position in 1 unsigned byte
--            -> ECUResponse Res
-- moveECUIAC port pos
--   | pos < currentPos                 = sendCommandANdGetNByteRes port moveIAC 1
--   | pos > currentPos && pos < IACmax = 

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
  
-- | VT100 カーソル移動 エスケープシーケンス　行　桁
vt100mv :: Int -> Int -> String 
vt100mv l c = "\ESC[" ++ show l ++ ";" ++ show c ++ "H"
-- | VT100 画面消去エスケープシーケンス ; m = 0 ... カーソルから, 1 ... カーソルまで, 2 ... 画面全体
vt100cr :: Int -> String
vt100cr m = "\ESC[" ++ show m ++ "J"
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

