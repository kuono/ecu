{-
Module      : Lib
Description : Ecu Communication Library for Rover Mini
Copyright   : (c) Kentaro UONO, 2018
License     : BSD3
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}

-- {-# LANGUAGE BangPatterns #-}
module Lib  ( runEcuAt, defaultUSBPath, testModeFile ) where

import System.Hardware.Serialport
import System.Directory
import System.IO -- for stdin, Buffering Mode
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Class
import Control.Concurrent
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
    -- import FRP.Sodium
    -- import qualified Graphics.UI.Threepenny   as UI
    -- import           Graphics.UI.Threepenny.Core
    -- import System.Console.ANSI
    -- import qualified UI.HSCurses.Curses as TW
    -- import Control.Monad.STM
    -- import Control.Concurrent.MVar
    -- import Control.Concurrent.STM.TChan
    
    ---------------------------
    -- type definition       --
    ---------------------------
    -- data ECUModel = MNE101170 | MNE101070 | MNE10078 | Others | ModelError String deriving (Show)
    -- ECUID of My Mini : 39 00 00 5C : 57 0 0 92
    --
    -- ASCII code lint ： http://www12.plala.or.jp/mz80k2/electronics/ascii/ascii.html
    -- 　　　　 　   　　： https://www.ibm.com/support/knowledgecenter/ja/ssw_aix_71/com.ibm.aix.networkcomm/conversion_table.htm
    -- printf format 　： https://www.k-cube.co.jp/wakaba/server/format.html
    --
    -- 【各種センサーの役割や信号の意味】  http://www.geocities.jp/dmxbd452/injection.html より
    -- クランク角センサー　約１，３～１，５KΩ　点火、燃料噴射の為の基本センサー　故障時　始動不良。
    --                　数Ωにショートしている場合、リレーモジュールよりカチャカチャ異音発生しアクセルONでも回転上がらず。
    -- MAPセンサー　　　　ECU内部に有るセンサー。点火時期、噴射時間調整の為の最重要センサー。　インテークマニホールドの負圧で電圧変化。
    --                 大気圧時　４．５３V　　最大負圧時　６９．７mV(ミリボルト）を発生する。　から
    --                 ホース切れ等の場合(負圧無し）　黒煙発生　点火時期不良(遅れる）　加速不良
    --                 電圧を発生しない場合　始動性が極端に悪くなるが(セル１０回でも始動するかな？）エンストしない、パワーは無い。　
    -- スロットルポジション　センサー(ポテンショメーター）
    --                 実測値(今回は電圧で表示しました。スロットルボディーに付いている状態で）
    --               　全閉時　約０，５V　　全開時　約３．０V　（これで　正常値です！故障だと思わないように！）
    --               　０V　又は　５V出力は　センサー故障を意味するので　ECUは、故障モードになり、センサー故障がメモリーされる。
    --               　故障時　加速不良　ACコンプレッサー作動不良。
    -- λ(ラムダ)センサー　O2センサー
    --                故障時　排気ガスが濃い　又は薄い　黒煙発生　エンジン不調　エンスト発生
    --                内部にヒーター有り。国産に変更出来ますがそのままだと作動しません。　
    -- 水温センサー,吸気温センサー Ω値特性は両方同じ　
    --   水温センサー　 冷間時　約３．０V（１０℃　点検日の気温。　通常２０℃の抵抗値で表示する）
    --                温間時　約０．４V（電動ファン作動時の水温時）
    --                ０V 　又は　５V出力時は、センサー故障とECUがみなし故障モードになり、故障がメモリーにインプットされる。
    --                水温センサー故障時は冷間時の始動不良になる。（キャブ車のチョーク作動不良と同じ）
    --                故障モードは、　水温６０℃　吸気温３５℃に設定　　　年式により黒煙を発生するＥＣＵ有り
    -- ステッパーモーター約１５Ω　　７．５度ステップのモーター　アイドリング回転数の調整用。　AC作動時、AT車のR、D時（P、N以外）、
    --                電気負荷の大きい時等アイドル・アップさせる。ハンダ割れ、水浸入による固着故障有り。
    -- PTCヒーター      インテーク・マニホールドを暖めるヒーター　水温が上昇するとOFFする。　時々焼損していて振るとカラカラ音の
    --                発生するPTC有り。冷間時の始動性にはあまり関係無い。排気ガス対策と思われます。理由は、スイッチＯＮでは作動
    --                しません。エンジン始動後ＰＴＣリレーがＯＮする。通電後数秒で手で触れない位熱くなる。
    
    -- 【ECU仕様の知識】
    -- https://www.minimania.com/A_Basic_Guide_to_Electronic_Fuel_Injection_for_Minis より
    -- For a typical Mini EFI conversion, the EFI ECU should receive information from the following sensors:
    -- * A throttle position sensor that tells the computer how hard your foot is on the accelerator pedal.
    -- * A crank angle sensor that basically tells the computer where the pistons are in their travels.
    -- * A knock sensor which detects any sign of detonation (pre-ignition or 'pinging') which is where the 
    --   air/fuel mixture is exploding violently inside the combustion chamber instead of as a controlled 
    --   progressive burning.
    -- * A Manifold Absolute Pressure (MAP) sensor which measures manifold vacuum (or boost!).
    -- * The engine speed in revolutions per minute, which, among other functions, informs the ECU whether 
    --   or not the rev limiter should be invoked.
    -- * An oil pressure sensor as an emergency input, if the oil pressure is too low, some EFI ECUs actually 
    --   turn the engine off after triggering a warning to the driver to restrict engine damage.
    -- * An engine temperature sensor to let the ECU know if a cold-start function is required where the idle 
    --   speed is increased along with different fuel and ignition settings are used until the engine has 
    --   reached a normal operating temperature.
    -- * An oxygen sensor that is plumbed into the exhaust manifold that examines the exhaust gases leaving 
    --   the engine and informs the ECU what the air/fuel ratio is.
    -- * Many other input sensors that can be used include the temperature of the intake air (especially 
    --   important for a Mini using forced induction), a sensor indicating which gear you have selected and 
    --   many more I don't have room to list.
    
data Frame80  = Frame80 {
                      size        :: Int , -- 0x00	Size of data frame, including this byte. This should be 0x1C (28 bytes) for the frame described here.
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
frame80Title  = "E Speed,coolant T,ambient T,intakeAir T,fuel T,map Sensor,btVolt,throtle Pot,idle Swch,0B,p/n switch, CTS E,IATS E, FPC E, TPC E,0F,10,11,iACMP,iSDev,15,ignAd,coil T,19,1A,1B"
frame80Fmt    = "%5d,%3d,%3d,%3d,%3d,%3d,%6.2f,%6.2f,%c,%02X,%c,%c,%c,%c,%c,%02X,%02X,%02X,%3d,%6d,%02X,%5.1f,%5.1f,%02X,%02X,%02X"
-- frame80Title1 = "=average(c5:c10000),=average(d5:d100000),=average(e5:e100000)"
-- frame80Title2 = "=stdev.p (c5:c100000),=stdev.p (e5:e100000),=stdev.p (e5:e100000)"
    
data Frame7d  = Frame7d {                -- Byte position, Field
                      size_7d       :: Int,  -- 0x00, size of data frame, including this byte
                      lambda_voltage:: Int,  -- This lambda value is a calculated value (if it is the same as the British emissions test).     And a value of, say, 1.05, suggests it is 5% too lean.   But, if your oxygen (and CO and HC) readings are all good, then it suggests your high lambda reading is because of a leak in the exhaust wgich pulls in fresh air (and oxygen).     You could try starting your car when it is cold and put your hand over the exhaust pipe and look underneath to see if water is leaking from any if the joints. 
                      closed_loop'  :: Int,  
                      fuel_trim'    :: Int 
                      -- idle_base_pos :: Int
    } deriving Show
    
type Data80      = BS.ByteString
type Data7d      = BS.ByteString
    
data UserCMD     = Table | Lines | Reconnect | Quit deriving (Show, Typeable) 
instance Ex.Exception UserCMD
    
    -- Ecu model and its identical data
data ModelData = ModelData { name :: String, mdb :: BS.ByteString , d8size ::Int , d7size ::Int}
instance Show ModelData where 
      show model = show $ name model
mne00000  = ModelData { name = "N/A", mdb = BS.empty , d8size = 0, d7size = 0 }
mne101070 = ModelData { name = "MNE101070", mdb = BS.pack [0x99, 0x00, 0x02, 0x03] , d8size = 28 , d7size = 32 } -- 153, 0, 2, 3 Manual SPI -        Cooper	 69493	103112
mne101170 = ModelData { name = "MNE101170", mdb = BS.pack [0x99, 0x00, 0x03, 0x03] , d8size = 28 , d7size = 32 } -- 153, 0, 3, 3 Manual SPI - Except Cooper	103113	134454
mne10078  = ModelData { name = "MNE10078 the Great Japanese Model!!" , mdb = BS.pack [0x39, 0x00, 0x00, 0x5c] , d8size = 28 , d7size = 14 {- =0x0E -} } 
    -- https://blogs.yahoo.co.jp/dmxbd452/5751726.html
    -- http://www.minispares.com/product/Classic/MNE101070.aspx
    -- Part number	Manual / Automatic	Attributes	              VIN No. From - VIN No. To 
    -- MNE10026	 Automatic	SPI -         Except Cooper	 	          - 59844
    -- MNE10027	 Manual    	SPI - Japan - Except Cooper	          	- 60487
    -- MNE10097	 Manual	    SPI -         Except Cooper - 1992-93	 	- 59586
    -- MNE10078	 Manual	    SPI - Japan - Cooper	 	                - 60487  #### これかな？ ####
    -- MNE10090	 Automatic	SPI -         Except Cooper	            59845	68084
    -- MNE101060 Automatic	SPI -         Except Cooper	            68085	103112
    -- MNE10092	 Manual	    SPI -         Cooper	                  60488	69492
    -- MNE101070 Manual	    SPI -         Cooper	                  69493	103112
    -- MNE10089	 Manual	    SPI - Except Cooper	                    59587	67377
    -- MNE101040 Manual	    SPI - Except Cooper	67378	103112
    -- MNE101150 Manual	    SPI - Except Cooper	103113	134454
    -- MNE101160 Automatic	SPI - Except Cooper	103113	134454
    -- MNE101170 Manual   	SPI - Except Cooper	103113	134454
    -- MNE101350 Manual	    SPI - Except UK - 1996 on	134455 - 	 
    -- MNE101351 Manual	    SPI - Except UK - 1996 on	134455 - 
    -- MNE101360 Automatic	SPI - Except UK - 1996 on	134455 - 	 
    -- MNE101361 Automatic	SPI - Air Con - Except UK - 1996 on	134455 - 	 
    
    -- ^ original number for size of Frame 7d = 32
    
data ECUData     = ECUData { status :: ECUStatus, at :: LocalTime , res :: Maybe ECUData807d }  
data ECUStatus   = Connected ModelData | NotConnected String 
instance Show ECUStatus where
  show d = case d of 
    Connected    mdl -> show mdl
    NotConnected msg -> show msg
data ECUData807d = ECUData807d { d80 :: Data80 , d7d :: Data7d }
instance Show ECUData807d where
  show d = show (d80 d) ++  show (d7d d)  
type ECUDataCh   = Chan ECUData
type ECUFrame    = (Frame80, Frame7d)
type ECUDataSet  = (ECUData, ECUDataLog )
data ECUDataLog  = ECUDataLog {
                         num  :: Integer     -- ^ 直前までの累積データ数 
                       , pdat :: ECUFrame    -- ^ 直前のデータ
                       , mind :: ECUFrame    -- ^ 最小値のセット。範囲を計算すればR管理図によるUCL,LCLがすぐ作れる？
                       , maxd :: ECUFrame    -- ^ 最大値のセット。但し，明らかな異常データははねる。
                       , logd :: Log
                       } 
type Log         = [String]
    
data ControlLimit = ControlLimit { ucl :: UCLSet, lcl :: LCLSet }
data UCLSet       = UCLSet { ucl80 ::Frame80, ucl7d ::Frame7d}
data LCLSet       = LCLSet { lcl80 ::Frame80, lcl7d ::Frame7d }
    
    -- | VT100 制御エスケープシーケンス
    -- | 　カーソル移動　行;桁  
vt100mv :: Int -> Int -> String
vt100mv l c = "\ESC[" ++ (show l) ++ ";" ++ (show c) ++ "H"
    -- | 画面を消去　ESC [ Ps J
vt100cr :: Int -> String
vt100cr m = "\ESC[" ++ (show m) ++ "J"
    -- ESC [ J	  カーソルから画面の終わりまでを消去
    -- ESC [ 0 J  同上
    -- ESC [ 1 J	画面の始めからカーソルまでを消去
    -- ESC [ 2 J	画面全体を消去
    
    -- init:: Model
    -- init = Model {
    --       ecuModel  = ModelData { name="Test", mdb = BS.empty , d8size = 0 , d7size = 0}
    --     , logFileh  = stderr
    --     , ecuStatus = Stopped
    -- }
    
emptyFrame80 :: Frame80
emptyFrame80 = Frame80 { 
      size = 0 , engineSpeed = 0 , coolantTemp = 0 , ambientTemp = 0 , intakeATemp = 0 ,
      fuelTemp = 0 , mapSensor = 0 , battVoltage = 0.0 , throttlePot = 0.0 , idleSwitch  = False ,
      unknown0B   = 0 , pnClosed    = False , faultCode1  = False , faultCode2  = False ,
      faultCode10 = False , faultCode16 = False , unknown0F   = 0 , unknown10   = 0 ,
      unknown11   = 0 , idleACMP    = 0 , idleSpdDev  = 0 , unknown15   = 0 , ignitionAd  = 0.0 ,
      coilTime    = 0.0 , unknown19   = 0 , unknown1A   = 0 , unknown1B   = 0
    }
    
emptyFrame7d :: Frame7d
emptyFrame7d = Frame7d {
      size_7d        = 0 , lambda_voltage = 0 , closed_loop'   = 0 , fuel_trim'     = 0
    }
    
controlLimit     = ControlLimit {
                         ucl = UCLSet { ucl80 = Frame80 {
                             size        = 0x1c  , engineSpeed = 6000  , coolantTemp = 100
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
                            size        = 0x1c  , engineSpeed = 200   , coolantTemp = 25
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
    
emptyD80 = BS.pack [0x1c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00] -- 28バイト
emptyD7d = BS.pack [0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00] -- 32バイト

initECUDataLog      = do
  time <- currentTime
  return ECUDataLog { num  = 0
                    , pdat = ( parse80 emptyD80, parse7d emptyD7d)
                    , mind = ( parse80 emptyD80, parse7d emptyD7d)
                    , maxd = ( parse80 emptyD80, parse7d emptyD7d)
                    , logd = [""] }
    
type Res           = BS.ByteString
type SetCmd        = Word8
type SData         = BS.ByteString
type SDataSet      = (SetCmd, SData)
type GetCmd        = Word8
g80 = 0x80 ::GetCmd -- get data for frame80 - 128
g7d = 0x7d ::GetCmd -- get data for frame7d - 125
gip = 0xfb ::GetCmd -- 236 
hbt = 0xf4 ::SetCmd -- 0xf4 244
cfd = 0xcc ::Word8  -- 204
    
testModeFile :: String
testModeFile = "TestData.csv"
    
runEcuAt :: String -> IO()
runEcuAt path = do 
  exist <- doesFileExist path
  if not exist then
    do { Prelude.putStrLn "Device does not exist. Check USB plug and/or serial cable connection." ; return () } 
  else
    do
      datach  <- newChan :: IO ECUDataCh
      datach' <- dupChan datach
      lid <- forkIO $ logger    datach  -- ^ fork logger thread
      did <- forkIO $ displayer datach' -- ^ fork displayer thread
      cid <- forkIO $ communicateWith path datach -- ^ fork communication thread

      hSetBuffering stdin NoBuffering -- set non buffering mode 
      hSetEcho      stdin True

      loopECUwith datach (lid, did, cid)     -- ^ event driven style main commander loop as follows
      where
        loopECUwith dch (lid, did, cid) = do
          ch <- getChar
          case ch of
            '\ESC'    -> do {Ex.throwTo lid (Quit) ; Ex.throwTo did Quit ; Ex.throwTo cid Quit ; Ex.throw Quit } -- ; return ()}
            'q'       -> do {Ex.throwTo lid (Quit) ; Ex.throwTo did Quit ; Ex.throwTo cid Quit ; Ex.throw Quit } -- ; return ()}
            '\''      -> do {Ex.throwTo lid (Quit) ; Ex.throwTo did Quit ; Ex.throwTo cid Quit ; Ex.throw Quit } -- ; return ()}
            -- ^ quit command issued. single quote key on dvorak layout is located at 'q' location of qwerty keyboard. 
            otherwise -> do -- {killThread cid ; cid' <- forkIO $ communicateWith path dch; loopECUwith dch (lid,did,cid') } 
                            {Ex.throwTo cid Reconnect ; loopECUwith dch (lid, did, cid) }
                    -- ^ kill communication thread 
                    --   いきなりkillするのは危険すぎないか，要検討。--> withSerial使っているから大丈夫？
                    --   ところがどっこい，キーを押すとその瞬間エンジンが瞬停する場合がある。
                    --   communicateWithにmask関数を導入。瞬停はいまのところない。
                    --   いきなりケーブルを引っこ抜く，その後にケーブルを接続しても再接続できない
                    --   (ポートビジー）状態でいるなどの時，アイドリング回転数が落ちた。再現性はあるか，不明。

-- withSerial' :: String -> SerialPortSettings -> ( SerialPort -> IO () ) -> IO ()
-- withSerial' dev settings = Ex.bracket 
--   {- open  -} (openSerial dev settings)
--   {- close -} (\port -> do
--                 closeSerial port
--                 withSerial' dev settings
--                 )
-- -- bracket:: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

-- | ECUとのコミュニケーションをする関数（というのかね，アクションばかりの場合も...)。
communicateWith :: FilePath -> ECUDataCh -> IO ()
communicateWith f dc = do
  if f == testModeFile then do
    composeAndWriteDataWithTestDataFile f dc
  else
    forever $ do
      -- Prelude.putStrLn $ "File path = " ++ (show f)
      -- forever $ do -- ^ ECUとのコミュニケーションが取れなくなっても再接続を永遠に試みる。
      -- withECU :: String -> SerialPortSettings -> ( SerialPort -> IO a ) -> IO a
      withECU f defaultSerialSettings {timeout= 5, flowControl = Software }  -- time out = 0.5sec
        -- ^ withSerialはbracketを使っているので例外発生時もポートを閉じてくれる。
        -- ^ ただし、例外処理を受け取ったら、そのまま動作を継続してくれるのか？おそらく違う。そうだとするとcatchは
        -- ^ どこに入れるべきか？ withSerialにかかるように入れてみた。
        -- ^ ポート開閉時以外は非同期例外を受け付けてしまうので、新たなラッピング関数 wethECU を導入。2019.03.15 
        -- ^ withECU も実態は bracket なので、非同期例外発生時もポートを閉じる。
        -- ^ また、
          $ \(ecu, model) -> Ex.mask $ \unmask -> do 
              -- Prelude.putStrLn $ "I will initialize ECU in withSerial" -- ++ (show ecu)
              -- m <- initialize ecu :: IO (ECUStatus)
                    forever $ {- Ex.mask $ \unmask -> -} do
                        jikoku <- currentTime :: IO LocalTime
                        r8 <- sndCmd80to' ecu 
                        case r8 of 
                          Left e  -> do 
                            writeChan dc $ ECUData { status = NotConnected ("E8: " ++ e) , at = jikoku , res = Nothing }
                            -- Ex.throwIO Reconnect
                          Right data80 -> do
                            if (BS.length data80) /= (fromIntegral $ BS.index data80 0) then do
                              writeChan dc $ ECUData { status = NotConnected $ "8S: D80 size was reported as " ++ show (BS.index data80 0 ) ++ " but actual size was " ++ show (BS.length data80)  , 
                                                      at = jikoku , res = Nothing }
                              -- Ex.throwIO Reconnect
                            else do
                              r7 <- sndCmd7Dto' ecu
                              case r7 of
                                Left e -> do
                                  writeChan dc $ ECUData { status = NotConnected ("E7: " ++ e) , at = jikoku , res = Just ECUData807d { d80 = data80, d7d = BS.empty } }
                                  -- communicateWith f dc
                                  -- Ex.throwIO Reconnect
                                Right data7d -> do
                                  if fromIntegral (BS.index data7d 0) == (BS.length data7d) then do 
                                    writeChan dc $ ECUData { status = Connected model , at = jikoku , res = Just ECUData807d { d80 = data80, d7d = data7d} }
                                  else do 
                                    writeChan dc $ ECUData { status = NotConnected $ "7S: D7D size was " ++ show (BS.length data7d) ++ " but reported as " ++ show (BS.index data7d 0 ) , 
                                                          at = jikoku , res = Nothing }
                                    -- Ex.throwIO Reconnect
  `Ex.catch`
            \e -> case (e::UserCMD) of 
                  Reconnect  -> do  --ima <- currentTime
                                  -- Prelude.putStrLn $ (vt100mv 0 0) ++ "restarting communication module because of reconnect command at " ++ show ima
                                  Prelude.putStr $ bred ++ white ++ "Try recconnect" ++ reset
                                  communicateWith f dc
                  -- Quit      -> do -- ima <- currentTime
  --                                 -- Prelude.putStrLn $ (vt100mv 0 0) ++ "quitting communication module because of quit command at " ++ show ima
  --                                 Ex.throwIO Quit
  --                                 return ()
  --                 otherwise -> do ima <- currentTime
  --                                 Prelude.putStrLn $ (vt100mv 0 0) ++ "quitting communication module because of something wrong at " ++ show ima
  --                                 return ()
  where
    withECU :: String -> SerialPortSettings -> ( (SerialPort,ModelData) -> IO a ) -> IO a
    withECU dev settings = Ex.bracket
      ( do 
          ecu <- openSerial dev settings
          stat <- initialize ecu :: IO (ECUStatus)
          case stat of
            Connected model -> return (ecu , model)
            NotConnected  _ -> Ex.throwIO Reconnect
      )
      ( \(ecu,_) -> closeSerial ecu )


composeAndWriteDataWithTestDataFile::FilePath -> ECUDataCh -> IO()
composeAndWriteDataWithTestDataFile p dc = do
  withFile p ReadMode loop
  where
      loop ::Handle -> IO ()
      loop h = do
        eof <- hIsEOF h
        if eof
          then return ()
          else do
            raw <- hGetLine h
            ima <- currentTime
            let d807d = compose raw
            case d807d of
              Right d  -> do
                writeChan dc $ ECUData (Connected mne00000) ima $ Just d 
                threadDelay 400000 -- 0.4 sec
                loop h
              Left m   -> do
                -- print m
                writeChan dc $ ECUData (NotConnected m ) ima $ Nothing
                threadDelay 400000 -- 0.4 sec
                loop h            

compose :: String -> Either String ECUData807d
compose d = -- Left m where m = show . length $ splitOn "," d 
        case splitOn "," d of
          [ d0, d00, d01, d02, d03, d04, d05, d06, d07, d08, d09,
            d10, d11, d12, d13, d14, d15, d16, d17, d18, d19,
            d20, d21, d22, d23, d24, d25, d26, d27, d28, d29 ]
                    -> Right $ ECUData807d {
                          d80 = BS.pack [
                            0x1c, -- 00
                            upperbyte ( read d01 ) :: Word8, -- 01 e speed
                            lowerbyte ( read d01 ) :: Word8, -- 02
                            ( readd d02 ) + 55 :: Word8, -- 03 cool t
                            ( readd d03 ) + 55 :: Word8, -- 04 amb t
                            ( readd d04 ) + 55 :: Word8, -- 05 int t
                            ( readd d05 ) + 55 :: Word8, -- 06 fuel t
                            ( readd ( d06 ) ) :: Word8,     -- 07 mapS
                            truncate (( read d07 ) * 10) :: Word8, -- 08 btv
                            truncate (( read d08 ) * 200) :: Word8, --09 thrott
                            if d09  == "T" then 16 :: Word8 else 0 :: Word8, --10 idlesw
                            ( readHex d10 ) :: Word8, -- 11 unknown 0b
                            if d11  == "T" then 0 :: Word8 else 0xff :: Word8, --12 pncls
                            (if d12 == "T" then 1 else 0 ) .|.
                            (if d13 == "T" then 2 else 0 ) :: Word8, -- 13 cool air
                            (if d14 == "T" then 1 else 0 ) .|.
                            (if d15 == "T" then 128 else 0 ) :: Word8, --14 fu / tn
                            ( readd d16 ) :: Word8 , -- 15
                            ( readHex d17 ) :: Word8 , -- 16
                            ( readHex d18 ) :: Word8 , -- 17
                            ( readHex d19 ) :: Word8 , -- 18 idleacmp
                            upperbyte ( read d20 ) :: Word8, -- 19
                            lowerbyte ( read d20 ) :: Word8, -- 20
                            ( readHex d21 ) :: Word8 , -- 21
                            ( readd d22 ) * 2 + 24 :: Word8 , -- 22
                            upperbyte . fromIntegral $ truncate ((read d23 ) * 500) :: Word8, -- coilt u
                            lowerbyte . fromIntegral $ truncate ((read d23 ) * 500) :: Word8,
                            ( readHex d24 ) :: Word8,
                            ( readHex d25 ) :: Word8,
                            ( readHex d26 ) :: Word8] ,
                          d7d = BS.pack [
                            0x20, -- 00
                            0x00,0x00,0x00,0x00,0x00,
                            ( readd d27 ), -- 06
                            0x00,0x00,0x00,
                            ( readd d28 ), -- 0a
                            0x00,
                            ( readd d29 ),  --0c
                            0x00,0x00,0x00,
                            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0,0,0,0,0,0,0
                          ]
                        }
                        where 
                          readd::String -> Word8
                          readd s = readd' $ reverse s
                            where 
                              readd'::String -> Word8
                              readd' []     = 0
                              readd' [u]    = hextoint u
                              readd' (f:r)  = hextoint f + 10 * (readd' r) 
                          upperbyte::Int -> Word8
                          upperbyte p = if p > 0xffff then 0xff else fromIntegral $ p `div` 256
                          lowerbyte::Int -> Word8
                          lowerbyte p = if p > 0xffff then 0xff else fromIntegral ( p - fromIntegral ( upperbyte p ))
                          readHex::String -> Word8
                          readHex i = 
                            case length i of 
                              0   -> 0
                              1   -> hextoint $ head i
                              2   -> 16 * ( hextoint $ head i) + (hextoint (head (tail i) ))
                              otherwise -> 0xFF
                          hextoint ::Char -> Word8
                          hextoint c = 
                            case c of 
                              '0' -> 0
                              '1' -> 1
                              '2' -> 2
                              '3' -> 3
                              '4' -> 4
                              '5' -> 5
                              '6' -> 6
                              '7' -> 7
                              '8' -> 8
                              '9' -> 9
                              'A' -> 10
                              'B' -> 11
                              'C' -> 12
                              'D' -> 13
                              'E' -> 14 
                              'F' -> 15
                              otherwise -> 0
          
          otherwise -> Left d

logger :: ECUDataCh -> IO()
logger dch = do
  -- Prelude.putStrLn "Now logger started."
  logfn <- logFileName :: IO (FilePath)
  withFile logfn WriteMode
    $ \logfh -> do
        -- hPutStrLn logfh  $ "Date,Time," ++ frame80Title ++ "," ++ frame7DTitle
        -- hPutStrLn logfh  $ ",," ++ frame80Title1
        -- hPutStrLn logfh  $ ",," ++ frame80Title2
        -- hFlush logfh
        forever $ do
          d <- readChan dch
          case status d of 
            Connected model -> {- Ex.mask $ \unmask -> -} do
              case res d of
                Just d807d -> do
                  let j  = localTimetoString $ at d
                      d8 = frame80toTable.parse80 $ d80 d807d
                      d7 = frame7DtoTable.parse7d $ d7d d807d
                  hPutStrLn logfh  $ j ++ "," ++ d8 ++ "," ++ d7
                otherwise  -> do
                  hPutStrLn logfh $ "At " ++ show ( at d ) ++ ", While in a loop, Illegan Nothing data."
            NotConnected msg -> do
              hPutStrLn logfh  $ "At " ++ show ( at d ) ++ msg
    `Ex.catch`(\e -> case e of 
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
        logger dch
      -- | Table | Lines| Reconnect
      otherwise -> logger dch )
      -- ^ ここでQUITエラーを捕まえていることになる？　withFileで例外は捕まえられない？ <- 捕まえている
    
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
              -- ./log/ECU...としていたが，ディレクトリが存在していないとランタイムエラーを起こすので変更
    
displayer :: ECUDataCh -> IO ()
displayer dch = do
        Prelude.putStr $ vt100cr 2    -- 画面全体消去   -- Prelude.putStrLn $ "displayer Started."
        -- Prelude.putStr $ vt100mv 2 0  -- 2行0桁へ移動
        il <- initECUDataLog     -- ^ initial empty min-max data
        loop dch il `Ex.catch`(\e -> case (e::UserCMD) of
          Quit      -> do { Prelude.putStrLn $ vt100cr 2 ; return () }
          Reconnect  -> displayer dch 
          otherwise -> Ex.throw e )
        where
          -- これらの処理はディスクへの書き出しをしているLoggerとほぼ同じ，かつ結構時間を食う処理なので
          -- パイプ処理にして途中で変換し，その結果をLoggerもdisplayerも引き取るように改造するべきか。
          -- フォールトコードの表示や最新情報の表示をつけたので，全く同じではなくなったけど...。
          loop :: ECUDataCh -> ECUDataLog -> IO ()
          loop dch dl = do
            d <- readChan dch 
            let dl' = renew dl d
            tableView dl' d
            loop dch dl'
    
renew :: ECUDataLog -> ECUData -> ECUDataLog
renew dl d = case status d of 
                  Connected m -> case res d of
                    Nothing -> dl { logd = "Connected but data was empty." : logd dl }
                    Just d' ->
                      let nnum   = 1 + num dl
                          ndat   = (parse80.d80 $ d' , parse7d.d7d $ d') :: ECUFrame
                          ndf    = select d dl
                          nminds = snd ndf
                          nmaxds = fst ndf
                          nlogd  = case checklog of 
                                    Nothing -> logd dl
                                    Just l' -> l':(logd dl)
                      in ECUDataLog { num = nnum, pdat = ndat, mind = nminds, maxd = nmaxds, logd = nlogd }
                       where select:: ECUData -> ECUDataLog -> (ECUFrame,ECUFrame) -- ^ Max,Min
                             select d dl = ((n8x,n7x),( n8n , n7n ) )
                               where  n8x = emptyFrame80  { 
                                                 engineSpeed = max (engineSpeed.parse80.d80 $ d' ) (engineSpeed.fst.maxd $ dl) :: Int
                                               , coolantTemp = max (coolantTemp.parse80.d80 $ d' ) (coolantTemp.fst.maxd $ dl) :: Int
                                               , ambientTemp = max (ambientTemp.parse80.d80 $ d' ) (ambientTemp.fst.maxd $ dl) :: Int
                                               , fuelTemp    = max (fuelTemp.parse80.d80    $ d' ) (fuelTemp.fst.maxd    $ dl) :: Int
                                               , mapSensor   = max (mapSensor.parse80.d80   $ d' ) (mapSensor.fst.maxd   $ dl) :: Int
                                               , battVoltage = max (battVoltage.parse80.d80 $ d' ) (battVoltage.fst.maxd $ dl) :: Float
                                               }
                 　                    n7x = emptyFrame7d {
                                                 lambda_voltage = max (lambda_voltage.parse7d.d7d $ d' ) (lambda_voltage.snd.maxd $ dl)
                                               , closed_loop'   = max (closed_loop'.parse7d.d7d   $ d' ) (closed_loop'.snd.maxd   $ dl)
                                               , fuel_trim'     = max (fuel_trim'.parse7d.d7d     $ d' ) (fuel_trim'.snd.maxd     $ dl) 
                                               }
                                      n8n = emptyFrame80 { 
                                                 engineSpeed = min (engineSpeed.parse80.d80 $ d') (engineSpeed.fst.mind $ dl) :: Int
                                               , coolantTemp = min (coolantTemp.parse80.d80 $ d') (coolantTemp.fst.mind $ dl) :: Int
                                               , ambientTemp = min (ambientTemp.parse80.d80 $ d') (ambientTemp.fst.mind $ dl) :: Int 
                                               , fuelTemp    = min (fuelTemp.parse80.d80    $ d') (fuelTemp.fst.mind    $ dl) :: Int
                                               , mapSensor   = min (mapSensor.parse80.d80   $ d') (mapSensor.fst.mind   $ dl) :: Int
                                               , battVoltage = min (battVoltage.parse80.d80 $ d') (battVoltage.fst.mind $ dl)
                                               }
                 　                    n7n = emptyFrame7d {
                                                 lambda_voltage = min (lambda_voltage.parse7d.d7d $ d') (lambda_voltage.snd.mind $ dl)
                                               , closed_loop'   = min (closed_loop'.parse7d.d7d   $ d') (closed_loop'.snd.mind   $ dl)
                                               , fuel_trim'     = min (fuel_trim'.parse7d.d7d     $ d') (fuel_trim'.snd.mind     $ dl) 
                                               }
    
                             checklog = if (engineSpeed $ parse80 $ d80 $ d') <= (engineSpeed $ ucl80 $ ucl controlLimit) 
                                        then Nothing else Just ("Error (Illegal Engine Speed Data ) at : " ++ (show (at d)))
                  NotConnected msg -> dl { logd = ("At " ++ show (at d) ++ msg ): logd dl }
        
lineView::ECUDataSet -> IO()
lineView ds@(d,l) = do
      ima <- currentTime
      case status d of
        NotConnected m -> do { Prelude.putStrLn $ "At " ++ show ima ++ m }
        Connected m    -> do
          case res d of 
            Nothing -> return ()
            Just d' -> do
              Prelude.putStrLn $  j ++ "," ++  d8 ++ "," ++ d7   -- wAddStr :: Window -> String -> IO ()    標準の画面に文字を出力
              where j  = localTimetoString  $ at d
                    d8 = frame80toTable.parse80.d80 $ d'
                    d7 = frame7DtoTable.parse7d.d7d $ d'
    
-- | Escape sequence in VT100
reset    = "\ESC[0m"
brev     = "\ESC[7m"  -- ^ set reverse
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
    
tableView::ECUDataLog -> ECUData -> IO()
tableView l d  = do
  ima <- currentTime
  -- case status d of
  --   NotConnected msg -> do { Prelude.putStrLn $ vt100mv 0 0 ; Prelude.putStrLn $ brev ++ msg ++ reset }
  --   Connected m   ->
  case res d of 
    Nothing  -> do
      Prelude.putStr "\BEL\BEL\BEL"
      Prelude.putStrLn $ vt100mv 0 0
      Prelude.putStrLn   "-----------------------------------------------"
      Prelude.putStrLn   " MyEcu: Copyright (C)2018-2019 by Kentaro UONO "
      Prelude.putStrLn   "-----------------------------------------------"
      Prelude.putStrLn $ bred ++ yellow ++ " Not Connected " ++ reset ++ (printf " %3s " ( take 3 $ show $ status d ) ) ++ "at: " ++ (show $ at d) 
      return () 
    Just d'  -> do -- ds@(d,l) = do
      Prelude.putStrLn $ vt100cr 2
      Prelude.putStrLn $ vt100mv 0 0
      Prelude.putStrLn   "-----------------------------------------------"
      Prelude.putStrLn   " MyEcu: Copyright (C)2018-2019 by Kentaro UONO "
      Prelude.putStrLn   "-----------------------------------------------"
      Prelude.putStrLn $ connected ++ "   Model       " ++ reset ++ ( printf  " %s " modelstr )
      Prelude.putStrLn $ reset ++ "-------------- time ---------------------------"
      Prelude.putStrLn $ (printf " %6s " numl) ++ (printf " %6s " numr) ++ ": " ++ j
      Prelude.putStrLn   "-------------- 80 data ------------------------"
      Prelude.putStrLn $ (printf "   Engine Speed: %5d"   (engineSpeed d8 )) ++ ( bar   0  4000 ( engineSpeed d8 ))
      Prelude.putStrLn $ (printf "throttle Potent: %5.2f" (throttlePot d8 )) ++ ( bar   0    50 ( truncate ( 10 * (throttlePot d8 ))))
      Prelude.putStrLn $ (printf "   Coolant Temp:   %3d" (coolantTemp d8 )) ++ ( bar (-55) 100 ( coolantTemp d8 ))
      Prelude.putStrLn $ (printf "   ambient Temp:   %3d" (ambientTemp d8 )) ++ ( bar (-55) 100 ( ambientTemp d8 ))
      Prelude.putStrLn $ (printf "intake Air Temp:   %3d" (intakeATemp d8 )) ++ ( bar (-55) 100 ( intakeATemp d8 ))
      Prelude.putStrLn $ (printf "     map Sensor:   %3d" (mapSensor   d8 )) ++ ( bar   0   130 ( mapSensor   d8 ))
      Prelude.putStrLn $ volt                                                ++ ( bar   0   200 ( truncate ( 10 * ( battVoltage d8 ))))
      Prelude.putStrLn $ (printf "    idle switch: %5s" (tf (idleSwitch  d8 )))
      Prelude.putStrLn $ (printf "     pin closed: %5s" (tf (pnClosed    d8 )))
      Prelude.putStrLn $ (printf "idl Air Ctl M P:   %3d" (idleACMP    d8 )) ++ (bar   0  200 ( idleACMP   d8 ))
      Prelude.putStrLn $ (printf "idl Spd deviatn: %5d"   (idleSpdDev  d8 )) ++ (bar   0 1000 ( idleSpdDev d8 ))
      Prelude.putStrLn $ (printf "ignition advnce: %5.2f" (ignitionAd  d8 )) ++ (bar   0   20 ( truncate (ignitionAd d8 )))
      Prelude.putStrLn $ (printf "      coil Time: %5.2f" (coilTime    d8 )) ++ (bar   0   20 ( truncate (coilTime   d8 )))
      Prelude.putStrLn   "-------------- 7D data ------------------------"
      Prelude.putStrLn $ (printf " lambda voltage:   %3d" (lambda_voltage d7 )) ++ ( bar 0 1000 ( lambda_voltage d7 ))
      Prelude.putStrLn $ (printf "    closed loop:   %3d" (closed_loop'   d7 )) -- ++ (printf "   %3d    %3d" (closed_loop'   d7x) (closed_loop'   d7m) )
      Prelude.putStrLn $ (printf "      fuel trim:   %3d" (fuel_trim'     d7 )) ++ (bar  0 500 ( fuel_trim' d7 )) --(printf "   %3d    %3d" (fuel_trim'     d7x) (fuel_trim'     d7m) )
      Prelude.putStrLn $ "- Fault Code ----------------------------------"
      Prelude.putStrLn $ " (01) Coolant temp Sensor      | " ++ e01
      Prelude.putStrLn $ " (02) Air temp sensor          | " ++ e02
      Prelude.putStrLn $ " (10) Fuel pump cirkit         | " ++ e10
      Prelude.putStrLn $ " (16) Throttle position sensor | " ++ e16
      Prelude.putStrLn   "----------------- Log -------------------------"
      mapM_ Prelude.putStrLn $ if length log >= 5 then  take 5 log else log 
      -- Prelude.putStrLn   "-----------------------------------------------"
      where 
        connected ::String
        connected = case status d of 
                      Connected _ -> yellow ++ bgreen 
                      otherwise   -> white  ++ bred 
        modelstr::String
        modelstr = case status d of
                      Connected r -> show r
                      otherwise -> ""
        bar::Int -> Int -> Int -> String
        bar min max x = printf "  %-10s" $ take (fromIntegral (10 * x `div` (max-min))) "**********"
        tf c = if c then green ++ "True " ++ reset else red ++ "False" ++ reset
        nums = printf "%6s" ( show $ num l )
        numr = if odd $ num l then (bwhite ++ black ++ nums ++ reset) else nums
        numl = if not . odd $ num l then (bwhite ++ black ++ nums ++ reset ) else nums
        j    = localTimetoString $ at d
        d8   = parse80.d80 $ d'
        -- d8x  = fst $ maxd l
        -- d8m  = fst $ mind l
        d7   = parse7d.d7d $ d'
        -- d7x  = snd $ maxd l
        -- d7m  = snd $ mind l
        b    = battVoltage d8
        bs   = printf "battery Voltage: %5.2f" b
        bell = if (b > 15.0 || b < 9.0 ) then "" else "" -- \BEL\BEL\BEL" else "\BEL"  
        volt = if (b > 15.0 || b < 9.0 ) then red ++ bs ++ reset else bs
        log  = logd l
        e01 = if faultCode1 d8  then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (01) Coolant temp Sensor 
        e02 = if faultCode2 d8  then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (02) Air temp sensor 
        e10 = if faultCode10 d8 then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (10) Fuel pump cirkit 
        e16 = if faultCode16 d8 then green ++ "__" ++ reset else red ++ "■■" ++ reset -- (16) Throttle position sensor 
        -- let colour = if (b > 15.0 || b < 9.0 ) then red else black 

grafficalView::ECUData -> ECUDataLog -> IO()
grafficalView d l = undefined

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
  
localTimetoString :: LocalTime -> String
-- to convert constant length string
localTimetoString (LocalTime n t) = 
  let hi     = show    n
      ji     = todHour t
      hun    = todMin  t
      byo    = todSec  t
      byo'   = take 9 $ if byo >= 10 then showFixed False byo
                            else '0':(showFixed False byo) 
  in printf "%10s,%02d:%02d:%9s" hi ji hun byo'

---------------------------
-- constants definition  --
---------------------------
-- USB-serial related
defaultUSBPath   = "/dev/tty.usbserial-DJ00L8EZ" -- :: FilePath
alterntUSBPath   = "/dev/tty.usbserial-FT90HWC8" -- :: FilePath
zero             = BS.singleton 0x00 :: BS.ByteString

-- | Initialization related data
-- | initCommand :: [BS.ByteString]
initCommand = [0xca, 0x75, 0xd0] :: [Word8] -- [202,117(u),208]

sndCmd80to':: SerialPort -> IO (Either String Res)
sndCmd80to' port = {-# SCC "send80'" #-} do
  r80 <- sendCommand port $ BS.singleton g80 :: IO (Either String Int)
  case r80 of
    Left e    -> return $ Left e
    otherwise -> do
      size <- tryIO 5 $ recv port 1 
      if size == BS.empty then do
        jikoku <- currentTime
        return $ Left $ "Sent Get80 but the response was empty at :" ++ (show jikoku)
      else do
        let l = fromIntegral (BS.head size) -- ^ タクトタイムを短くするために導入してみた
        if l < 1 then
          return $ Left $ "The size of Command 80 response was too short:" ++ show l
        else 
          do
            res <- repeatIO (l - 1) (BS.pack []) $ recv port 1 -- :: IO BS.ByteString
            return $ Right $ BS.concat [size,res]

tryIO :: Int -> IO(BS.ByteString) -> IO(BS.ByteString)
tryIO n a
  | n <= 0    = return BS.empty
  | otherwise = do
                  r <- a
                  if r == BS.empty then
                    tryIO (n-1) a
                  else 
                    return r

tryIOwith10msecDelay :: Int -> IO (BS.ByteString) -> IO (BS.ByteString)
tryIOwith10msecDelay n a
  | n <= 0    = return BS.empty
  | otherwise = do
                  r <- a
                  if r == BS.empty then do
                    threadDelay 10000 -- 10msec
                    tryIOwith10msecDelay  (n-1) a
                  else 
                    return r


repeatIO :: Int -> BS.ByteString -> IO BS.ByteString -> IO BS.ByteString
repeatIO c r a 
  |c <= 0    = return r
  |otherwise = do { r'<- a ; repeatIO (c-1) (BS.append r r') a}

parse80 :: BS.ByteString -> Frame80
parse80 d =  {-# SCC "parse80" #-} 
  if d == BS.empty || ( (toInteger (BS.length d) ) /= ( (toInteger $ BS.index d 0 ) ) ) then
    emptyFrame80 -- 時々取りこぼしが発せするようなので、フィルタリング
  else 
    Frame80 {
          size        = toInt $ BS.index d 0,
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

toInt = fromIntegral . toInteger
  
sndCmd7Dto':: SerialPort -> IO (Either String Res)
sndCmd7Dto' port =  {-# SCC "send7D'" #-} do
  r7d <- sendCommand port $ BS.singleton g7d :: IO (Either String Int)
  case r7d of
    Left e    -> return $ Left e
    otherwise -> do
      size   <- tryIO 5 $ recv port 1
      jikoku <- currentTime
      if size == BS.empty then do
        return $ Left $ "The response was empty at :" ++ show jikoku 
      else do
        let l = fromIntegral (BS.head size)
        if l < 1 then
          return $ Left $ "The size of response was too short:" ++ show l ++ " at " ++ show jikoku
        else do
          res <- repeatIO (l - 1) (BS.pack []) $ recv port 1 -- :: IO BS.ByteString
          return $ Right $ BS.concat [size,res]  
      
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

currentTime :: IO LocalTime
currentTime = do
  timezone <- getCurrentTimeZone
  utctime  <- getCurrentTime
  return $ utcToLocalTime timezone utctime

dispHex :: String -> BS.ByteString -> IO ()
dispHex t d = do
  Prelude.putStr t
  mapM_ (printf " %02X") $ BS.unpack d

dispHexLn :: String -> BS.ByteString -> IO ()
dispHexLn t d = dispHex t d >> Prelude.putStrLn ""

sendCommand :: SerialPort -> BS.ByteString -> IO (Either String Int)  
sendCommand port cmd = do
    byteSend  <- send port cmd -- SerialPort -> B.ByteString -> IO Int
    byteRcvd  <- tryIOwith10msecDelay 3 $ recv port 1 -- IO ByteString
    case (byteRcvd == BS.empty, byteRcvd == cmd) of
      (True, _) -> return $ Left  $ "EC: ECU did not respond for:" ++ show cmd
      (_,False) -> return $ Left  $ "EE: ECU responded illegally. " ++ show ( BS.unpack byteRcvd )++ " for:" ++ show ( BS.unpack cmd )
      (_,_)     -> return $ Right 1 

sendHeartBt :: SerialPort -> IO (Either String ())
sendHeartBt port = do
  byteSend  <- sendCommand port $ BS.singleton hbt
  byteRcvd  <- recv port 1 -- IO ByteString : It should be 00 (\NUL)
  case (BS.length byteRcvd == 0, byteRcvd == BS.singleton 0 ) of
    (True, _  ) -> return  $ Left $ "EB: ECU did not responded heart beat as" ++ show hbt
    (_, False ) -> return  $ Left $ "EB: ECU echoed beat (" ++ show hbt ++ ") but not matched with " ++ show byteRcvd
    (_, True  ) -> return  $ Right ()

initialize ::SerialPort -> IO (ECUStatus)
initialize port  = do
  -- Prelude.putStrLn $  "Now I will start initializing ECU"
  flush port
  jikoku1 <- currentTime
  r1 <- sendCommand port $ BS.singleton 0xca  -- 202 'ha no hankaku' 
  case r1 of
    Left e   -> return $ NotConnected $ "E1: Initialization error when sending 1st command. " ++ e ++ "at " ++ show jikoku1
    oherwise -> do
      jikoku2 <- currentTime
      r2 <- sendCommand port $ BS.singleton 0x75  -- 117 'u' 
      case r2 of
        Left e   -> return $ NotConnected $ "E2: Initialization error when sending 2nd command. " ++ e ++ "at " ++ show jikoku2
        oterwise -> do
          jikokubt <- currentTime
          bt <- sendHeartBt port              -- It is heart beat. 0xf4 244
          case bt of
            Left e   -> return $ NotConnected $ "Eb: Initialization error when sending heart beat.  " ++ e ++ "at " ++ show jikokubt
            oterwise -> do
              jikoku3 <- currentTime
              r3 <- sendCommand port $ BS.singleton 0xd0  -- 208 'mi no hankaku'
              case r3 of 
                Left e    -> return $ NotConnected $ "E3: Initialization error when sendning heart beat. " ++ e ++ "at " ++ show jikoku3
                otherwise -> do
                  jikokum <- currentTime
                  modeldata <- repeatIO 4 BS.empty (recv port 1)
                  -- modeldata <- recv port 4 -- IO ByteString -- recv :: SerialPort -> Int -> IO BS.ByteString
                  -- 上記のように単純に４バイトを指定して読み込むと，取りこぼしを起こすので，下記のよう改善。
                  -- dispHex "modeldata :" modeldata
                  if (BS.length modeldata == 4 ) then
                    case data2model modeldata of
                      Just theModel -> return $ Connected theModel
                      otherwise     -> return $ NotConnected $ "EM: Unknown model. the data is : " ++ show modeldata ++ "at " ++ show jikokum
                  else
                    return $ NotConnected $ "E4: Initialization error. model data is not 4 bytes:" ++ show modeldata ++ "at " ++ show jikokum

data2model :: BS.ByteString -> Maybe ModelData
data2model key    | key == mdb mne10078  = Just mne10078
                  | key == mdb mne101070 = Just mne101070
                  | key == mdb mne101170 = Just mne101170
                  | otherwise            = Nothing

onFaultCodesClearRequested::IO()
onFaultCodesClearRequested = 
  clearFaults

clearFaults::IO()
clearFaults = undefined
-- void MEMSInterface::onFaultCodesClearRequested()
-- {
--   if (m_initComplete && mems_is_connected(&m_memsinfo))
--   {
--     if (mems_clear_faults(&m_memsinfo))
--     {
--       emit faultCodesClearSuccess();
--     }
--     else
--     {
--       emit errorSendingCommand();
--     }
--   }
--   else
--   {
--     emit notConnected();
--   }
-- }