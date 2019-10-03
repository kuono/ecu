{- |
Module      : UI
Description : Text User Interface Library for Rover Mini ECU Monitor
Copyright   : (c) Kentaro UONO, 2019
License     : N/A
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}

module UI where

import Lib
import qualified ECU
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as Ex
import qualified Data.ByteString   as BS
import Data.Time.LocalTime
import Text.Printf

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.ProgressBar as BP
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified TextPlot as G (PlotConfig(..),ParamFunction(..),Function,(.+),(.|),(.-),
                 plotWithConfig,emptyXYPlot,)
import Linear.V2 (V2(..))
import Lens.Micro ((^.))   
--
maxGraphLength = 40
-- 
-- UI Name space
-- 
data Display = Dialog | DataPanel | CurrentStatus | CurrentData | GraphLog | BarLog | TextLog
--
-- UI Attribute Map
--
normalAttr = attrName "normalAttr"
errorAttr  = attrName "errorAttr"
alertAttr  = attrName "alertAttr"
pgcompAttr = attrName "progressComplete"
pgtodoAttr = attrName "progressIncomplete"
espeedAttr = attrName "espeedAttr"
thpotAttr  = attrName "thpotAttr"
msensAttr  = attrName "msensAttr"
batvAttr   = attrName "batvAttr"
mnotselectedAttr = attrName "menuisnotselectedAttr"
mselectedAttr   = attrName "menuisselectedAttr"
--
theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (normalAttr, V.white `on` V.black)          -- ^ normalAttr : 黒背景色に白字
    , (errorAttr,  V.red `on` V.yellow)           -- ^ errorAttr  : 黄背景色に赤字
    , (alertAttr,  fg V.red `V.withStyle` V.bold) -- ^ alertAttr  : 赤字；Boldはよくわからない
    , (pgcompAttr, bg V.red)
    , (pgtodoAttr, bg V.white )
    , (espeedAttr, fg V.white)
    , (thpotAttr,  fg V.green)
    , (msensAttr,  fg V.red)
    , (batvAttr,   fg V.blue)
    , (mnotselectedAttr, V.white `on` V.black )
    , (mselectedAttr,    V.black `on` V.white )
    ]

-- Draw functions
--
drawInitialScreen :: String -- ^ current version
                  -> String -- ^ compiled date
                  -> Widget Name
drawInitialScreen currentVersion compiledOn =  
      str   "    \\              '             `"
  <=> str   "     .              .             `     ,_ .`"
  <=> str   "--_,   ,            |              /  /`. +'.'"
  <=> str   "+ + \". .===========================w. || = =  "
  <=> str   "= = :|V-Monitor for Rover Mini MEMS-\\\\ \\.+_+.'"
  <=> str   "- -,\"|:--------- ------------ -------:|     |"
  <=> str ( ( take 37 ( " .   \\\\-Version " ++ currentVersion ++ " on " ++ compiledOn )) ++ "-//   .  ." )
  <=> str   "`.`  .^.== +--------------------+ == .`.'.` ,"
  <=> str   "-----| |-- |  by Kentaro UONO   |----| |-''' "
  <=> str   "````  '    +--------------------+     '"
--
--
-- Drawing
--
drawPanes :: Status -> [Widget Name]
drawPanes ecu =
    [ withBorderStyle BS.unicodeRounded $ B.borderWithLabel (str "Rover Mini MEMS Monitor") $
            vLimit 1
            ( drawEcuStatus ecu <+> drawTime ecu )
        <=> drawMenu ecu
        <=> draw807dData ecu--  <+> B.vBorder
        <=> B.hBorderWithLabel ( str "Fault Status | IAC Pos")
        <=> drawEcuFaultStatus ecu <+> drawIACPos ecu
        <=> B.hBorderWithLabel ( str "Note")
        <=> drawEcuErrorContents ecu 
        <=> drawNote ecu
         
    ]
--
drawNote :: Status -> Widget Name
drawNote s = str $ note $ rdat s
--
drawEcuStatus :: Status -> Widget Name
drawEcuStatus s = viewport StatusPane Horizontal $ hLimit 30 $
  case event s of
    ECU.PortNotFound _ -> withAttr errorAttr  $ str  " Port Not Found               " <+> B.vBorder
    ECU.Connected m    -> withAttr normalAttr $ str (" Connected. " ++ (ECU.mname $ model s) ++ "(" ++ show (ECU.d8size $ model s) ++ "," ++ show (ECU.d7size $ model s) ++ ")") <+> B.vBorder
    ECU.OffLined       -> withAttr alertAttr  $ str  " Off Line                     " <+> B.vBorder
    ECU.Tick r         -> withAttr normalAttr $ str (" Connected. " ++ (ECU.mname $ model s) ++ "(" ++ show (ECU.d8size $ model s) ++ "," ++ show (ECU.d7size $ model s) ++ ")") <+> B.vBorder
    ECU.GotIACPos _    -> withAttr normalAttr $ str (" Connected. " ++ (ECU.mname $ model s) ++ "(" ++ show (ECU.d8size $ model s) ++ "," ++ show (ECU.d7size $ model s) ++ ")") <+> B.vBorder
    ECU.Error s        -> withAttr errorAttr  $ str (" Error             : " ++ s ) <+> B.vBorder
    _                  -> emptyWidget
--
drawEcuErrorContents :: Status -> Widget Name
drawEcuErrorContents s = viewport ErrorContentsPane Horizontal $ 
    case event s of
      ECU.PortNotFound f -> withAttr errorAttr  $ str f
      ECU.Connected m    -> withAttr normalAttr $ str $ ECU.mname m
      ECU.OffLined       -> withAttr errorAttr  $ str "     "
      ECU.Tick _         -> emptyWidget
      ECU.Error s        -> withAttr errorAttr  $ str s
      _                  -> emptyWidget
--
drawTime :: Status -> Widget Name
drawTime s = withAttr ( if odd' s then mselectedAttr else mnotselectedAttr ) $ tstr s
  where 
    sec (LocalTime _ t') = truncate $ todSec t'
    time = fst . evnt . rdat
    odd' = odd . sec . time
    tstr = str . take 22 . show . time
--
draw807dData :: Status -> Widget Name
draw807dData s = vLimit 21 $ case event s of
  ECU.PortNotFound p -> drawInitialScreen ver date-- hLimit 60 $ vBox [drawData s]
  _                  -> -- ECU.OffLine or ECU.OnLine 
        B.hBorderWithLabel ( str "Data 80/7D" ) 
    <=> ( hLimit 41 (drawData s) <+> B.vBorder <+> drawGraph s )
--
drawBar :: Status -> Widget Name
drawBar s = {- vLimit 19 $ hLimit 20 $ -} vBox [
  case event s of
    ECU.PortNotFound p -> emptyWidget
    _                  -> -- ECU.OffLine or ECU.OnLine
      B.borderWithLabel (str "Graph") $ viewport GraphPane Vertical $
            BP.progressBar Nothing (ratio 0    3500 ECU.engineSpeed s) 
        <=> BP.progressBar Nothing (ratio 0.0  4.0  ECU.throttlePot s)
        <=> BP.progressBar Nothing (ratio 0    130  ECU.mapSensor   s)
        <=> BP.progressBar Nothing (ratio 11.0 15.0 ECU.battVoltage s)
      ]
--
-- | plotTextバージョン
--
drawGraph :: Status -> Widget Name
drawGraph s = viewport GraphPane Both $ str $ G.plotWithConfig config graph
  where
    config = G.PlotConfig {G.c'width = 61, G.c'height =  15, G.c'samples = maxData, G.c'showAxes = True }
    graph  = G.emptyXYPlot G..+ esp G..- (0,fromIntegral maxData - 1) G..| (0,100)
                           G..+ tpot G..+ maps {- G..+  batv -} G..+ ctmp {- G..+ atmp G..+ itmp -}
                           G..+ igad G..+ o2vt
    e t = snd.evnt $ (dset s) !! (round t)
    esp :: G.Function
    esp t  = case e t of
                ECU.Tick r -> 100.0 / 3500.0 * ( fromIntegral . ECU.engineSpeed $ ECU.parse r )
                _          -> 0
    tpot :: G.Function
    tpot t = case e t of
                ECU.Tick r -> 100.0 / 4.0 * ( realToFrac . ECU.throttlePot $ ECU.parse r )
                _          -> 0
    maps :: G.Function
    maps t = case e t of
                ECU.Tick r -> 100.0 / 130.0 * ( fromIntegral . ECU.mapSensor $ ECU.parse r )
                _          -> 0
    batv :: G.Function
    batv t = case e t of
                ECU.Tick r -> 100.0 / 15.0 * ( realToFrac . ECU.battVoltage $ ECU.parse r )
                _          -> 0
    ctmp :: G.Function
    ctmp t = case e t of
                ECU.Tick r -> 100.0 / 110 * ( realToFrac . ECU.coolantTemp $ ECU.parse r )
                _          -> 0
    atmp :: G.Function
    atmp t = case e t of
                ECU.Tick r -> 100.0 / 110 * ( realToFrac . ECU.ambientTemp $ ECU.parse r )
                _          -> 0
    itmp :: G.Function
    itmp t = case e t of
                ECU.Tick r -> 100.0 / 110 * ( realToFrac . ECU.intakeATemp $ ECU.parse r )
                _          -> 0
    igad :: G.Function
    igad t = case e t of
                ECU.Tick r -> 100.0 / 135.0 * ( realToFrac . ECU.ignitionAd $ ECU.parse r )
                _          -> 0
    o2vt t = case e t of
                ECU.Tick r -> 100.0 / 1000.0 * ( fromIntegral . ECU.lambda_voltage $ ECU.parse r )
                _          -> 0
--  
-- | 単純文字列グラフバージョン
drawGraph' :: Status -> Widget Name
drawGraph' s = viewport GraphPane Both $
          str ( graph 0    3500 ECU.engineSpeed  dsets )
      <=> str ( graph 0.0  4.0  ECU.throttlePot  dsets )
      <=> str ( graph 0    130  ECU.mapSensor    dsets )
      <=> str ( graph 11.0 15.0 ECU.battVoltage  dsets )
      <=> str ( graph (-20) 110 ECU.coolantTemp  dsets )
      <=> str ( graph (-20) 110 ECU.ambientTemp  dsets )
      <=> str ( graph (-20) 110 ECU.intakeATemp  dsets )
      <=> str " "
      <=> str " "
      <=> str ( graph 0     180 ECU.idleACMP     dsets)
      <=> str ( graph 0     360 ECU.idleSpdDev   dsets)
      <=> str ( graph (-24.0) 135.0 ECU.ignitionAd dsets)
      <=> str ( graph 0.0 1300.0 ECU.coilTime    dsets)
      <=> str ( graph 0    1000 ECU.lambda_voltage dsets)
      where
        dsets :: [DataSet]
        dsets = rdat s : dset s 
        graph :: (Real a) => a -> a -> (ECU.Frame -> a) -> [DataSet] -> String
        graph min max f = map ( hBarCh min max f) 
        hBarCh :: (Real a) => a -> a -> (ECU.Frame -> a )-> DataSet -> Char
        hBarCh min max f ds = case snd $ evnt ds of
          ECU.Done           -> 'C'
          ECU.GotIACPos _    -> 'C'
          ECU.Error _        -> 'E'
          ECU.PortNotFound _ -> 'x'
          ECU.Connected _    -> '+'
          ECU.OffLined       -> '-'
          ECU.Tick r         -> let d = f $ ECU.parse r in case  (d < min, max < d) of
            (True, _   ) -> 'L'
            (_,True    ) -> 'U'
            _            -> gs !! truncate (  toRational ( length gs -1 )  *   ( toRational d - toRational min ) / toRational (max-min))
        gs = " ▁▂▃▄▅▆▇█"
-- | draw ECU Fault status in error code pane
drawEcuFaultStatus :: Status -> Widget Name
drawEcuFaultStatus s = case event s of
  ECU.PortNotFound p -> emptyWidget
  _                  -> -- ECU.OffLine or ECU.OnLine
          str (        " (01) Coolant temp Sensor      | " ++ e01 ++ " (02) Air temp sensor          | " ++ e02 )
      <=> str (        " (10) Fuel pump circuit        | " ++ e10 ++ " (16) Throttle position sensor | " ++ e16 )
      where
        d' = case event s of
                ECU.Tick r  -> ECU.parse r
                _           -> ECU.parse ECU.emptyData807d
        e01 = ene $ ECU.faultCode1  d' -- (01) Coolant temp Sensor 
        e02 = ene $ ECU.faultCode2  d' -- (02) Air temp sensor 
        e10 = ene $ ECU.faultCode10 d' -- (10) Fuel pump cirkit 
        e16 = ene $ ECU.faultCode16 d' -- (16) Throttle position sensor 
        ene :: Bool -> String
        ene b = if b 
            then {- bred ++ yellow ++ -}   " ER " --  ++ reset 
            else {- bgreen ++ yellow ++ -} " OK " -- ++ reset
--
drawIACPos :: Status -> Widget Name
drawIACPos s = case event s of
    ECU.GotIACPos p -> str $ "IAC Pos : " ++ show p
    _               -> emptyWidget

--
drawData :: Status -> Widget Name
drawData s = viewport DataPane Vertical $ case event s of
  ECU.PortNotFound p -> UI.drawInitialScreen ver date
  _                  -> -- ECU.OffLine or ECU.OnLine
               str ( printf "   Engine Speed (rpm) :   %5d o "      ( ECU.engineSpeed d' ) ) <+> hLimit 10 ( BP.progressBar Nothing (ratio 0 3500 ECU.engineSpeed)    )
        <=>  ( str ( printf "throttle Potent ( V ) :   %5.2f x "  ( ECU.throttlePot d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (ratio 0.0 4.0 ECU.throttlePot)   ) )
        <=>  ( str ( printf "     map Sensor (kPa) :     %3d   "  ( ECU.mapSensor   d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (ratio 0 130 ECU.mapSensor)       ) )
        <=>  ( str ( printf "battery Voltage ( V ) :   %5.2f   "  ( ECU.battVoltage d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (ratio 11.0 15.0 ECU.battVoltage) ) )
        <=>  str ( printf "   Coolant Temp (dgC) :     %3d + "  ( ECU.coolantTemp d' ) ) 
        <=>  str ( printf "   ambient Temp (dgC) :     %3d   "  ( ECU.ambientTemp d' ) )
        <=>  str ( printf "intake Air Temp (dgC) :     %3d   "  ( ECU.intakeATemp d' ) )
        <=>  str ( printf " park or neutral? A/C?: %5s "      ( parkorneutral (ECU.pnClosed d' ))   ++ ' ':aconoff (ECU.pnClosed d')  )
        <=>  str ( printf "    idle switch       : %5s "      ( closedorclear (ECU.idleSwitch d' ) ) )
        <=>  str ( printf "idl Air Ctl M P(C/O)  :     %3d "  ( ECU.idleACMP d'   ) )
        <=>  str ( printf "idl Spd deviatn       :   %5d "    ( ECU.idleSpdDev d' ) )
        <=>  str ( printf "ignition advnce (deg) :  %6.2f # "   ( ECU.ignitionAd d' ) )
        <=>  str ( printf "      coil Time (msc) : %7.2f "  ( ECU.coilTime d'   ) )
        <=>  str ( printf " lambda voltage ( mV) :    %4d   "  ( ECU.lambda_voltage d' ) ++ richorlean ( ECU.lambda_voltage d' ) )
        <=>  str ( printf "      closed loop     :     %3d   "  ( ECU.closed_loop'   d' ) ++ openorclosed ( ECU.closed_loop' d' ) )
        <=>  str ( printf "      fuel trim ( %% ) :     %3d  "  ( ECU.fuel_trim' d'     ) )
        <=>  B.hBorderWithLabel (str "Unknown 80 data")
        <=>  str          " 0B 0F 10 11 15 19 1A 1B"
        <=>  str ( printf " %2x %2x %2x %2x %2x %2x %2x %2x" (ECU.unknown0B d') (ECU.unknown0F d') (ECU.unknown10 d') (ECU.unknown11 d') (ECU.unknown15 d') (ECU.unknown19 d') (ECU.unknown1A d') (ECU.unknown1B d') )
-- Prelude.putStrLn $ vt100mv 30 0  ++ "----------------- Log -------------------------"
-- mapM_ (Prelude.putStrLn . take 40 ) (if length logs >= 4 then take 4 logs else logs)
-- Prelude.putStrLn $ vt100mv 36 0  ++ "-----------------------------------------------" ++ vt100mv 3 0
       where  
          d' = ECU.parse $ case event s of
                  ECU.Tick r -> r
                  _          -> ECU.emptyData807d
          tf c = if c then {- green ++ -} "True " {- ++ reset -} else {- red ++ -} "False" -- ++ reset
          closedorclear::Bool -> String
          closedorclear b = if b then " Closed" else " Other "
          parkorneutral::Int -> String -- 0 is closed
          parkorneutral b = if b == 0 then " Closed" else " Open  "
          aconoff::Int -> String
          aconoff       d = if d == 0 then {- bblue ++ yellow ++ -}   " a/c on   " {- ++ reset -}
                                      else {- bgreen ++ yellow ++ -}  " a/c off  " {- ++ reset -}
          richorlean::Int -> String
          richorlean v   = if v >= 450 then {- bred ++ green  ++ -}   " rich     " {- ++ reset -} 
                                       else {- bgreen ++ yellow ++ -} " lean     " {- ++ reset -}
          openorclosed::Int -> String
          openorclosed d = if d == 0   then {- bred ++ green  ++ -}   "Crl wt FDt" {- ++ reset -}
                                       else {- bgreen ++ yellow ++ -} "Crl wt O2d" {- ++ reset -}
          ratio:: (Real a) => a -> a -> (ECU.Frame -> a) -> Float 
          ratio min max f = case (d' <= min,d' >= max) of
              (True , _ )  -> 0.0
              (_ , True )  -> 1.0
              _            -> fromRational ((toRational d' - toRational min ) / (toRational max - toRational min))
             where d' = case event s of
                          ECU.Tick r -> f $ ECU.parse r
                          _          -> min
--
drawMenu :: Status -> Widget Name
drawMenu s = 
  if not (inmenu s)
    then emptyWidget
    else str $ menu s
           --(foldl <=> (str "") (map str (map mstring (menu s))))
--
event :: Status -> ECU.EvContents
event = snd . evnt . rdat
--


