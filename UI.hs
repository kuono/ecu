{- |
Module      : UI
Description : Text User Interface Library for Rover Mini ECU Monitor
Copyright   : (c) Kentaro UONO, 2019-2021
License     : MIT Licence
Maintainer  : info@kuono.net
Stability   : experimental
Portability : macOS X
-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI where

import Lib
import qualified ECU
import Data.Time.LocalTime
import Text.Printf
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.ProgressBar as BP
import Brick.Forms
-- import qualified Data.Text as T
-- import Control.Lens.TH
import qualified Graphics.Vty as V
import Graphics.Vty
import Brick.BorderMap
import TextPlot ( PlotConfig(..) , PlotFunction
                --, ParamFunction(..), PlotColour(..) 
                , (.+),(.|),(.-)
                , plotStrWithConfig
                , emptyXYPlot
                -- , Line, Cell ,plotCellWithConfig,
                )
import Lens.Micro ((^.))
--
maxGraphLength :: Int  -- ^ Graph Plot Area width limitation
maxGraphLength = 20
-- dset:: V.Vector ECU.EvContents
-- dset = V.singleton ECU.OffLined
-- 
-- UI Name space
-- 
data Display = Dialog | DataPanel | CurrentStatus | CurrentData | GraphLog | BarLog | TextLog
--
-- Graph related definitions
-- 
-- | Drawing instruction
type Point = (Int,Int) -- ^ (x,y) x = 0..100, y = 0..100 
data Instruction
  = Point              -- ^ Point
  | Line Point Point   -- ^ Line from Point 1 to Point 2
  | End
--
type Canvas = [[(Int, Colour)]]
data Colour = Red | Yellow | Blue
--
-- | drawing function
plotGraph :: Canvas -> [Instruction] -> Canvas
plotGraph = undefined
--
-- UI Attribute Map
--
normalAttr , errorAttr , alertAttr , pgcompAttr , pgtodoAttr , espeedAttr
 , thpotAttr , msensAttr , batvAttr , mnotselectedAttr , mselectedAttr :: AttrName
normalAttr = attrName "normalAttr"
errorAttr  = attrName "errorAttr"                   :: AttrName
alertAttr  = attrName "alertAttr"                   :: AttrName
pgcompAttr = attrName "progressComplete"            :: AttrName
pgtodoAttr = attrName "progressIncomplete"          :: AttrName
espeedAttr = attrName "espeedAttr"                  :: AttrName
thpotAttr  = attrName "thpotAttr"                   :: AttrName
msensAttr  = attrName "msensAttr"                   :: AttrName
batvAttr   = attrName "batvAttr"                    :: AttrName
mnotselectedAttr = attrName "menuisnotselectedAttr" :: AttrName
mselectedAttr    = attrName "menuisselectedAttr"    :: AttrName
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
    , (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    ]
--
-- Form for default Data and function
--
-- | types for user information on Form Dialog 
-- data UserInfo = 
--   FormState { _portAddress    :: T.Text     -- ^ USB - シリアルコネクタを接続したポート名
--             , _logFolderPath  :: T.Text   -- ^ ログファイルを格納する場所の名前
--             , _logNameRule    :: T.Text     -- ^ ログファイルの名前の生成規則
--             } deriving (Show)

-- makeLenses ''UserInfo

-- | make User Information input form
-- mkUIForm :: UserInfo -> Form UserInfo e Name
-- mkUIForm =  
--   let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
--   in  newForm
--         [ label "Port Path     :" @@= editTextField portAddress PortAddressField (Just 1)
--         , label "Forder Path   :" @@= editTextField logFolderPath LogFolderPathField (Just 1) 
--         , label "Log File Name :" @@= editTextField logNameRule LogNameRuleField (Just 1)
--         ]
--
-- Draw functions
--
-- numbers :: ([Char], [[[Char]]])
-- numbers = (['0','1','1','2','3','4','5','6','7','8','9',',','.'],
--    [["****","   *","****","****","*  *","****","****","****","****","****","    ","    "],
--     ["*  *","   *","   *","   *","*  *","*   ","*   ","   *","*  *","*  *","    ","    "],
--     ["*  *","   *"," ** ","****","****","****","****","   *","****","****","    ","    "],
--     ["*  *","   *","**  ","   *","   *","   *","*  *","   *","*  *","   *","   *","    "],
--     ["****","   *","****","****","   *","****","****","   *","****","   *","  * ","  **"]]) 
-- alphabets :: ([Char], [[[Char]]])
-- alphabets = (['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'],
--   [["****","*   ","****","   *","****","****","****","*  *","*** ","  * ","*  *","*   ","* **","    ","    ","****","    ","****"," ***","****","*  *","    ","    ","   ","*  *","****"],
--    ["   *","*   ","*   ","   *","*   ","*   ","*  *","*  *"," *  ","  * ","* * ","*   ","****","    ","    ","*  *","****","*  *","*   ","  * ","*  *","    ","    ","   ","*  *","  * "],
--    ["****","****","*   ","****","****","****","****","****"," *  ","  * ","**  ","*   ","* **","****","****","****","*  *","****","****","  * ","*  *","*  *","* **","* *","****"," *  "],
--    ["*  *","*  *","*   ","*  *","*   ","*   ","   *","*  *"," *  ","  * ","* * ","*   ","* **","*  *","*  *","*   ","****","* * ","   *","  * ","*  *"," * *","* **"," * ","  * ","*   "],
--    ["****","****","****","****","****","*   ","****","*  *","*** ","*** ","*  *","****","* **","*  *","****","*   ","   *","*  *","*** ","  * "," ** ","  * ","****","* *","  * ","****"]])
--
drawInitialScreen :: String -- ^ current version
                  -> String -- ^ compiled date
                  -> Widget Name
drawInitialScreen v d =
      str   "    \\              '             `"
  <=> str   "     .              .             `     ,_ .`"
  <=> str   "--_,   ,            |              /  /`. +'.'"
  <=> str   "+ + \". .===========================w. || = =  "
  <=> str   "= = :|V-Monitor for Rover Mini MEMS-\\\\ \\.+_+.'"
  <=> str   "- -,\"|:--------- ------------ -------:|     |"
  <=> str ( ( take 37 ( " .   \\\\-Version " ++ v ++ " on " ++ d )) ++ "-//   .  ." )
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
        <=> ( B.hBorderWithLabel (str "MENU")
              -- <=> if inmenu ecu then
              --       let defaultUserInformation = FormState "" "" ""
              --       in C.vCenter $ C.hCenter $ renderForm $ mkUIForm defaultUserInformation
              --     else
              --       str ""
              <=> B.hBorder
        )
        <=> draw807dData ecu--  <+> B.vBorder
        <=> drawIACPos ecu
        <=> B.hBorderWithLabel ( str "Fault Status")
        <=> drawEcuFaultStatus ecu
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
    ECU.PortNotFound f -> withAttr errorAttr  $ str (" Port Not Found :" ++ f ) <+> B.vBorder
    ECU.Connected _    -> withAttr normalAttr $ str (" Connected. " ++ (ECU.mname $ model s) ++ "(" ++ show (ECU.d8size $ model s) ++ "," ++ show (ECU.d7size $ model s) ++ ")") <+> B.vBorder
    ECU.OffLined       -> withAttr alertAttr  $ str " Off Line                      " <+> B.vBorder
    ECU.Tick _         ->
      withAttr  (if True then normalAttr else alertAttr)
       $ str (" Connected. " ++ (ECU.mname $ model s) ++ "(" ++ show (ECU.d8size $ model s) ++ "," ++ show (ECU.d7size $ model s) ++ ")") <+> B.vBorder
    ECU.GotIACPos _    -> withAttr normalAttr $ str (" Connected. " ++ (ECU.mname $ model s) ++ "(" ++ show (ECU.d8size $ model s) ++ "," ++ show (ECU.d7size $ model s) ++ ")") <+> B.vBorder
    ECU.Error m        -> withAttr errorAttr  $ str (" Error             : " ++ m ) <+> B.vBorder
    _                  -> emptyWidget
--
drawEcuErrorContents :: Status -> Widget Name
drawEcuErrorContents s = viewport ErrorContentsPane Horizontal $
    case event s of
      ECU.PortNotFound f -> withAttr errorAttr  $ str f
      ECU.Connected m    -> withAttr normalAttr $ str $ ECU.mname m
      ECU.OffLined       -> withAttr errorAttr  $ str "     "
      ECU.Tick _         -> emptyWidget
      ECU.Error s'       -> withAttr errorAttr  $ str s'
      _                  -> emptyWidget
--
drawTime :: Status -> Widget Name
drawTime s = withAttr ( if sodd s then mselectedAttr else mnotselectedAttr ) $ tstr s
  where
    sec (LocalTime _ t') = truncate $ todSec t'
    time = fst . evnt . rdat
    sodd = odd . fromIntegral . sec . time
    tstr = str . take 22 . show . time
--
draw807dData :: Status -> Widget Name
draw807dData s = vLimit 21 $      B.hBorderWithLabel ( str "Data 80/7D" )
                              <=> ( hLimit 41 (drawData s) <+> B.vBorder <+> drawGraph' s )
--  vLimit 21 $ case event s of
--   ECU.PortNotFound p -> drawInitialScreen ver date-- hLimit 60 $ vBox [drawData s]
--   _                  -> -- ECU.OffLine or ECU.OnLine 
--         B.hBorderWithLabel ( str "Data 80/7D" ) 
--     <=> ( hLimit 41 (drawData s) <+> B.vBorder <+> drawGraph' s )
--
drawBar :: Status -> Widget Name
drawBar s = {- vLimit 19 $ hLimit 20 $ -} vBox [
  case event s of
    ECU.PortNotFound _ -> emptyWidget
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
drawGraph s = viewport GraphPane Both $ str $ plotStrWithConfig config graph
  where
    -- colour :: PlotColour -> String
    -- colour White = Lib.white
    -- colour Red   = Lib.red
    -- colour _     = Lib.blue
    config = PlotConfig
        { c'width    = maxGraphLength -- 40
        , c'height   = 15
        , c'samples  = maxData  -- 60 defined in Lib
        , c'showAxes = True
        }
    graph  = emptyXYPlot
                .+ esp .- (0,fromIntegral maxData - 1) .| (0,100)
                {- .+ tpot -} {- .+ maps -} {- .+  batv -}
                {- .+ ctmp -} {- .+ atmp .+ itmp -}
                .+ isdev
                .+ igad
                .+ o2vt
    e t = snd.evnt $ (dset s) !! (round t)
    esp :: PlotFunction
    esp t  = case e t of
                ECU.Tick r -> 100.0 / 4000.0 * ( fromIntegral . ECU.engineSpeed $ ECU.parse r )
                _          -> 0
    isdev :: PlotFunction
    isdev t = case e t of
                ECU.Tick r -> 100.0 / 1000.0 * ( realToFrac . ECU.idleSpdDev $ ECU.parse r )
                _          -> 0
    -- tpot :: PlotFunction
    -- tpot t = case e t of
    --             ECU.Tick r -> 100.0 / 4.0 * ( realToFrac . ECU.throttlePot $ ECU.parse r )
    --             _          -> 0
    -- maps :: PlotFunction
    -- maps t = case e t of
    --             ECU.Tick r -> 100.0 / 130.0 * ( fromIntegral . ECU.mapSensor $ ECU.parse r )
    --             _          -> 0
    -- batv :: PlotFunction
    -- batv t = case e t of
    --             ECU.Tick r -> 100.0 / 15.0 * ( realToFrac . ECU.battVoltage $ ECU.parse r )
    --             _          -> 0
    -- ctmp :: PlotFunction
    -- ctmp t = case e t of
    --             ECU.Tick r -> 100.0 / 110 * ( realToFrac . ECU.coolantTemp $ ECU.parse r )
    --             _          -> 0
    -- atmp :: PlotFunction
    -- atmp t = case e t of
    --             ECU.Tick r -> 100.0 / 110 * ( realToFrac . ECU.ambientTemp $ ECU.parse r )
    --             _          -> 0
    -- itmp :: PlotFunction
    -- itmp t = case e t of
    --             ECU.Tick r -> 100.0 / 110 * ( realToFrac . ECU.intakeATemp $ ECU.parse r )
    --             _          -> 0
    igad :: PlotFunction
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
        graph mind maxd f = map ( hBarCh mind maxd f)
        hBarCh :: (Real a) => a -> a -> (ECU.Frame -> a )-> DataSet -> Char
        hBarCh mind maxd f ds = case snd $ evnt ds of
          ECU.Done _         -> 'G'
          ECU.GotIACPos _    -> 'C'
          ECU.Error _        -> 'E'
          ECU.PortNotFound _ -> 'x'
          ECU.Connected _    -> '+'
          ECU.OffLined       -> '-'
          ECU.Tick r         -> let d = f $ ECU.parse r in case  (d < mind, maxd < d) of
            (True, _   ) -> 'L'
            (_,True    ) -> 'U'
            _            -> gs !! truncate (  toRational ( length gs -1 )  *   ( toRational d - toRational mind ) / toRational (maxd-mind))
        gs = " ▁▂▃▄▅▆▇█"
-- | draw ECU Fault status in error code pane
drawEcuFaultStatus :: Status -> Widget Name
drawEcuFaultStatus s = case event s of
  ECU.PortNotFound _ -> emptyWidget
  _                  -> -- ECU.OffLine or ECU.OnLine
          ( str ( "0D:" ++ e0d ) <+> ge " (01) COOLANT " e01 <+> ge " (02) A. TEMP " e02 <+> ge " (??) AMBIENT " ex4 <+> ge " (??) F. TEMP " ex5 )
      <=> ( str ( "0E:" ++ e0e ) <+> ge " (10) F. PUMP " e10 <+> ge " (??) MAP.S   " ey5 <+> ge " (16) T. POT  " e16 )
      where
        ge m f = if f then withAttr errorAttr (str m) else withAttr normalAttr (str m)
        d' = case event s of
                ECU.Tick r  -> ECU.parse r
                _           -> ECU.parse ECU.emptyData807d
        e0d = printf "%2X" $ ECU.faultCode0D d'
        e0e = printf "%2X" $ ECU.faultCode0E d'
        e01 = ECU.faultCode1  d' -- (01) Coolant temp Sensor 
        e02 = ECU.faultCode2  d' -- (02) Air temp sensor 
        e10 = ECU.faultCode10 d' -- (10) Fuel pump cirkit 
        e16 = ECU.faultCode16 d' -- (16) Throttle position sensor 
        ex4 = ECU.faultCodeX4 d' -- (??) Maybe Ambient air temp Sensor Error
        ex5 = ECU.faultCodeX5 d' -- (??) Maybe Fuel Temp Sensor Error *
        ey5 = ECU.faultCodeY5 d' -- (??) Maybe intake manifold pressure sesnor (MAP Sensor)
--
drawIACPos :: Status -> Widget Name
drawIACPos s =  str $   "IAC Pos : " ++  show (lIacPos s)
                  ++  ", ICool Temp : " ++  show (iCoolT s)

--
drawData :: Status -> Widget Name
drawData s = viewport DataPane Vertical $ case event s of
  ECU.PortNotFound _ -> UI.drawInitialScreen ver date
  _                  -> -- ECU.OffLine or ECU.OnLine
               str ( printf "   Engine Speed (rpm) :   %5d o "      ( ECU.engineSpeed d' ) ) <+> hLimit 10 ( BP.progressBar Nothing (dratio 0 3500 ECU.engineSpeed)    )
        <=>  ( str ( printf "throttle Potent ( V ) :   %5.2f x "  ( ECU.throttlePot d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (dratio 0.0 4.0 ECU.throttlePot)   ) )
        <=>  ( str ( printf "     map Sensor (kPa) :     %3d   "  ( ECU.mapSensor   d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (dratio 0 130 ECU.mapSensor)       ) )
        <=>  ( str ( printf "battery Voltage ( V ) :   %5.2f   "  ( ECU.battVoltage d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (dratio 11.0 15.0 ECU.battVoltage) ) )
        <=>  ( ( case iCoolT s of
                  Just t0 -> if coolantTemp > t0 + 1 then
                              withAttr normalAttr
                            else
                              withAttr alertAttr
                  Nothing -> withAttr errorAttr
               ) $ str ( printf "   Coolant Temp (dgC) :     %3d   "  coolantTemp ) )
        <=>  str ( printf "   ambient Temp (dgC) :     %3d   "  ( ECU.ambientTemp d' ) )
        <=>  str ( printf "intake Air Temp (dgC) :     %3d   "  ( ECU.intakeATemp d' ) )
        <=>  if ECU.name ( model s )  == "MNE10078  M/SPI Japan Cooper"
               then str ( printf " park or neutral      : %4s  %02X"  ( parkorneutral ( ECU.pnClosed d' ) ) ( ECU.pnClosed d') ++ aconoff (ECU.pnClosed d') )
               else str ( printf " Cooler (MEMS 1.3J)   : %10s" (aconoff (ECU.pnClosed d')) )
        <=>  str ( printf "    idle switch       :      %02x "  ( ECU.idleByte d'    ) ) -- ( closedorclear (ECU.idleSwitch d' ) ) )
        <=>  str ( printf "idl Air Ctl M P(C/O)  :     %3d "    ( ECU.idleACMP d'   ) )
        <=>  str ( printf "idl Spd deviatn       :   %5d + "    ( ECU.idleSpdDev d' ) )
        <=>  str ( printf "ignition advnce (deg) :  %6.2f # "   ( ECU.ignitionAd d' ) )
        <=>  str ( printf "      coil Time (msc) : %7.2f "      ( ECU.coilTime d'   ) )
        <=>  str ( printf " lambda voltage ( mV) :    %4d   "   ( ECU.lambda_voltage d' ) ++ richorlean ( ECU.lambda_voltage d' ) )
        <=>  str ( printf "      closed loop     :     %3d   "  ( ECU.closed_loop'   d' ) ++ openorclosed ( ECU.closed_loop' d' ) )
        <=>  str ( printf "      fuel trim ( %% ) :     %3d  "  ( ECU.fuel_trim' d'     ) )
        <=>  B.hBorderWithLabel (str "Unknown 80 data")
        <=>  str          " 0B 0F 10 11 15 19 1A 1B"
        <=>  str ( printf " %2x %2x %2x %2x %2x %2x %2x %2x" (ECU.unknown0B d') (ECU.unknown0F d') (ECU.unknown10 d') (ECU.unknown11 d') (ECU.unknown15 d') (ECU.unknown19 d') (ECU.unknown1A d') (ECU.unknown1B d') )
      where
        coolantTemp = ECU.coolantTemp d'
-- Prelude.putStrLn $ vt100mv 30 0  ++ "----------------- Log -------------------------"
-- mapM_ (Prelude.putStrLn . take 40 ) (if length logs >= 4 then take 4 logs else logs)
-- Prelude.putStrLn $ vt100mv 36 0  ++ "-----------------------------------------------" ++ vt100mv 3 0
       where
          d' = ECU.parse $ case event s of
                  ECU.Tick r -> r
                  _          -> ECU.emptyData807d
          -- tf c = if c then {- green ++ -} "True " {- ++ reset -} else {- red ++ -} "False" -- ++ reset
          -- closedorclear::Bool -> String
          -- closedorclear b = if b then " Closed" else " Other "
          parkorneutral::Int -> String -- 0 is closed
          parkorneutral b = if b == 0 then "Clsd" else "Open"
          aconoff::Int -> String
          aconoff       d = if d == 0 then {- bblue ++ yellow ++ -}   " a/c on   " {- ++ reset -}
                                      else {- bgreen ++ yellow ++ -}  " a/c off  " {- ++ reset -}
          richorlean::Int -> String
          richorlean v   = if v >= 450 then {- bred ++ green  ++ -}   " rich     " {- ++ reset -}
                                       else {- bgreen ++ yellow ++ -} " lean     " {- ++ reset -}
          openorclosed::Int -> String
          openorclosed d = if d == 0   then {- bred ++ green  ++ -}   "Crl wt FDt" {- ++ reset -}
                                       else {- bgreen ++ yellow ++ -} "Crl wt O2d" {- ++ reset -}
          dratio:: (Real a) => a -> a -> (ECU.Frame -> a) -> Float
          dratio llimit hlimit f = case (dt <= llimit,dt >= hlimit) of
              (True , _ )  -> 0.0
              (_ , True )  -> 1.0
              _            -> fromRational ((toRational dt - toRational llimit ) / (toRational hlimit - toRational llimit))
             where dt = case event s of
                          ECU.Tick r -> f $ ECU.parse r
                          _          -> llimit
--
drawMenu :: Status -> Widget Name
drawMenu s =
  if not (inmenu s)
    then emptyWidget
    else str $ menu s
           --(foldl <=> (str "") (map str (map mstring (menu s))))
--
customWidget :: String -> Widget n
customWidget s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        render $ str (s <> " " <> show (ctx^.availWidthL))
--
myFill :: Char -> Widget n
myFill ch =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let a = ctx^.attrL
        return $ Result (Graphics.Vty.charFill a ch (ctx^.availWidthL) (ctx^.availHeightL))
                        [] [] [] Brick.BorderMap.empty
--
drawNumPane :: Double -> Widget Name
drawNumPane n = str $ show n
--
event :: Status -> ECU.EvContents
event = snd . evnt . rdat
--

