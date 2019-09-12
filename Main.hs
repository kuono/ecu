-- # ECU Monitor for Rover Mini
-- usage
--  ecu dev   ... communicate ECU through dev
--  push any key to end this programme
-- {-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import qualified ECU
import UI

import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import Data.List -- for test
import Data.List.Split
import Data.Word
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.ByteString as BS
import Numeric
import System.Directory
import System.DiskSpace
import System.Environment (getArgs,getEnv,getEnvironment)
-- import System.Hardware.Serialport 
import System.IO -- for stdin, Buffering Mode
import Text.Printf

import Brick
--   ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
--   , customMain, neverShowCursor
--   , continue, halt
--   , hLimit, vLimit, vBox, hBox
--   , padRight, padLeft, padTop, padAll, Padding(..)
--   , withBorderStyle
--   , str
--   , attrMap, withAttr, emptyWidget, attrName, on, fg
--   , (<+>)
--   )
import Brick.Main
import qualified Brick.BChan as BC
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.ProgressBar as BP
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))   

ver   = "0.8.0"
date  = "2019.09.07"

ecuMonitor :: App Status Event Name
ecuMonitor = App { appDraw = drawPanes             
              , appChooseCursor = neverShowCursor  
              , appHandleEvent = handleEvent      
              , appStartEvent = return            
              , appAttrMap = const theMap        
              }

normalAttr = attrName "normalAttr"
errorAttr  = attrName "errorAttr"
alertAttr  = attrName "alertAttr"
pgcompAttr = attrName "progressComplete"
pgtodoAttr = attrName "progressIncomplete"
espeedAttr = attrName "espeedAttr"
thpotAttr  = attrName "thpotAttr"
msensAttr  = attrName "msensAttr"
batvAttr   = attrName "batvAttr"

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
    ]
--
-- | Main function 主関数
main :: IO ()
main = do
    args <- System.Environment.getArgs
    env  <- System.Environment.getEnv "HOME"
    let path = case (null args,env == "/Users/kuono") of
            (True,True) -> defaultUSBPathMac
            (True,_   ) -> defaultUSBPathRaspberryPi
            _           -> error "error: exactly one arguments needed."
        buildVty = V.mkVty V.defaultConfig
    iniVty  <- buildVty
    evntCh  <- BC.newBChan 10 :: IO (BC.BChan Event) -- ^ make an event channel for Brick
    ucmdCh  <- newTBQueueIO 2 :: IO (TBQueue ECU.UCommand) 
    iStatus <- initialState (evntCh,ucmdCh)
    cid     <- forkIO $ forever $ do
        -- Organize commands in stm ch |  Outside of loop             | Inside of loop
        --    when mems is offline,    | consume any command w/o init | only init is acceptable
        --              is online,     | do nothing                   | accept all
        -- without this organization, stm error will be issued by GHC runtime system 
        r <- atomically $ tryPeekTBQueue ucmdCh
        case r of
            (Just ECU.Init   ) -> do
                atomically $ readTBQueue ucmdCh
                ECU.run ECU.loop (path,evntCh,ucmdCh) -- ^ fork communication thread
            _                  -> do
                atomically $ readTBQueue ucmdCh
                return ()
    -- lid <- forkIO $ eculog dataCh  --  fork logger thread 
    fStatus <- Brick.Main.customMain iniVty buildVty
                (Just evntCh) ecuMonitor iStatus
    Prelude.putStrLn "Thank you for using Mini ECU Monitor. See you again!"
    return ()

-- eculog :: Chan ECU.RData -> IO()
-- eculog dataCh = do
--   l <- logFileName :: IO FilePath
--   withFile l WriteMode $
--       \h -> do
--         hPutStrLn h  $ "Date,Time," ++ frameTitle
--         forever $ do
--           r <- System.DiskSpace.getAvailSpace l
--           d <- readChan dataCh
--           let j  = localTimetoString $ ECU.t d
--               s  = frametoTable . ECU.parse $ ECU.d d
--           hPutStrLn h  $ j ++ "," ++ s
--     `Ex.catches`
--       [ Ex.Handler (\e -> case (e::ECU.UserCommand) of 
--           ECU.Quit -> do
--             ima <- currentTime
--             hPutStrLn h $ "At " ++ show ima ++ " , logger terminated because of quit msg."
--             hFlush h
--             hClose h
--           ECU.Reconnect -> do
--             ima <- currentTime
--             hPutStrLn h $ "At " ++ show ima ++ " , try to reconnect ECU."
--             hFlush h
--             hClose h
--             eculog dataCh
--           _  -> Ex.throwIO e)
--       , Ex.Handler (\e -> case (e::Ex.IOException) of
--           _  -> Ex.throwIO e -- print e 
--           )
--       ]

--
-- Drawing
--
drawPanes :: Status -> [Widget Name]
drawPanes ecu =
    [ withBorderStyle BS.unicodeRounded $ B.borderWithLabel (str "Rover Mini MEMS Monitor") $
        drawMenu ecu 
        <=> vLimit 1 ( drawEcuStatus ecu <+> drawTime ecu )
        <=> draw807dData ecu--  <+> B.vBorder
        <=> B.hBorderWithLabel ( str "Fault Status")
        <=> drawEcuFaultStatus ecu
    ]

drawEcuStatus :: Status -> Widget Name
drawEcuStatus s = viewport StatusPane Horizontal $ hLimit 30 $
  case event s of
    ECU.PortNotFound _ -> withAttr errorAttr  $ str   " Port Not Found               " <+> B.vBorder
    ECU.Connected _    -> withAttr normalAttr $ str   " Connected. Model             " <+> B.vBorder
    ECU.OffLined       -> withAttr alertAttr  $ str   " Off Line                     " <+> B.vBorder
    ECU.Tick r         -> withAttr normalAttr $ str   " On Line                      " 
    ECU.Error s        -> withAttr errorAttr  $ str ( " Error             : " ++ s )

drawTime :: Status -> Widget Name
drawTime s = str . take 22 . show . fst . evnt $ rdat s

draw807dData :: Status -> Widget Name
draw807dData s = vLimit 21 $ case event s of
  ECU.PortNotFound p -> drawInitialScreen ver date-- hLimit 60 $ vBox [drawData s]
  _                  -> -- ECU.OffLine or ECU.OnLine 
        B.hBorderWithLabel ( str "Data 80/7D" ) 
    <=> ( hLimit 41 (drawData s) <+> B.vBorder <+> drawGraph s )

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

drawGraph :: Status -> Widget Name
drawGraph s = viewport GraphPane Both $
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
      <=> str ( graph 0    512 ECU.lambda_voltage dsets)
      where
        dsets :: [DataSet]
        dsets = rdat s : dset s 
        graph :: (Real a) => a -> a -> (ECU.Frame -> a) -> [DataSet] -> String
        graph min max f = map ( hBarCh min max f) 
        hBarCh :: (Real a) => a -> a -> (ECU.Frame -> a )-> DataSet -> Char
        hBarCh min max f ds = case snd $ evnt ds of
          ECU.Done           -> 'C'
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
            then {- bred ++ yellow ++ -}   " E R R O R " --  ++ reset 
            else {- bgreen ++ yellow ++ -} " No ERROR  " -- ++ reset
--
event :: Status -> ECU.EvContents
event = snd . evnt . rdat
--
drawData :: Status -> Widget Name
drawData s = viewport DataPane Vertical $ case event s of
  ECU.PortNotFound p -> UI.drawInitialScreen ver date
  _                  -> -- ECU.OffLine or ECU.OnLine
               str ( printf "   Engine Speed (rpm) :  %5d "      ( ECU.engineSpeed d' ) ) <+> hLimit 10 ( BP.progressBar Nothing (ratio 0 3500 ECU.engineSpeed)    )
        <=>  ( str ( printf "throttle Potent ( V ) :  %5.2f "  ( ECU.throttlePot d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (ratio 0.0 4.0 ECU.throttlePot)   ) )
        <=>  ( str ( printf "     map Sensor (kPa) :    %3d "  ( ECU.mapSensor   d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (ratio 0 130 ECU.mapSensor)       ) )
        <=>  ( str ( printf "battery Voltage ( V ) :  %5.2f "  ( ECU.battVoltage d' ) )   <+> hLimit 10 ( BP.progressBar Nothing (ratio 11.0 15.0 ECU.battVoltage) ) )
        <=>  str ( printf "   Coolant Temp (dgC) :    %3d"  ( ECU.coolantTemp d' ) ) 
        <=>  str ( printf "   ambient Temp (dgC) :    %3d"  ( ECU.ambientTemp d' ) )
        <=>  str ( printf "intake Air Temp (dgC) :    %3d"  ( ECU.intakeATemp d' ) )
        <=>  str ( printf " park or neutral? A/C?:%5s"      ( parkorneutral (ECU.pnClosed d' ))   ++ ' ':aconoff (ECU.pnClosed d')  )
        <=>  str ( printf "    idle switch       :%5s"      ( closedorclear (ECU.idleSwitch d' ) ) )
        <=>  str ( printf "idl Air Ctl M P(C/O)  :    %3d"  ( ECU.idleACMP d'   ) )
        <=>  str ( printf "idl Spd deviatn       :  %5d"    ( ECU.idleSpdDev d' ) )
        <=>  str ( printf "ignition advnce (deg) : %6.2f"   ( ECU.ignitionAd d' ) )
        <=>  str ( printf "      coil Time (msc) :  %5.2f"  ( ECU.coilTime d'   ) )
        <=>  str ( printf " lambda voltage ( mV) :    %3d  "  ( ECU.lambda_voltage d' ) ++ richorlean ( ECU.lambda_voltage d' ) )
        <=>  str ( printf "      closed loop     :    %3d  "  ( ECU.closed_loop'   d' ) ++ openorclosed ( ECU.closed_loop' d' ) )
        <=>  str ( printf "      fuel trim ( %% ) :    %3d "  ( ECU.fuel_trim' d'     ) )
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

drawMenu :: Status -> Widget Name
drawMenu s = hLimit 40 $ 
  str " | Menu | " <+> 
  case event s of
    ECU.PortNotFound p -> str "ECU:Quit, 1:Set Port   " 
    ECU.OffLined       -> str "ESC:Quit, 1:Reconnect  " 
    ECU.Connected env  -> str "ESC:Quit, 1:Disconnect "
    ECU.Tick _         -> str "ESC:Quit, 1:Disconnect "
    ECU.Done           -> str "ESC:Quit"
    ECU.Error s        -> str "ESC:Quit"

-- drawGameOver :: Bool -> Widget Name
-- drawGameOver dead =
--   if dead
--      then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
--      else emptyWidget

--
-- |　-- handling events モデル更新関数
handleEvent :: Status -> BrickEvent Name Event -> EventM Name (Next Status)
-- handleEvent g (AppEvent Tick)                       = continue $ step g
-- handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
-- handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
-- handleEvent g _                                     = continue g
--
handleEvent s (VtyEvent (V.EvKey (V.KChar '1') [])) = do
    t <- liftIO Lib.currentTime
    let r = ECU.emptyData807d
        f = ECU.parse r
        ch = snd $ cset s
    liftIO $ atomically $ writeTBQueue ch ECU.Init
    -- continue s ECUへの接続がない間の臨時関数
    continue Status
      { rdat = DataSet 
          { evnt = (t, ECU.Tick r )
          , gdat = gdat $ rdat s
          , note = "Initialized"
          }
      , dset = take maxData $ rdat s : dset s
      , cset = cset s
      , menu = basicMenu
      }
--
handleEvent s (VtyEvent (V.EvKey (V.KChar '2') [])) = do
    t <- liftIO Lib.currentTime
    let ch = snd $ cset s
    liftIO $ atomically $ writeTBQueue ch ECU.Disconnect
    continue $ Status 
        { rdat = DataSet 
            { evnt = (t, ECU.OffLined)
            , gdat = gdat $ rdat s
            , note = "OffLined"
            }
        , dset = dset s
        , cset = cset s
        , menu = basicMenu
        }
-- | 臨時のハンドラー；ECUにつながることがわかったら別のロジックに
-- | むりやりOnlineステータスにする。手続き的なECUへの司令を
-- できれば定義的なものに変えたい。状態遷移ブロックへの指示？
handleEvent s (VtyEvent (V.EvKey (V.KChar '3') [])) = do
    t <- liftIO Lib.currentTime
    r <- liftIO ECU.dummyData807d
    let f = ECU.parse r
        ch = snd $ cset s
    liftIO $ atomically $ writeTBQueue ch ECU.Init
    continue $ Status 
      { rdat = DataSet
          { evnt = ( t,ECU.Tick r )
          , gdat = 
              [ (engspeed,    ECU.engineSpeed  f )
              , (tposition,   ECU.ithrottlePot f )
              , (mapsensor,   ECU.mapSensor    f )
              , (battvoltage, ECU.ibattVoltage f )
              ,  (coolanttemp,ECU.coolantTemp  f )
              ]
          , note = ""
          }
      , dset = take maxData $ rdat s : dset s
      , cset = cset s
      , menu = basicMenu
      }
handleEvent s (VtyEvent (V.EvKey (V.KChar c ) [])) = do
    t <- liftIO Lib.currentTime
    r <- liftIO ECU.dummyData807d
    let f = ECU.parse r
    continue $ Status
      { rdat = DataSet
          { evnt = ( t,ECU.Tick r )
          , gdat = 
              [ (engspeed,    ECU.engineSpeed  f )
              , (tposition,   ECU.ithrottlePot f )
              , (mapsensor,   ECU.mapSensor    f )
              , (battvoltage, ECU.ibattVoltage f )
              ,  (coolanttemp,ECU.coolantTemp  f )
              ]
          , note = ""
          }
      , dset = take maxData $ rdat s : dset s
      , cset = cset s
      , menu = basicMenu
      }
handleEvent s (VtyEvent (V.EvKey V.KEsc []))       = halt s
handleEvent s _ = continue s
