{- |
* Module      : UI
* Description : Text User Interface Library for Rover Mini ECU Monitor
* Copyright   : (c) Kentaro UONO, 2019-2022
* License     : MIT Licence
* Maintainer  : info@kuono.net
* Stability   : experimental
* Portability : macOS Big Sur and RaspberyPi OS buster
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module UI where
--
--
import Lib
import qualified ECU
--
import qualified Data.Map as M
import Data.Time.LocalTime
import Brick
import qualified Brick.BChan as BC
import Brick.BorderMap
import Brick.Forms as F
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.ProgressBar as BP

import Control.Concurrent.STM.TChan as STM (  TChan , newTChan, writeTChan )
import Control.Lens ( (^.) )
import Control.Monad.STM ( atomically )
import Control.Monad.Trans ( liftIO )
import Data.Bits ( Bits(testBit) )
import Graphics.Vty as V
import TextPlot
import Text.Printf ( printf ) 
import qualified System.Environment
import System.Directory (doesFileExist)
--
-- * Brick related 
--
data Name  = StatusPane | DataPane | GraphPane | NotePane | ErrorContentsPane
           | PortAddressField | LogFolderPathField | LogNameRuleField
   deriving (Eq,Ord,Show)
--
-- * functions
--
-- | App type 
ecuMonitor :: Brick.App Status ECU.Event Name
ecuMonitor = Brick.App
  { appDraw         = drawPanes             
  , appChooseCursor = neverShowCursor  
  , appHandleEvent  = handleEvent      
  , appStartEvent   = return ()           
  , appAttrMap      = const theMap        
  }
--
-- * Mode
--
data Mode = Loop | Line | Help deriving (Eq,Ord)
--
-- * menus
--
type MenuItem = String
type Menu = M.Map UI.Mode MenuItem
menus :: UI.Menu
menus = M.fromList
  [ ( UI.Loop , "Q :Quit  | 1 :Initialize   | 2 :OffLine | d : Get dummy Data" )
  , ( UI.Line , "Q :Quit  | 0 :Clear Faults | 1 :Initialize | ← :Dec IAC Pos | → :Inc IAC POS | p :Get IAC Pos | ↑ :Inc IgAd | ↓ :Dec IgAd " )
  , ( UI.Help , "" )
  ]
--
-- * Status of App
--
-- | App Status definition
data DataSet = DataSet
  { evnt :: !ECU.Event
  -- , gdat :: !ChartData
  , note :: !String
  }
data Status = Status
    { mode     :: UI.Mode            -- ^ testmode : memsを接続しないで試す
    , env      :: OSEnv              -- ^ env    : 
    , model    :: !ECU.MNEModel      -- ^ model  : 接続した ECU のモデル識別データ
                                     --            ECU.ModelDataSet { name :: !String, d8size :: !Int, d7size :: !Int} deriving Eq
    , rdat     :: UI.DataSet         -- ^ rdat   : 
-- type EventData = (LocalTime, EvContents)
    , dset     :: [UI.DataSet]       -- ^ dset   : 直前までのデータセット（最大 maxData個）
    , echan    :: BC.BChan ECU.Event -- ^ echan  : Brickのイベントをやりとりするチャンネル
    , cchan    :: TChan ECU.UCommand -- ^ cchan  : ECUにコマンドを送り込むチャンネル
    , lchan    :: TChan ECU.Event    -- ^ lchan  : 
    , inmenu   :: !Bool              -- ^ inmenu : 
    , after    :: !AfterAction       -- ^ shutdown :: whether or not shutdown system after quit this app
    , menu     :: !MenuItem
    , lIacPos  :: !(Maybe Int)       -- ^ latest iac position
    , iCoolT   :: !(Maybe Int)       -- ^ initial coolant temperature
    } 
--
-- * UI Attribute Map
--
normalAttr , errorAttr , alertAttr , pgcompAttr , pgtodoAttr , espeedAttr
 , thpotAttr , msensAttr , batvAttr , mnotselectedAttr , mselectedAttr :: AttrName
-- | 黒背景色に白字
normalAttr       = attrName "normalAttr"
-- | 黄背景色に赤字
errorAttr        = attrName "errorAttr"
-- | 赤字；Boldはよくわからない
alertAttr        = attrName "alertAttr"
pgcompAttr       = attrName "progressComplete"
pgtodoAttr       = attrName "progressIncomplete"
espeedAttr       = attrName "espeedAttr"                  :: AttrName
thpotAttr        = attrName "thpotAttr"                   :: AttrName
msensAttr        = attrName "msensAttr"                   :: AttrName
batvAttr         = attrName "batvAttr"                    :: AttrName
mnotselectedAttr = attrName "menuisnotselectedAttr" :: AttrName
mselectedAttr    = attrName "menuisselectedAttr"    :: AttrName
--
-- | Attribution map used in Brick system
theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (normalAttr, V.white `on` V.black)
    , (errorAttr,  V.red `on` V.yellow)
    , (alertAttr,  fg V.red `V.withStyle` V.bold)
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
    , (F.invalidFormInputAttr, V.white `on` V.red)
    , (F.focusedFormInputAttr, V.black `on` V.yellow)
    ]
--
-- * event handlers 
--
-- |　-- event handlers as an updating model function モデル更新関数群
handleEvent :: BrickEvent Name ECU.Event -> EventM Name Status ()
handleEvent MouseUp   {} = continueWithoutRedraw
handleEvent MouseDown {} = continueWithoutRedraw
-- | to react App Events
handleEvent (AppEvent (t,ECU.PortNotFound f)) = do
  s <- Brick.get
  Brick.put $ s { rdat = UI.DataSet { UI.evnt = ( t,ECU.PortNotFound f ) 
                              , UI.note = "Port Not Found: " ++ show f 
                              }
  }
-- | 
handleEvent (AppEvent ev@(_,ECU.Connected m)) = do
  s <- Brick.get
  Brick.put $ s { model = ECU.model m
          , rdat  = UI.DataSet { UI.evnt = ev
                                , UI.note = "Connected to " ++ show m
                                }
  }
-- | 
handleEvent (AppEvent ev@(_,ECU.Error estr)) = do
  s <- Brick.get
  Brick.put $ s { rdat = UI.DataSet { evnt = ev 
                              , note = "Error " ++ estr
                              }
  }
-- | 
handleEvent (AppEvent ev@(_,ECU.Done msg)) = do
  s <- Brick.get
  Brick.put $ s { rdat = UI.DataSet { evnt = ev
                              , note = "Done: " ++ msg
                              }
  }
--
handleEvent (AppEvent (t,ECU.GotIACPos p)) = do
  s <- get
  put $ s { rdat = s.rdat { evnt = ( t,ECU.GotIACPos p )
                           , note = "Got IAC Position"  } 
          , lIacPos = Just p 
          }
-- | for Raspberry Pi OS
handleEvent (AppEvent (_,ECU.OffLined)) = do
  s <- get
  put $ s { after = Restart }
--  halt
--  
handleEvent (AppEvent (t,ECU.Tick r)) = do
  s <- get
  let f = ECU.parse r
  put $ s 
      { rdat = DataSet
          { evnt = ( t,ECU.Tick r )
          -- , gdat = 
          --         [ (engspeed,    ECU.engineSpeed  f )
          --         , (tposition,   ECU.ithrottlePot f )
          --         , (mapsensor,   ECU.mapSensor    f )
          --         , (battvoltage, ECU.ibattVoltage f )
          --         , (coolanttemp, ECU.coolantTemp  f )
          --         ]
          , note = "Tick"
          }
      , dset    = take maxData $ rdat s : dset s
      , lIacPos = Just $ ECU.idleACMP f
      , iCoolT  = case iCoolT s of
          Just _  -> iCoolT s
          Nothing -> Just $ ECU.coolantTemp f  
      }
--
-- * Event Handlers as a part of UI
--
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do { s <- get ; put $ s { after = Quit } ; halt }
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = do { s <- get ; put $ s { after = Restart } ; halt }
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = do { s <- get ; put $ s { after = Shutdown } ; halt }
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  s <- get
  put $ s { mode = if mode s == UI.Loop then UI.Line else Loop }
handleEvent (VtyEvent (V.EvKey (V.KChar '1') [])) = do
  s <- get
  t <- liftIO currentTime
  case mode s of
    UI.Line -> do
                let ch = cchan s
                liftIO ( atomically $ STM.writeTChan ch ECU.Init )
                put $ s { rdat = UI.DataSet { evnt = (t,ECU.Done "Init command issued.") , note = "I will initialize mems." }}

    _       -> continueWithoutRedraw
-- handleEvent s (VtyEvent (V.EvKey (V.KChar '2') []))
--   | not $ testmode s = continue s { rdat = (rdat s) { note = ""}}
--   | otherwise        = do
--         let ch = cchan s
--         liftIO $ atomically $ writeTChan ch ECU.Disconnect
--         continue s { rdat = (rdat s) { note = "I will disconnect mems."}}
-- handleEvent s (VtyEvent (V.EvKey (V.KChar '0') []))
--   | inmenu s && not (testmode s) = do
--         let ch = cchan s
--         liftIO $ atomically $ writeTChan ch ECU.ClearFaults
--         continue s { rdat = (rdat s) { note = "I will clear faults."}}
--   | otherwise = continue s { rdat = (rdat s) { note = "Got clear fault command but do nothing."}}
-- -- | 'p' -> Get IAC Position 
-- -- handleEvent s (VtyEvent (V.EvKey (V.KChar 'p') []))
-- --   | inmenu s && not (testmode s) = do
-- --         let ch = cchan s
-- --         liftIO $ atomically $ writeTChan ch ECU.GetIACPos
-- --         continue s { rdat = (rdat s) { note = "I will get IAC Position."}}
-- --   | otherwise = continue s { rdat = (rdat s) { note = ""}}
-- handleEvent s (VtyEvent (V.EvKey V.KRight []))
--   | not $ inmenu s = continue s
--   | otherwise = do
--             let ch = cchan s
--             liftIO $ atomically $ writeTChan ch ECU.IncIACPos
--             continue s { rdat = (rdat s) { note = "I will increment IAC Pos." }}
-- handleEvent s (VtyEvent (V.EvKey V.KLeft [])) 
--   | not $ inmenu s = continue s
--   | otherwise = do
--             let ch = cchan s
--             liftIO $ atomically $ writeTChan ch ECU.DecIACPos
--             continue s { rdat = (rdat s) { note = "I will decrement IAC Pos." }}
-- handleEvent s (VtyEvent (V.EvKey V.KUp []))
--   | not $ inmenu s = continue s
--   | otherwise = do
--             let ch = cchan s
--             liftIO $ atomically $ writeTChan ch ECU.IncIgAd
--             continue s { rdat = (rdat s) { note = "I will increment Ignition Ad." }}
-- handleEvent s (VtyEvent (V.EvKey V.KDown [])) 
--   | not $ inmenu s = continue s
--   | otherwise = do
--             -- t <- liftIO Lib.currentTime
--             let ch = cchan s
--             liftIO $ atomically $ writeTChan ch ECU.DecIgAd
--             continue s { rdat = (rdat s) { note = "I will decrement Ignition Ad." }}
-- -- | 'd' -> get dummy data
-- --     This is a temporal handler which produce a dummy data
-- --    If the machine is in a test mode, this function do nothing.
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  s <- get
  t <- liftIO currentTime
  r <- liftIO ECU.dummyData807d
  let f = ECU.parse r
  put $ s { 
              rdat = UI.DataSet {
                  evnt = ( t,ECU.Tick r )
                , note    = "I have got dummy data" }
                -- , gdat =  [ (engspeed,    ECU.engineSpeed  f )
                --           , (tposition,   ECU.ithrottlePot f )
                --           , (mapsensor,   ECU.mapSensor    f )
                --           , (battvoltage, ECU.ibattVoltage f )
                --           , (coolanttemp, ECU.coolantTemp  f )
                --           ]
            , dset    = take maxData $ rdat s : dset s
            , lIacPos = Just $ ECU.idleACMP f 
            , iCoolT  = case iCoolT s of
                          Just _  -> iCoolT s
                          Nothing -> Just $ ECU.coolantTemp f
            }
--
-- handleEvent s (VtyEvent (V.EvKey (V.KChar _) [])) = continue s
-- --
handleEvent  (VtyEvent _ ) = continueWithoutRedraw
-- --
-- * for test
--
-- | get dummy status with random data
getDummyStatus :: Status -> IO Status
getDummyStatus s = do
  dummyLiveData <- ECU.dummyData807d
  jikoku        <- Lib.currentTime
  let d = DataSet { evnt = (jikoku, ECU.Tick dummyLiveData)
                  , note = "dummy data"
                  } 
  return s
    { -- , model = ECU.dummyModel
      rdat = d
    , dset = take maxData $ d : dset s
    }
--
-- * UI components related
--
maxGraphLength :: Int  -- ^ Graph Plot Area width limitation
maxGraphLength = 20
-- dset:: V.Vector ECU.EvContents
-- dset = V.singleton ECU.OffLined
--
initialState :: IO Status
initialState = do
  a <- System.Environment.getArgs
  e <- System.Environment.getEnv "HOME"
  j <- currentTime
  let ( p , e' ) = case a of
            []    ->  if e == "/Users/kuono" then ( defaultUSBPathMac , MacOS )
                                             else ( defaultUSBPathRaspberryPi , RaspberryPiOS )
            [opt] -> ( opt , UnsupportedOS )   
            _     -> error "error: exactly one arguments needed."
  exist <- doesFileExist p
  let c = if exist then ECU.OffLined else ECU.PortNotFound p
  eChan <- BC.newBChan 10 :: IO (BC.BChan ECU.Event) 
  tChan <- atomically STM.newTChan :: IO (TChan ECU.UCommand) 
  lChan <- atomically STM.newTChan :: IO (TChan ECU.Event   ) 
  return Status
    { mode = Loop
    , env  = e' 
    , model = ECU.MNEUnknown
    , rdat = DataSet { evnt = (j,c),note = "" }
    , dset = []
    , echan = eChan
    , cchan = tChan
    , lchan = lChan
    , inmenu = False
    , after  = Quit
    , menu   = ""
    , lIacPos = Nothing
    , iCoolT = Nothing 
    } 
--
drawInitialScreen :: String -- ^ current version 
                  -> Widget Name
drawInitialScreen v =
      str   "    \\              '             `"
  <=> str   "     .              .             `     ,_ .`"
  <=> str   "--_,   ,            |              /  /`. +'.'"
  <=> str   "+ + \". .===========================w. || = =  "
  <=> str   "= = :|V-Monitor for Rover Mini MEMS-\\\\ \\.+_+.'"
  <=> str   "- -,\"|:--------- ------------ -------:|     |"
  <=> str ( take 37 ( " .   \\\\-Version " ++ v ++ "              "  ) ++ "-//   .  ." )
  <=> str   "`.`  .^.== +--------------------+ == .`.'.` ,"
  <=> str   "-----| |-- |  by Kentaro UONO   |----| |-''' "
  <=> str   "````  '    +--------------------+     '"

--
-- Drawing
--
drawPanes :: Status -> [Widget Name]
drawPanes ecu
  | env ecu  == MacOS =
    [ withBorderStyle BS.unicodeRounded $ B.borderWithLabel (str "Rover Mini MEMS Monitor") $
            vLimit 1
            ( drawEcuStatus ecu <+> drawTime ecu )
        <=> drawMenu ecu
--        <=> ( B.hBorderWithLabel (str "MENU")
--             <=> B.hBorder
--        )
        <=> draw807dData ecu--  <+> B.vBorder
        <=> drawIACPos ecu
        -- <=> B.hBorderWithLabel ( str "Fault Status")
        -- <=> drawEcuFaultStatus ecu
        <=> B.hBorderWithLabel ( str "Note")
        <=> drawEcuErrorContents ecu
        <=> drawNote ecu
    ]
  -- in case of raspberian os or others, redundunt message would not be displayed
  | otherwise =
    [ -- withBorderStyle BS.unicodeRounded $ B.borderWithLabel (str "Rover Mini MEMS Monitor") $
            vLimit 1
            ( drawEcuStatus ecu <+> drawTime ecu )
        -- <=> drawMenu ecu
        -- <=> B.hBorderWithLabel (str "MENU")
            --  <=> B.hBorder
        <=> draw807dData ecu--  <+> B.vBorder
        <=> drawIACPos ecu
        -- <=> B.hBorderWithLabel ( str "Fault Status")
        -- <=> drawEcuFaultStatus ecu
        -- <=> B.hBorderWithLabel ( str "Note")
        <=> drawEcuErrorContents ecu
        <=> drawNote ecu
    ]

--
drawNote :: Status -> Widget Name
drawNote s = str . note $ rdat s
--
drawEcuStatus :: Status -> Widget Name
drawEcuStatus s = viewport StatusPane Horizontal $ hLimit 30 $
  case event s of
    ECU.PortNotFound f -> withAttr errorAttr  $ str (" Port Not Found :" ++ f ) <+> B.vBorder
    ECU.Connected _    -> withAttr normalAttr $ str (" Connected. " ++ show (model s) ++ "(" ++ show (ECU.d80size $ frameData s) ++ "," ++ show (ECU.d7dsize $ frameData s) ++ ")") <+> B.vBorder
    ECU.OffLined       -> withAttr alertAttr  $ str " Off Line                      " <+> B.vBorder
    ECU.Tick _         ->
      withAttr  (if True then normalAttr else alertAttr)
       $ str (" Connected. " ++ show ( model s ) ++ "(" ++ show  ( ECU.d80size (frameData s)) ++ "," ++ show (ECU.d7dsize ( frameData s )) ++ ")") <+> B.vBorder
    ECU.GotIACPos _    -> withAttr normalAttr $ str (" Connected. " ++ show (model s) ++ "(" ++ show (ECU.d80size $ frameData s) ++ "," ++ show (ECU.d7dsize $ frameData s) ++ ")") <+> B.vBorder
    ECU.Error m        -> withAttr errorAttr  $ str (" Error             : " ++ m ) <+> B.vBorder
    _                  -> emptyWidget
--
drawEcuErrorContents :: Status -> Widget Name
drawEcuErrorContents s = viewport ErrorContentsPane Horizontal $
    case event s of
      ECU.PortNotFound f -> withAttr errorAttr  $ str f
      ECU.Connected m    -> withAttr normalAttr $ str . show $ ECU.longName m
      ECU.OffLined       -> withAttr errorAttr  $ str "     "
      ECU.Tick _         -> emptyWidget
      ECU.Error s'       -> withAttr errorAttr  $ str s'
      _                  -> emptyWidget
--
drawTime :: Status -> Widget Name
drawTime s = withAttr ( if sodd s then mselectedAttr else mnotselectedAttr ) $ tstr s
  where
    sec :: LocalTime -> Int
    sec (LocalTime _ t') = truncate $ todSec t'
    time = fst . evnt . rdat
    sodd = odd . sec . time
    tstr = str . take 22 . show . time
--
draw807dData :: Status -> Widget Name
draw807dData s = vLimit 21 $
                      B.hBorderWithLabel ( str "Data 80/7D" )
                  <=> ( hLimit 41 ( drawData s <=> drawEcuFaultStatus s )
                        <+> B.vBorder
                        <+> (   drawGraph' s
                            <=> B.hBorderWithLabel (str "Unknown 80 data / Fault Bytes")
                            <=>  str          " 0B 0F 10 11 15 19 1A 1B / 0D 0E"
                            <=>  str ( printf " %2x %2x %2x %2x %2x %2x %2x %2x / %2x %2x" (ECU.unknown0B d') (ECU.unknown0F d') (ECU.unknown10 d') (ECU.unknown11 d') (ECU.unknown15 d') (ECU.unknown19 d') (ECU.unknown1A d') (ECU.unknown1B d') (ECU.faultCode0D d') (ECU.faultCode0E d') )
                            )
                      )
  where
          d' = ECU.parse $ case event s of
                  ECU.Tick r -> r
                  _          -> ECU.emptyData807d
--  vLimit 21 $ case event s of
--   ECU.PortNotFound p -> drawInitialScreen ver date-- hLimit 60 $ vBox [drawData s]
--   _                  -> -- ECU.OffLine or ECU.OnLine
--         B.hBorderWithLabel ( str "Data 80/7D" )
--     <=> ( hLimit 41 (drawData s) <+> B.vBorder <+> drawGraph' s )
--
drawBar :: Status -> Widget Name
drawBar s = {- vLimit 19 $ hLimit 20 $ -} vBox
  [ case event s of
      ECU.PortNotFound _ -> emptyWidget
      _                  -> -- ECU.OffLine or ECU.OnLine
        B.borderWithLabel (str "Graph") $ viewport GraphPane Vertical $
              BP.progressBar Nothing (ratio 0    3500 $ ECU.engineSpeed d)
          <=> BP.progressBar Nothing (ratio 0.0  4.0  $ ECU.throttlePot d)
          <=> BP.progressBar Nothing (ratio 0    130  $ ECU.mapSensor   d)
          <=> BP.progressBar Nothing (ratio 11.0 15.0 $ ECU.battVoltage d)
  ]
  where d :: ECU.Frame 
        d = frameData s
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
        { c'width    = maxGraphLength -- 20
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
    e t = snd . evnt $ dset s !! round t
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
          -- Initialized _  -> 'i'
          -- ECU.LiveData _     -> '*'
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
      -- case env s of
      --  MacOS ->      ( str ( "0D:" ++ e0d ) <+> ge " (01) COOLANT " e01 <+> ge " (02) A. TEMP " e02 <+> ge " (??) AMBIENT " ex4 <+> ge " (??) F. TEMP " ex5 )
      --                <=> ( str ( "0E:" ++ e0e ) <+> ge " (10) F. PUMP " e10 <+> ge " (??) MAP.S   " ey5 <+> ge " (16) T. POT  " e16 )
      --   _     ->      ( str ( "0D:" ++ e0d ) 
      --                <+> 
                      --       ge " (01) CLNT " e01 
                      --   <+> ge " (02) ATMP " e02 
                      --   <+> ge " (??) AMBT " ex4 
                      --   <+> ge " (??) FTMP " ex5 -- )
                        (   ge "01.CLNT" e01
                        <+> ge "02.ATMP" e02
                        <+> ge "--.AMBT" ex4
                        <+> ge "--.FTMP" ex5 )
                      <=>
      --                ( str ( "0E:" ++ e0e ) 
      --                  <+> 
                        (   ge "10.FPMP" e10
                        <+> ge "--.MAPS" ey5
                        <+> ge "16.TPOT" e16 )
      where
        ge m f =  let atr = if f then errorAttr else normalAttr
                  in      ( withAttr normalAttr (str " ") )
                      <+> ( withAttr atr ( str m ) )
                      <+> ( withAttr normalAttr (str " ") )
        d' = ECU.parse $ case snd . evnt $ rdat s of 
              ECU.Tick r ->  r
              _          -> ECU.emptyData807d
        -- e0d = printf "%2X" $ ECU.faultCode0D d'
        -- e0e = printf "%2X" $ ECU.faultCode0E d'
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

-- | frame to table
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
      --(tf (ECU.idleSwitch  f )) -- ::Bool ,-- 0x0A	Idle switch. Bit 4 will be set if the throttle is closed, and it will be clear otherwise.
      (ECU.idleByte    f ) -- ::Int , at 0x0a 
      (ECU.unknown0B   f ) -- ::Word8,-- 0x0B	Unknown. Probably a bitfield. Observed as 0x24 with engine off, and 0x20 with engine running. A single sample during a fifteen minute test drive showed a value of 0x30.
      (ECU.pnClosed    f ) -- :: 0x0C ::Park/neutral switch. Zero is closed, nonzero is open.
      -- (tf (ECU.faultCode1  f )) -- 0x0D * Bit 0: Coolant temp sensor fault (Code 1)
      -- (tf (ECU.faultCode2  f )) --      * Bit 1: Inlet air temp sensor fault (Code 2)
      -- (tf (ECU.faultCode10 f )) -- 0x0E * Bit 1: Fuel pump circuit fault (Code 10)
      -- (tf (ECU.faultCode16 f )) --      * Bit 7: Throttle pot circuit fault (Code 16)
      (ECU.faultCode0D     f )
      (ECU.faultCode0E     f )
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
--
drawData :: Status -> Widget Name
drawData s = viewport DataPane Vertical $ case event s of
  ECU.PortNotFound _ -> UI.drawInitialScreen ver
  _                  -> -- ECU.OffLine or ECU.OnLine
               str ( printf "   Engine Speed (rpm) :   %5d o "    ( ECU.engineSpeed d' ) ) <+> hLimit 10 ( BP.progressBar Nothing (dratio 0 3500 ECU.engineSpeed)    )
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
        <=>  if model s == ECU.MNE10078
               then str ( printf " park or neutral      : %4s  %02X"  ( parkorneutral ( ECU.pnClosed d' ) ) ( ECU.pnClosed d') ++ aconoff (ECU.pnClosed d') )
               else str ( printf " Cooler (MEMS 1.3J)   : %10s" (aconoff (ECU.pnClosed d')) )
        <=>  str ( printf "    idle switch       :    %04s " idleSwitch )
               -- " ( ECU.idleByte d'    ) ) -- ( closedorclear (ECU.idleSwitch d' ) ) )
        <=>  str ( printf "idl Air Ctl M P(C/O)  :     %3d "    ( ECU.idleACMP d'   ) )
        <=>  str ( printf "idl Spd deviatn       :   %5d + "    ( ECU.idleSpdDev d' ) )
        <=>  str ( printf "ignition advnce (deg) :  %6.2f # "   ( ECU.ignitionAd d' ) )
        <=>  str ( printf "      coil Time (msc) : %7.2f "      ( ECU.coilTime d'   ) )
        <=>  str ( printf " lambda voltage ( mV) :    %4d   "   ( ECU.lambda_voltage d' ) ++ richorlean ( ECU.lambda_voltage d' ) )
        <=>  str ( printf "      closed loop     :     %3d   "  ( ECU.closed_loop'   d' ) ++ openorclosed ( ECU.closed_loop' d' ) )
        <=>  str ( printf "      fuel trim ( %% ) :     %3d  "  ( ECU.fuel_trim' d'     ) )
  where
    d' :: ECU.Frame 
    d' = frameData s
    coolantTemp :: Int
    coolantTemp = ECU.coolantTemp d'
    idleSwitch :: String
    idleSwitch  = if testBit ( ECU.idleByte d' ) ( if model s == ECU.MNE10078 then 1 else 4 )
                          then "IDLE" else "T.ON"
    dratio:: (Real a) => a -> a -> (ECU.Frame -> a) -> Float
    dratio llimit hlimit f = case (dt <= llimit,dt >= hlimit) of
              (True , _ )  -> 0.0
              (_ , True )  -> 1.0
              _            -> fromRational ((toRational dt - toRational llimit ) / (toRational hlimit - toRational llimit))
             where dt = case event s of
                          ECU.Tick r -> f $ ECU.parse r
                          _          -> llimit
-- Prelude.putStrLn $ vt100mv 30 0  ++ "----------------- Log -------------------------"
-- mapM_ (Prelude.putStrLn . take 40 ) (if length logs >= 4 then take 4 logs else logs)
-- Prelude.putStrLn $ vt100mv 36 0  ++ "-----------------------------------------------" ++ vt100mv 3 0
--       where
          -- tf c = if c then {- green ++ -} "True " {- ++ reset -} else {- red ++ -} "False" -- ++ reset
          -- closedorclear::Bool -> String
          -- closedorclear b = if b then " Closed" else " Other "
event :: Status -> ECU.EvContents
event = snd . UI.evnt . rdat
--
frameData :: Status -> ECU.Frame
frameData s = ECU.parse $ case event s of
  ECU.Tick r -> r
  _          -> ECU.emptyData807d
testBitPlace :: Status -> Int
testBitPlace s 
          | model s == ECU.MNE10078 = 1
          | otherwise               = 4
                                 
parkorneutral :: Int -> String -- 0 is closed
parkorneutral b = if b == 0 then "Clsd" else "Open"
aconoff :: Int -> String
aconoff       d = if d == 0 then {- bblue ++ yellow ++ -}   " a/c on   " {- ++ reset -}
                                else {- bgreen ++ yellow ++ -}  " a/c off  " {- ++ reset -}
richorlean::Int -> String
richorlean v   = if v >= 450 then {- bred ++ green  ++ -}   " rich     " {- ++ reset -}
                                 else {- bgreen ++ yellow ++ -} " lean     " {- ++ reset -}
openorclosed::Int -> String
openorclosed d = if d == 0   then {- bred ++ green  ++ -}   "Crl wt FDt" {- ++ reset -}
                                 else {- bgreen ++ yellow ++ -} "Crl wt O2d" {- ++ reset -}
--
drawMenu :: Status -> Widget Name
drawMenu s = str $ menu s
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
        let a = ctx ^. attrL 
        return $ Result (V.charFill a ch (ctx^.availWidthL) (ctx^.availHeightL))
                        [] [] [] Brick.BorderMap.empty
--