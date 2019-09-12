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
import Control.Monad.STM
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as Ex
import qualified Data.ByteString   as BS
import Text.Printf

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))   

type GraphData = [[Dots]]
type Dots = (Char,Char)
data GraphRange = OverUL | Over75 | InBetween7550 | InBetween5025 | Under25 | UnderLL deriving (Eq,Ord)

maxGraphLength = 40

-- 
-- UI Name space
-- 
data Display = Dialog | DataPanel | CurrentStatus | CurrentData | GraphLog | BarLog | TextLog

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
