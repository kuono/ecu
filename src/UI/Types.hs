module UI.Types where

import Lib
import Brick
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Edit as E
import Brick.Forms as F


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
-- Form for default Data and function
--
-- | types for user information on Form Dialog
data UserInfo =
  FormState { _portAddress    :: T.Text     -- ^ USB - シリアルコネクタを接続したポート名
            , _logFolderPath  :: T.Text     -- ^ ログファイルを格納する場所の名前
            , _logNameRule    :: T.Text     -- ^ ログファイルの名前の生成規則
            } deriving (Show)

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
