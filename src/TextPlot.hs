-- , MultiParamTypeClasses, 
{-# LANGUAGE TypeSynonymInstances, TupleSections, FunctionalDependencies, FlexibleInstances #-}
--
-- Copyright (c) Kentaro UONO 2019
-- Copyright (c) William Tennien Murphy 2011
-- Copyright (c) Sergey Astanin 2012
--

module TextPlot
    ( -- * Data types
      PlotFunction
    , Line, Cell
    , Range
    , Plot ()
    , EditPlot(..), (.+), (.-), (.|)
      -- * Plot types
    -- , PlotMode(..)
    , PlotColour(..)
    , XYPlot(..), emptyXYPlot
    , ParamXYPlot(..), ParamFunction(..), emptyParamXYPlot
    , PolarPlot(..), PolarFunction(..), emptyPolarPlot
    -- * Screen representation
    , PlotConfig(..), defaultConfig
    -- * Output
    , plotStr, plotCell
    -- , plot
    , plotStrWithConfig, plotCellWithConfig
    -- , printPlot
    -- * Example
    -- $example
    ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array
import Data.Array.ST
import Text.Printf (printf)

data PlotColour = White | Red | Green | Blue | Cyan | Yellow | Magenta deriving (Eq,Show)
type Cell       = (PlotColour,Char)
type Line       = [Cell]
type TextPlot   = [Line]
-- data PlotMode   = Brick | VT100 | Braille | None deriving (Eq,Show)

-- | Range of values @(from, to)@.
type Range = (Double, Double)

-- some reasonable default values
defaultXrange :: Range
defaultXrange = (0.0, 1.0)
defaultYrange :: Range
defaultYrange = (0.0, 1.0)

-- | A function of one variable (@x -> y@).
type PlotFunction = Double -> Double
-- {-# LANGUAGE FlexibleInstances #-}
instance Show PlotFunction where show = const "<Tete Plot function::Double->Double>"

-- | A type class with functional dependency to allow the same
-- editing operations across all plot types.
class Plot plot => EditPlot plot function | plot -> function where
    thenPlot :: plot -> function -> plot  -- ^ add another function to the plot
    xlim     :: plot -> Range -> plot -- ^ set limits of the abscissa (x) axis
    ylim     :: plot -> Range -> plot -- ^ set limits of the ordinate (y) axis

-- | Shortcut to 'thenPlot'. Mnemonics: plus to add another function.
(.+) :: EditPlot p f => p -> f -> p
(.+) = thenPlot
-- | Shortcut to 'xlim'. Mnemonics: horizontal bar followed by horizontal range.
(.-) :: EditPlot p f => p -> Range -> p
(.-) = xlim
-- | Shortcut to 'ylim'. Mnemonics: vertical bar followed by vertical range.
(.|) :: EditPlot p f => p -> Range -> p
(.|) = ylim

-- | A type class to access plot dimensions in unform manner across
-- all plot types.
class Plot plot => Dimensions plot where
   getXlim :: plot -> Range
   getYlim :: plot -> Range

-- | Plot one or more functions (@x -> y@) in Cartesian coordinates.
data XYPlot = XYPlot {
      fp'xlim :: Range -- ^ limits of the abscissa (x) axis
    , fp'ylim :: Range -- ^ limits of the ordinate (y) axis
    , fp'functions :: [PlotFunction] -- ^ functions to plot
    } deriving Show

-- | A default empty 'XYPlot' with bounds of a unit square.
emptyXYPlot :: XYPlot
emptyXYPlot = XYPlot defaultXrange defaultYrange []

instance EditPlot XYPlot PlotFunction where
   thenPlot plot f = let fs = fp'functions plot
                     in  plot { fp'functions = f:fs }
   xlim plot r = plot { fp'xlim = r }
   ylim plot r = plot { fp'ylim = r }

instance Dimensions XYPlot where
    getXlim = fp'xlim
    getYlim = fp'ylim

data ParamFunction = ParamFunction {
      xfun :: PlotFunction  -- ^ the first parametrized variable (@t -> x@)
    , yfun :: PlotFunction  -- ^ the second parametrized variable (@t -> y@)
    , tlim :: Range  -- ^ range of the free parameter @t@
    } deriving Show

-- | Plot one or more parametric functions in Cartesian coordiantes.
data ParamXYPlot = ParamXYPlot {
      param'xlim :: Range -- ^ limits of the abscissa (x) axis
    , param'ylim :: Range -- ^ limits of the ordinate (y) axis
    , param'functions :: [ParamFunction] -- ^ functions to plot
    } deriving Show

-- | A default empty 'ParamXYPlot'
emptyParamXYPlot :: ParamXYPlot
emptyParamXYPlot = ParamXYPlot defaultXrange defaultYrange []

instance EditPlot ParamXYPlot ParamFunction where
   thenPlot plot f = let fs = param'functions plot
                     in  plot { param'functions = f:fs }
   xlim plot r = plot { param'xlim = r }
   ylim plot r = plot { param'ylim = r }

instance Dimensions ParamXYPlot where
    getXlim = param'xlim
    getYlim = param'ylim

data PolarFunction = PolarFunction {
      rfun   :: PlotFunction -- ^ radius as a function of angle @phi@ (@phi -> r@)
    , philim :: (Double,Double) -- ^ range of the angle argument @phi@
    }  deriving Show

-- | Plot one or more functions in polar coordinates.
data PolarPlot = PolarPlot {
      polar'xlim :: Range -- ^ limits of the abscissa (x) axis
    , polar'ylim :: Range -- ^ limits of the ordinate (y) axis
    , polar'functions :: [PolarFunction] -- ^ functions to plot
    } deriving Show

-- | A default empty 'PolarPlot'
emptyPolarPlot :: PolarPlot
emptyPolarPlot = PolarPlot defaultXrange defaultYrange []

-- instance EditPlot PolarPlot PolarFunction where
--    thenPlot plot f = let fs = polar'functions plot
--                      in  plot { polar'functions = f:fs }
--    xlim plot r = plot { polar'xlim = r }
--    ylim plot r = plot { polar'ylim = r }

-- instance Dimensions PolarPlot where
--     getXlim = polar'xlim
--     getYlim = polar'ylim

-- | Any kind of of plot.
class Plot a where
    draw :: PlotConfig -> a -> TextPlot

data PlotConfig = PlotConfig {
      c'width :: Int     -- ^ plot width in characters
    , c'height :: Int    -- ^ plot height in characters
    , c'samples :: Int   -- ^ samples per line
    , c'showAxes :: Bool -- ^ draw axes or not
    } deriving (Show, Eq)

-- | Default plot dimensions, suitable for 80x24 terminals.
defaultConfig :: PlotConfig
defaultConfig = PlotConfig 61 20 256 True -- Brick

instance Plot XYPlot where
    draw (PlotConfig width height _ showAxes ) plt =
      addAxes showAxes plt . fromArray $ runSTArray $ do
        arr <- createArray width height
        let xrange@(xmin,xmax) = fp'xlim plt
        let yrange = fp'ylim plt
        let dx = (xmax-xmin)/(fromIntegral width - 1)
        let xs = [ xmin + (fromIntegral c)*dx | c <- [0..width-1] ]
        forM_ (reverse (zip (fp'functions plt) symbols)) $
                  \(f, cell) -> markPoints xrange yrange arr cell xs (map f xs)
        return  arr

instance Plot ParamXYPlot where
    draw (PlotConfig width height samples showAxes ) plt =
      addAxes showAxes plt . fromArray $ runSTArray $ do
        arr <- createArray width height
        let xrange = param'xlim plt
        let yrange = param'ylim plt
        let fns = param'functions plt
        forM_ (reverse (zip fns symbols)) $
              \(f, (clr,sym)) -> do
                let (tmin,tmax) = tlim f
                let dt = (tmax-tmin)/(fromIntegral samples - 1)
                let ts = [ (fromIntegral t)*dt | t <- [0..samples-1] ]
                let xs = map (xfun f) ts
                let ys = map (yfun f) ts
                markPoints xrange yrange arr (clr,sym) xs ys
        return arr

-- instance Plot PolarPlot where
--     draw (PlotConfig width height samples showAxes) plt =
--       addAxes showAxes plt . fromArray $ runSTArray $ do
--         arr <- createArray width height
--         let xrange = polar'xlim plt
--         let yrange = polar'ylim plt
--         let fns = polar'functions plt
--         forM_ (reverse (zip fns symbols)) $
--               \(f, sym) -> do
--                 let (phimin, phimax) = philim f
--                 let dphi = (phimax-phimin)/(fromIntegral samples - 1)
--                 let phis = [ (fromIntegral t)*dphi | t <- [0..samples-1] ]
--                 let rs = map (rfun f) phis
--                 let toCartesian (r,phi) = (r*cos phi, r*sin phi)
--                 let (xs,ys) = unzip . map toCartesian $ zip rs phis
--                 markPoints xrange yrange arr sym xs ys
--         return arr

-- | Convert a plot to a multiline 'Cell' with default configuration
plotCell :: Plot p => p -> [Line]
plotCell = plotCellWithConfig defaultConfig
-- | Convert a plot to a multiline 'Cell' with default configuration
plotStr :: Plot p => p -> String
plotStr = plotStrWithConfig defaultConfig

-- | Convert a plot to multiline 'Cell' with custom configuration
plotCellWithConfig :: Plot p => PlotConfig -> p -> [Line]
plotCellWithConfig config p = draw config p
-- | Convert a plot to multiline 'String' with custom configuration
plotStrWithConfig :: Plot p => PlotConfig -> p -> String
plotStrWithConfig config p = unlines $ map concatCell (draw config p)
  where
    concatCell :: [Cell] -> String
    concatCell line = map snd line
-- | Print a plot with default configuration
-- printPlot :: Plot p => p -> IO()
-- printPlot = putStr . plot

{--------------------- backend array operations ---------------------------}

-- | Symbols to use for different plots.
--   with VT100 colour escape sequence
symbols :: [(PlotColour,Char)]
symbols = cycle [(Red,'o'),(Green,'x'),(Blue,'#'),(Yellow,'*'),(Magenta,'@'),(Cyan,'+')]
-- -- | Escape sequence in VT100 / set text foreground colour
black    = "\ESC[30m" 
red      = "\ESC[31m" 
green    = "\ESC[32m"  
yellow   = "\ESC[33m"
blue     = "\ESC[34m"
magenta  = "\ESC[35m"
cyan     = "\ESC[36m"
white    = "\ESC[37m"
-- -- | Escape sequence in VT100 / set text background Colours
-- bblack   = "\ESC[40m"--	Black
-- bred     = "\ESC[41m"--	Red
-- bgreen   = "\ESC[42m"--	Green
-- byellow  = "\ESC[43m"--	Yellow
-- bblue    = "\ESC[44m"--	Blue
-- bmagenta = "\ESC[45m"--	Magenta
-- bcyan    = "\ESC[46m"--	Cyan
-- bwhite   = "\ESC[47m"--	White 

-- | Create an 'STArray' of given screen dimensions
createArray :: Int -> Int -> ST s (STArray s (Int,Int) Cell)
createArray width height = do
  let screenDims = ((0,0),(height-1,width-1))
  newArray screenDims (White,' ') :: ST s (STArray s (Int,Int) Cell)

-- | Mark (x,y) points in a two-dimensional array of 'Char'
markPoints :: Range    -- ^ @x@ range
           -> Range    -- ^ @y@ range
           -> STArray s (Int,Int) Cell -- ^ an array we operate on
           -> Cell     -- ^ mark symbol with colour escape sequence string
           -> [Double] -- ^ @xs@
           -> [Double] -- ^ @ys@
           -> ST s (STArray s (Int,Int) Cell)
markPoints (xmin,xmax) (ymin,ymax) arr (clr,sym) xs ys = do
  ((rmin,cmin),(rmax,cmax)) <- getBounds arr
  let width = cmax-cmin+1
  let height = rmax-rmin+1
  let w = fromIntegral width
  let h = fromIntegral height
  let dx = (xmax-xmin)/(w-1)  -- larger dx, dy steps to guarantee that
  let dy = (ymax-ymin)/(h-1)  -- max values stay within plot bounds
  let cols = [ round$(x-xmin)/dx | x <- xs ]
  let rows = [ round$(h-1-(y-ymin)/dy) | y <- ys ]
  forM_ (zip cols rows) $ \(c, r) ->
      when (r >= rmin && r <= rmax && c >= cmin && c < cmax) $
           writeArray arr (r,c) (clr,sym)
  return arr

-- | Convert a two dimensional array to a list of lists
fromArray :: Array (Int,Int) a -> [[a]]
fromArray arr = splitEvery width (elems arr)
  where
    splitEvery :: Int -> [a] -> [[a]]
    splitEvery _ [] = []
    splitEvery n xs = (take n xs) : splitEvery n (drop n xs)
    width :: Int
    width = let ((_,colmin),(_,colmax)) = bounds arr
            in  colmax - colmin + 1

addAxes :: Dimensions plot => Bool -> plot -> TextPlot -> TextPlot
addAxes False _ txt = txt
addAxes True p txt = addYAxis (getYlim p) . addXAxis (getXlim p) $ txt

addXAxis :: Range -> TextPlot -> TextPlot
addXAxis (xmin,xmax) lns  =
    let w =  maximum . map length $ lns
        xminLabel = printf "%-g" (toF xmin)
        xmaxLabel = printf "%g"  (toF xmax)
        axis   = "+" ++ replicate (w-2) '-' ++ "+->"
        padw   = w - (length xminLabel + length xmaxLabel)
        labels = xminLabel ++ replicate padw ' ' ++ xmaxLabel
    in  lns ++ [ map (White,) axis,map (White,) labels]

addYAxis :: Range -> TextPlot -> TextPlot
addYAxis (ymin,ymax) lns =
    let minLabel = printf "%g" (toF ymin)
        maxLabel = printf "%g" (toF ymax)
        lw    = max (length minLabel) (length maxLabel) + 1
        tip   = map (White,) $ replicate lw ' ' ++ "^"
        maxL  = map (White,) $ replicate (lw - length maxLabel - 1) ' ' ++ maxLabel ++ " +"
        midL  = map (White,) $ replicate lw ' ' ++ "|"
        minL  = map (White,) $ replicate (lw - length minLabel - 1) ' ' ++ minLabel ++ " +"
        axisL = replicate (lw + 1) (White,' ')
        n = length lns
        labels = (tip:maxL:(replicate (n-4) midL)) ++  [minL, axisL, axisL]
    in  zipWith (++) labels ([(White,' ')]:lns)

toF :: Double -> Float
toF = fromRational . toRational

-- $example
--
-- Plot a mexican hat wavelet function:
--
-- > ghci> let hat t = 0.5*(1-t**2)*exp(-0.5*t**2)/(sqrt (3*(sqrt pi)))
-- > ghci> let plot = emptyXYPlot .+ hat .- (-5,5) .| (-0.125,0.25)
-- > ghci> printPlot plot
-- >        ^
-- >   0.25 +
-- >        |
-- >        |                             ooo
-- >        |                            o   o
-- >        |
-- >        |                           o     o
-- >        |
-- >        |
-- >        |                          o       o
-- >        |
-- >        |                         o         o
-- >        |
-- >        |
-- >        |oooooooooooo            o           o            ooooooooooo
-- >        |            oo                                 oo
-- >        |              oo       o             o       oo
-- >        |                o     o               o     o
-- >        |                 ooo o                 o ooo
-- >        |                    o                   o
-- > -0.125 +
-- >         +-----------------------------------------------------------+->
-- >         -5.0                                                      5.0
--
-- A parametric plot:
--
-- > ghci> let circle = ParamFunction sin cos (0,2*pi)
-- > ghci> let paramplot = emptyParamXYPlot `thenPlot` circle `xlim` (-1.1,1.1) `ylim` (-1.1,1.1)
-- > ghci> printPlot paramplot
-- >      ^
-- >  1.1 +
-- >      |                    ooooooooooooooooooooo
-- >      |              ooooooo                   ooooooo
-- >      |           oooo                               oooo
-- >      |        ooo                                       ooo
-- >      |      ooo                                           ooo
-- >      |     oo                                               oo
-- >      |    o                                                   o
-- >      |   o                                                     o
-- >      |   o                                                     o
-- >      |   o                                                     o
-- >      |   o                                                     o
-- >      |    o                                                   o
-- >      |     oo                                               oo
-- >      |      oo                                             oo
-- >      |        ooo                                       ooo
-- >      |          ooooo                               ooooo
-- >      |               ooooo                     ooooo
-- >      |                    ooooooooooooooooooooo
-- > -1.1 +
-- >       +-----------------------------------------------------------+->
-- >       -1.1                                                      1.1
--