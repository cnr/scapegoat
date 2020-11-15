{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Main where

import Data.Colour.SRGB
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Image
import Diagrams.TwoD.Size
import Diagrams.TwoD.Text

cardWidth :: Double
cardWidth = 474

-- red = 1
-- blue = 2
-- yellow = 3
-- green = 4
-- brown = 5
-- purple = 6

main :: IO ()
main = do
    ---------- GOAT DECK
    redImg <- loadImage "../raw-assets/scapegoat_joe.png"
    blueImg <- loadImage "../raw-assets/scapegoat_bill.png"
    yellowImg <- loadImage "../raw-assets/scapegoat_outsourced.png"
    greenImg <- loadImage "../raw-assets/scapegoat_connor.png"
    brownImg <- loadImage "../raw-assets/scapegoat_brad.png"
    purpleImg <- loadImage "../raw-assets/scapegoat_lance.png"

    let red = Goat "joe" redImg (sRGB24 0xED 0x24 0x29)
    let blue = Goat "bill" blueImg (sRGB24 0x17 0xA3 0xDD)
    let yellow = Goat "outsourced" yellowImg (sRGB24 0xF4 0xEB 0x22)
    let green = Goat "connor" greenImg (sRGB24 0x00 0xA5 0x50)
    let brown = Goat "brad" brownImg (sRGB24 0xAC 0x6C 0x29)
    let purple = Goat "lance" purpleImg (sRGB24 0xA1 0x79 0xB2)

    let goats = [red,blue,yellow,green,brown,purple]

    greyImg <- loadImage "../raw-assets/scapegoat_maja.png"
    let innocent = Goat "maja" greyImg (sRGB24 0x4F 0x50 0x50)

    -- 6x6 goat "multiplication table" matrix
    let cardMatrix = mkCardMatrix goats
    -- card with six goats on it
    let sixGoats = sixGoatCard red blue yellow green brown purple
    -- full-grey innocent goat card
    let innocentCard = oneGoatCard innocent

    -- row 1 col 7 is six goats
    -- row 2 col 7 is innocent goat
    renderSVG "goat_deck.svg" nosize (cardMatrix ||| (sixGoats === innocentCard))

    backImg <- loadImage "../raw-assets/scapegoat_back.png"
    let backGoat = Goat "back" backImg (sRGB24 0x00 0x00 0x00)
    renderSVG "goat_back.svg" nosize (oneGoatCard backGoat)

    ---------- GOAT PLACEMATS
    stepsImg <- loadImage "../raw-assets/turnsteps.png"
    renderSVG "placemat_red.svg" nosize $ mkPlacemat stepsImg red
    renderSVG "placemat_blue.svg" nosize $ mkPlacemat stepsImg blue
    renderSVG "placemat_yellow.svg" nosize $ mkPlacemat stepsImg yellow
    renderSVG "placemat_green.svg" nosize $ mkPlacemat stepsImg green
    renderSVG "placemat_brown.svg" nosize $ mkPlacemat stepsImg brown
    renderSVG "placemat_purple.svg" nosize $ mkPlacemat stepsImg purple

    ---------- LOCATION DECK
    -- scale to 50% size and rotate 1/4 turn right
    let formatLocation = scaleUToX 750 . rotate ((-90) @@ deg)
    prepareFrontImg <- formatLocation <$> loadImage "../raw-assets/location_prepare.png"
    prepareBackImg <- formatLocation <$> loadImage "../raw-assets/location_prepare_back.png"
    spyImg <- formatLocation <$> loadImage "../raw-assets/location_spy.png"
    stashImg <- formatLocation <$> loadImage "../raw-assets/location_stash.png"
    tradeImg <- formatLocation <$> loadImage "../raw-assets/location_trade.png"
    copsFrontImg <- formatLocation <$> loadImage "../raw-assets/location_cops.png"
    copsBackImg <- formatLocation <$> loadImage "../raw-assets/location_cops_back.png"

    let locationsFront = prepareFrontImg ||| stashImg ||| spyImg ||| tradeImg ||| copsFrontImg
    let locationsBack = prepareBackImg ||| stashImg ||| spyImg ||| tradeImg ||| copsBackImg

    renderSVG "locations_deck.svg" nosize locationsFront
    renderSVG "locations_back.svg" nosize locationsBack

    ---------- TOKENS

    renderSVG "token_red.svg" nosize $ mkToken red
    renderSVG "token_blue.svg" nosize $ mkToken blue
    renderSVG "token_yellow.svg" nosize $ mkToken yellow
    renderSVG "token_green.svg" nosize $ mkToken green
    renderSVG "token_brown.svg" nosize $ mkToken brown
    renderSVG "token_purple.svg" nosize $ mkToken purple

loadImage :: FilePath -> IO (Diagram SVG)
loadImage path = loadImageEmb path >>= either (ioError . userError) (pure . image)

nosize :: Num n => SizeSpec V2 n
nosize = mkSizeSpec2D Nothing Nothing

data Goat = Goat
    { goatName :: String
    , goatImage :: Diagram SVG
    , goatColor :: Colour Double
    }

instance Eq Goat where
    (Goat n _ _) == (Goat n' _ _) = n == n'

---------- Tokens

tokenWidth :: Double
tokenWidth = 237

mkToken :: Goat -> Diagram SVG
mkToken goat = scaled `atop` padder
    where
        scaled = goatImage goat # scaleUToX tokenWidth
        maxDimension = max (width scaled) (height scaled)
        padder = (rect maxDimension maxDimension :: Diagram SVG) # fillColor (goatColor goat) # lineWidth 0 # center

---------- Placemats

mkPlacemat :: Diagram SVG -> Goat -> Diagram SVG
mkPlacemat steps goat = content `atop` padder
    where
        content = ((steps # scaleUToY 800) ||| strutX 50 ||| (goatImage goat # scaleUToY 600)) # center
        padder = (rect (width content + 100) (height content + 100) :: Diagram SVG) # fillColor (goatColor goat) # lineWidth 0 # center


---------- Card matrix (one- and two-goat cards)

mkCardMatrix :: [Goat] -> Diagram SVG
mkCardMatrix goats = vcat [mkCardRow goats goat | goat <- goats]

mkCardRow :: [Goat] -> Goat -> Diagram SVG
mkCardRow goats goat = hcat $ map (mkCard goat) goats

mkCard :: Goat -> Goat -> Diagram SVG
mkCard cur other
  | cur == other = oneGoatCard cur
  | otherwise = twoGoatCard cur other

oneGoatCard :: Goat -> Diagram SVG
oneGoatCard goat = scaled `atop` padder
    where
        scaled = goatImage goat # scaleUToX cardWidth
        padder = (rect (width scaled) (height scaled * 2) :: Diagram SVG) # fillColor (goatColor goat) # lineWidth 0 # center

twoGoatCard :: Goat -> Goat -> Diagram SVG
twoGoatCard top bottom = (goatImage top === goatImage bottom) # scaleUToX cardWidth # center


---------- six-goat card

sixGoatCard :: Goat -> Goat -> Goat -> Goat -> Goat -> Goat -> Diagram SVG
sixGoatCard one two three four five six = ((mkOne one ||| mkOne two) === (mkOne three ||| mkOne four) === (mkOne five ||| mkOne six)) # center
    where
        scaled = goatImage one # scaleUToX cardWidth

        fullCardWidth = width scaled
        oneTileWide = fullCardWidth / 2

        fullCardHeight = height scaled * 2
        oneTileTall = fullCardHeight / 3

        -- make one 1/6 tile
        mkOne goat = goatImage goat # scaleUToX oneTileWide `atop` ((rect oneTileWide oneTileTall :: Diagram SVG) # fillColor (goatColor goat) # lineWidth 0 # center)
