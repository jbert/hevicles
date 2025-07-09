module Main where

import Control.Concurrent
import Control.Monad
import qualified Draw as D
import Linear.Affine
import Linear.V2

-- strength runs from 0.0 -> 1.0
data StaticSource = StaticSource
    { ssCentre :: D.Pt
    , ssStrength :: Double
    , ssWidth :: Double
    , ssHeight :: Double
    }
    deriving (Show)

instance D.Drawable StaticSource where
    centre = ssCentre
    theta _ = 0
    width = ssWidth
    height = ssHeight
    localLines = staticSourceLines

fieldAt :: StaticSource -> D.Pt -> Double
fieldAt (StaticSource centre strength _ _) pt =
    let
        dist = (distanceA centre pt)
        scale = 1000.0 * strength
    in
        if dist == 0 then 0 else scale / dist

data Hevicle = Hevicle
    { hvCentre :: D.Pt
    , hvTheta :: Double
    , hvWidth :: Double
    , hvHeight :: Double
    }
    deriving (Show)

instance D.Drawable Hevicle where
    centre = hvCentre
    theta = hvTheta
    width = hvWidth
    height = hvHeight
    localLines = hevicleLines

data Scene = Scene {fields :: [StaticSource], hevicles :: [Hevicle]}

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    sdlMain

sdlMain :: IO ()
sdlMain = do
    dc <- D.init D.Config{D.w = 1024, D.h = 768, D.appName = "Hevicle"}

    let sSize = D.screenSize dc

    let scene =
            Scene
                { fields =
                    -- [StaticSource{ssCentre = fmap (/ 2.0) sSize, ssStrength = 1.0}]
                    [StaticSource (0.5 * sSize) 1.0 20.0 20.0]
                , hevicles =
                    [ Hevicle{hvCentre = V2 30 30, hvTheta = -45, hvWidth = 20.0, hvHeight = 40.0}
                    , Hevicle{hvCentre = sSize - V2 40 40, hvTheta = 90, hvWidth = 20.0, hvHeight = 40.0}
                    ]
                }

    sdlLoop dc scene 0
    D.done dc

circleLines :: Double -> Double -> [D.Pt]
circleLines steps radius =
    let
        thetas = map (* (2 * pi / steps)) [0 .. steps]
    in
        map (\t -> V2 (radius * cos t) (radius * sin t)) thetas

staticSourceLines :: StaticSource -> [[D.Pt]]
staticSourceLines _ =
    [circleLines 20.0 1.0]

hevicleLines :: Hevicle -> [[D.Pt]]
hevicleLines _ =
    map
        (map D.ptFromTuple)
        [ [(-1, -1), (1, -1), (1, 1), (-1, 1), (-1, -1)]
        , [(-0.2, 0.8), (0, 1), (0.2, 0.8)]
        , [(0, -1), (0, 1)]
        ]

drawFields :: D.Ctx -> [StaticSource] -> D.Colour -> IO ()
drawFields dc srcs colour = do
    mapM_ (D.draw dc colour) srcs

drawHevicles :: D.Ctx -> [Hevicle] -> D.Colour -> IO ()
drawHevicles dc hvs col = do
    mapM_ (D.draw dc col) hvs

updateHevicle :: Hevicle -> Hevicle
updateHevicle hv =
    let
        f = D.forward hv
        speed = 1.0
    in
        hv{hvCentre = (hvCentre hv) + speed * f}

updateScene :: Scene -> Scene
updateScene scene =
    scene{hevicles = map updateHevicle (hevicles scene)}

drawScene :: D.Ctx -> Scene -> Integer -> IO ()
drawScene dc scene _ = do
    D.clearScreen dc D.black
    drawFields dc (fields scene) D.red

    {-
    let fgCol = SDL.V4 0 255 0 255
    let x1 = tick `mod` (w conf)
    let fr = V2 (fromIntegral x1) 0
    let to = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)
    drawLine dc fgCol fr to
    -}

    drawHevicles dc (hevicles scene) D.green
    D.present dc

sdlLoop :: D.Ctx -> Scene -> Integer -> IO ()
sdlLoop dc scene tick = do
    shouldExit <- D.shouldExit dc
    -- Pass tick to allow animations
    drawScene dc scene tick
    let scene' = updateScene scene
    threadDelay (10 * 1000)
    unless shouldExit (sdlLoop dc scene' (tick + 1))
