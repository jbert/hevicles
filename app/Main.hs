module Main where

import Control.Concurrent
import Control.Monad
import Draw
import Linear.Affine
import Linear.V2

-- strength runs from 0.0 -> 1.0
data StaticSource = StaticSource
    { ssCentre :: Pt
    , ssStrength :: Double
    , ssWidth :: Double
    , ssHeight :: Double
    }
    deriving (Show)

instance Drawable StaticSource where
    centre = ssCentre
    theta _ = 0
    width = ssWidth
    height = ssHeight
    localLines = staticSourceLines

fieldAt :: StaticSource -> Pt -> Double
fieldAt src pt =
    let
        dist = (distanceA (ssCentre src) pt) / 1000.0
    in
        if dist == 0 then 0 else 1.0 / (dist * dist)

data Hevicle = Hevicle
    { hvCentre :: Pt
    , hvTheta :: Double
    , hvWidth :: Double
    , hvHeight :: Double
    }
    deriving (Show)

instance Drawable Hevicle where
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
    dc <- dcInit Config{w = 1024, h = 768, appName = "Hevicle"}

    let sSize = screenSize dc

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
    dcDone dc

circleLines :: Double -> Double -> [Pt]
circleLines steps radius =
    let
        thetas = map (* (2 * pi / steps)) [0 .. steps]
    in
        map (\t -> V2 (radius * cos t) (radius * sin t)) thetas

staticSourceLines :: StaticSource -> [[Pt]]
staticSourceLines _ =
    [circleLines 20.0 1.0]

hevicleLines :: Hevicle -> [[Pt]]
hevicleLines _ =
    map
        (map ptFromTuple)
        [ [(-1, -1), (1, -1), (1, 1), (-1, 1), (-1, -1)]
        , [(-0.2, 0.8), (0, 1), (0.2, 0.8)]
        , [(0, -1), (0, 1)]
        ]

drawFields :: DrawCtx -> [StaticSource] -> Colour -> IO ()
drawFields dc srcs colour = do
    mapM_ (draw dc colour) srcs

drawHevicles :: DrawCtx -> [Hevicle] -> Colour -> IO ()
drawHevicles dc hvs col = do
    mapM_ (draw dc col) hvs

updateHevicle :: Hevicle -> Hevicle
updateHevicle hv =
    let
        f = forward hv
        speed = 1.0
    in
        hv{hvCentre = (hvCentre hv) + speed * f}

updateScene :: Scene -> Scene
updateScene scene =
    scene{hevicles = map updateHevicle (hevicles scene)}

drawScene :: DrawCtx -> Scene -> Integer -> IO ()
drawScene dc scene _ = do
    drawClearScreen dc black
    drawFields dc (fields scene) red

    {-
    let fgCol = SDL.V4 0 255 0 255
    let x1 = tick `mod` (w conf)
    let fr = V2 (fromIntegral x1) 0
    let to = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)
    drawLine dc fgCol fr to
    -}

    drawHevicles dc (hevicles scene) green
    drawPresent dc

sdlLoop :: DrawCtx -> Scene -> Integer -> IO ()
sdlLoop dc scene tick = do
    shouldExit <- drawShouldExit dc
    -- Pass tick to allow animations
    drawScene dc scene tick
    let scene' = updateScene scene
    threadDelay (10 * 1000)
    unless shouldExit (sdlLoop dc scene' (tick + 1))
