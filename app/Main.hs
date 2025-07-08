module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Foreign.C.Types
import GHC.Word
import Linear.Affine
import Linear.V2
import qualified SDL

-- strength runs from 0.0 -> 1.0
data StaticSource = StaticSource {ssPos :: Pt, ssStrength :: Double}

fieldAt :: StaticSource -> Pt -> Double
fieldAt src pt =
    let
        dist = (distanceA (ssPos src) pt) / 1000.0
    in
        if dist == 0 then 0 else 1.0 / (dist * dist)

data Hevicle = Hevicle {pos :: Pt, theta :: Double}

data Scene = Scene {fields :: [StaticSource], hevicles :: [Hevicle]}

type Pt = V2 Double
type Vec = V2 Double
type Colour = SDL.V4 Word8

data DrawCtx = DrawCtx Config SDL.Renderer

screenSize :: DrawCtx -> Pt
screenSize (DrawCtx conf _) = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    sdlMain

data Config = Config {w :: Integer, h :: Integer, appName :: T.Text}

sdlMain :: IO ()
sdlMain = do
    SDL.initializeAll

    let conf = Config{w = 1024, h = 768, appName = T.pack "Hevicle"}
    window <-
        SDL.createWindow
            (appName conf)
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (fromIntegral $ w conf) (fromIntegral $ h conf)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    let dc = DrawCtx conf renderer
    let sSize = screenSize dc

    let scene =
            Scene
                { fields =
                    -- [StaticSource{ssPos = (0.5 .+^ sSize), ssStrength = 1.0}]
                    [StaticSource{ssPos = fmap (/ 2.0) sSize, ssStrength = 1.0}]
                , hevicles = [Hevicle{pos = V2 0 0, theta = 135}]
                }

    sdlLoop dc scene 0
    SDL.destroyWindow window

toSDLPt :: Pt -> SDL.Point SDL.V2 CInt
toSDLPt (V2 x y) = SDL.P $ SDL.V2 (round x) (round y)

drawLine :: DrawCtx -> Colour -> Pt -> Pt -> IO ()
drawLine (DrawCtx _ r) colour fr to = do
    SDL.rendererDrawColor r SDL.$= colour
    SDL.drawLine r (toSDLPt fr) (toSDLPt to)

drawCircle :: DrawCtx -> Colour -> Pt -> Double -> IO ()
drawCircle dc col pt radius = do
    let steps = 20
    let thetas = map (* (2 * pi / steps)) [0 .. steps]
    let offsets = map (\t -> (radius * cos t, radius * sin t)) thetas
    let pts = map (\(dx, dy) -> pt + V2 dx dy) offsets
    mapM_ (\(fr, to) -> drawLine dc col fr to) $ zip pts (drop 1 pts)

drawStaticSource :: DrawCtx -> Colour -> StaticSource -> IO ()
drawStaticSource dc col src = do
    -- let fr = ssPos src
    -- let to = fr + V2 0.0 30.0
    -- drawLine dc baseCol fr to
    let centre = ssPos src
    drawCircle dc col centre 20

drawHevicle :: DrawCtx -> Colour -> Hevicle -> IO ()
drawHevicle dc baseCol hv = do
    let fr = pos hv
    let l = 20
    let rt = (theta hv * pi * 180)
    let to = fr + l .+^ (V2 (cos rt) (sin rt))
    drawLine dc baseCol fr to

drawFields :: DrawCtx -> [StaticSource] -> Colour -> IO ()
drawFields dc srcs colour = do
    mapM_ (drawStaticSource dc colour) srcs

drawHevicles :: DrawCtx -> [Hevicle] -> Colour -> IO ()
drawHevicles dc hvs col = do
    mapM_ (drawHevicle dc col) hvs

drawScene :: DrawCtx -> Scene -> Integer -> IO ()
drawScene dc@(DrawCtx _ r) scene _ = do
    let bgCol = SDL.V4 0 0 0 255
    let srcCol = SDL.V4 0 0 255 255
    let hvCol = SDL.V4 0 255 0 255
    SDL.rendererDrawColor r SDL.$= bgCol
    SDL.clear r
    drawFields dc (fields scene) srcCol

    {-
    let fgCol = SDL.V4 0 255 0 255
    let x1 = tick `mod` (w conf)
    let fr = V2 (fromIntegral x1) 0
    let to = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)
    drawLine dc fgCol fr to
    -}

    drawHevicles dc (hevicles scene) hvCol
    SDL.present r

sdlLoop :: DrawCtx -> Scene -> Integer -> IO ()
sdlLoop dc scene tick = do
    events <- SDL.pollEvents
    let eventIsQPress event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events

    drawScene dc scene tick
    threadDelay (10 * 1000)
    unless qPressed (sdlLoop dc scene (tick + 1))
