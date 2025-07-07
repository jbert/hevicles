module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Foreign.C.Types
import GHC.Word
import Linear.Affine
import Linear.V2
import qualified SDL

-- class Field a where
-- fieldAt :: a -> Pt -> Double

--    fieldAt src p =
--        let d =

-- strength runs from 0.0 -> 1.0
data StaticSource = StaticSource {ssPos :: Pt, ssStrength :: Double}

fieldAt :: StaticSource -> Pt -> Double
fieldAt src pt =
    let
        dist = (distanceA (ssPos src) pt) / 1000.0
    in
        if dist == 0 then 0 else 1.0 / (dist * dist)

-- type Fields = StaticSource

data Hevicle = Hevicle {pos :: Pt, theta :: Double}

data Scene = Scene {fields :: [StaticSource], hevicles :: [Hevicle]}

type Pt = V2 Double
type Vec = V2 Double
type Colour = SDL.V4 Word8

screenSize :: Config -> Pt
screenSize conf = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    sdlMain

data Config = Config {w :: Integer, h :: Integer, appName :: T.Text}

sdlMain :: IO ()
sdlMain = do
    SDL.initializeAll

    let conf = Config{w = 1024, h = 768, appName = T.pack "Hevicle"}
    let sSize = screenSize conf

    let scene =
            Scene
                { fields =
                    -- [StaticSource{ssPos = (0.5 .+^ sSize), ssStrength = 1.0}]
                    [StaticSource{ssPos = fmap (/ 2.0) sSize, ssStrength = 1.0}]
                , hevicles = [Hevicle{pos = V2 0 0, theta = 135}]
                }

    window <-
        SDL.createWindow
            (appName conf)
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (fromIntegral $ w conf) (fromIntegral $ h conf)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    sdlLoop conf scene renderer 0
    SDL.destroyWindow window

toSDLPt :: Pt -> SDL.Point SDL.V2 CInt
toSDLPt (V2 x y) = SDL.P $ SDL.V2 (round x) (round y)

drawLine :: SDL.Renderer -> Colour -> Pt -> Pt -> IO ()
drawLine renderer colour fr to = do
    SDL.rendererDrawColor renderer SDL.$= colour
    SDL.drawLine renderer (toSDLPt fr) (toSDLPt to)

drawPoint :: SDL.Renderer -> Colour -> Pt -> IO ()
drawPoint renderer colour pt = do
    SDL.rendererDrawColor renderer SDL.$= colour
    SDL.drawPoint renderer (toSDLPt pt)

drawAll :: Config -> SDL.Renderer -> (Pt -> Colour) -> IO ()
drawAll c r ptColour = do
    let pts = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    mapM_ drawIntPoint' pts
  where
    width = w c
    height = h c
    drawIntPoint' (x, y) = do
        let colour = ptColour $ V2 (fromIntegral x) (fromIntegral y)
        SDL.rendererDrawColor r SDL.$= colour
        SDL.drawPoint r $ SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)

scaleColour :: Colour -> Double -> Colour
scaleColour base s =
    fmap (\x -> round $ (*) s $ fromIntegral x) base

drawStaticSource :: Config -> SDL.Renderer -> Colour -> StaticSource -> IO ()
drawStaticSource c r baseCol src = do
    drawAll c r ptCol
  where
    ptCol pt =
        let s = fieldAt src pt
        in scaleColour baseCol s

{-
    let fr = ssPos src
    let to = fr + V2 0.0 30.0
    drawLine renderer colour fr to
    -}

drawFields :: Config -> SDL.Renderer -> Scene -> Colour -> IO ()
drawFields conf renderer scene colour = do
    mapM_ (drawStaticSource conf renderer colour) (fields scene)

drawScene :: Config -> Scene -> SDL.Renderer -> Integer -> IO ()
drawScene conf scene renderer tick = do
    let bgCol = SDL.V4 0 0 0 255
    let srcCol = SDL.V4 0 0 255 255
    let fgCol = SDL.V4 0 255 0 255
    SDL.rendererDrawColor renderer SDL.$= bgCol
    SDL.clear renderer
    drawFields conf renderer scene srcCol

    let x1 = tick `mod` (w conf)
    let fr = V2 (fromIntegral x1) 0
    let to = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)

    drawLine renderer fgCol fr to
    SDL.present renderer

sdlLoop :: Config -> Scene -> SDL.Renderer -> Integer -> IO ()
sdlLoop conf scene renderer tick = do
    events <- SDL.pollEvents
    let eventIsQPress event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events

    drawScene conf scene renderer tick
    threadDelay (10 * 1000)
    unless qPressed (sdlLoop conf scene renderer (tick + 1))
