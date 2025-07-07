module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Foreign.C.Types
import GHC.Word
import Linear.V2
import qualified SDL

class Field a where
    fieldAt :: a -> Pt -> Double

--    fieldAt src p =
--        let d =

data StaticSource = StaticSource {ssPos :: Pt, ssStrength :: Double}

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
    let origin = V2 0.0 0.0

    let scene =
            Scene
                { fields =
                    [StaticSource{ssPos = fmap (/ 2.0) sSize, ssStrength = 100.0}]
                , hevicles = [Hevicle{pos = origin, theta = 135}]
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

drawStaticSource :: Config -> SDL.Renderer -> Colour -> StaticSource -> IO ()
drawStaticSource _ renderer colour src = do
    let fr = ssPos src
    let to = fr + V2 0.0 30.0
    drawLine renderer colour fr to

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
