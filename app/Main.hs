module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Foreign.C.Types
import qualified SDL

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    sdlMain

data Config = Config {w :: Integer, h :: Integer, appName :: T.Text}

data Point = P Integer Integer

sdlMain :: IO ()
sdlMain = do
    SDL.initializeAll

    let conf = Config{w = 1024, h = 768, appName = T.pack "Hevicle"}

    window <-
        SDL.createWindow
            (appName conf)
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (fromIntegral $ w conf) (fromIntegral $ h conf)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    sdlLoop conf renderer 0
    SDL.destroyWindow window

toSDLPt :: Point -> SDL.Point SDL.V2 CInt
toSDLPt (P x y) = SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)

drawScene :: Config -> SDL.Renderer -> Integer -> IO ()
drawScene conf renderer tick = do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
    SDL.clear renderer
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 0

    let x1 = tick `mod` (w conf)
    let fr = P x1 0
    let to = P (w conf) (h conf)

    SDL.drawLine renderer (toSDLPt fr) (toSDLPt to)
    SDL.present renderer

sdlLoop :: Config -> SDL.Renderer -> Integer -> IO ()
sdlLoop conf renderer tick = do
    events <- SDL.pollEvents
    let eventIsQPress event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events

    drawScene conf renderer tick
    threadDelay (10 * 1000)
    unless qPressed (sdlLoop conf renderer (tick + 1))
