module Draw where

import qualified Data.Text as T
import qualified Data.Vector.Storable as SV

-- import Debug.Trace
import Foreign.C.Types
import GHC.Word
import Linear.V2
import qualified SDL

type Pt = V2 Double

ptFromTuple :: (Double, Double) -> Pt
ptFromTuple (x, y) = V2 x y

data Config = Config {w :: Integer, h :: Integer, appName :: String}

data Ctx = Ctx Config SDL.Renderer SDL.Window

type Colour = SDL.V4 Word8

black :: Colour
black = SDL.V4 0 0 0 255

white :: Colour
white = SDL.V4 255 255 255 255

red :: Colour
red = SDL.V4 255 0 0 255

green :: Colour
green = SDL.V4 0 255 0 255

blue :: Colour
blue = SDL.V4 0 0 255 255

init :: Config -> IO Ctx
init conf = do
    SDL.initializeAll
    window <-
        SDL.createWindow
            (T.pack $ appName conf)
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 (fromIntegral $ w conf) (fromIntegral $ h conf)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    return $ Ctx conf renderer window

done :: Ctx -> IO ()
done (Ctx _ _ window) =
    SDL.destroyWindow window

screenSize :: Ctx -> Pt
screenSize (Ctx conf _ _) = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)

-- A drawable gets a local coord space from [-1,-1] to [+1,+1]
--
-- to map to world space, it needs:
-- a centre (in world coords)
-- angle (theta) in degrees (zero is +ve x axis, anticlockwise increasing)
-- width (in world coords)
-- height (in world coords)
class Drawable a where
    centre :: a -> Pt
    theta :: a -> Double
    width :: a -> Double
    height :: a -> Double

    localLines :: a -> [[Pt]]

    forward :: a -> Pt
    forward dw = ptRotate (theta dw) (V2 0 1)

    -- Translate to world coords from local coords
    -- [-1,-1] -> [1,1] => [-w/2,-h/2] -> [+w/2,+h/2] (scale)
    -- rotate
    -- offset
    toW :: a -> Pt -> Pt
    toW dw (V2 x y) =
        let
            x_sc = x * width dw / 2
            y_sc = y * height dw / 2
            p_rot = ptRotate (theta dw) (V2 x_sc y_sc)
        in
            -- traceShow ((x, y), (x_sc, y_sc), (x_rot, y_rot), p_rot)
            (p_rot + centre dw)

    -- Call localLines and toW to draw on the context in the colour provided
    draw :: Ctx -> Colour -> a -> IO ()
    draw dc@(Ctx _ renderer _) col dw = do
        mapM_ drawOneLine $ localLines dw
      where
        drawOneLine line = do
            let wpts = map (toW dw) line
            let sdlPts = map (toSDL dc) wpts
            SDL.rendererDrawColor renderer SDL.$= col
            SDL.drawLines renderer $ SV.fromList sdlPts

-- Could have separate scales for world and SDL in Ctx
toSDL :: Ctx -> Pt -> SDL.Point SDL.V2 CInt
toSDL _ (V2 x y) = SDL.P $ SDL.V2 (round x) (round y)

{-
drawLine :: Ctx -> Colour -> Pt -> Pt -> IO ()
drawLine dc@(Ctx _ r _) colour fr to = do
    SDL.rendererDrawColor r SDL.$= colour
    SDL.drawLine r (toSDL dc fr) (toSDL dc to)
    -}

clearScreen :: Ctx -> Colour -> IO ()
clearScreen (Ctx _ r _) col = do
    SDL.rendererDrawColor r SDL.$= col
    SDL.clear r

shouldExit :: Ctx -> IO Bool
shouldExit _ = do
    events <- SDL.pollEvents
    let eventIsQPress event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
                        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events
    return $ qPressed

present :: Ctx -> IO ()
present (Ctx _ r _) = do
    SDL.present r

ptRotate :: Double -> Pt -> Pt
ptRotate degrees (V2 x y) =
    let rads = degrees * pi / 180

        x_rot = x * cos rads - y * sin rads
        y_rot = x * sin rads + y * cos rads
    in V2 x_rot y_rot

ptScale :: Double -> Pt -> Pt
ptScale s = fmap (* s)
