module Main where

import Control.Concurrent
import Control.Monad
import qualified Draw as D
import Linear.Affine
import Linear.V2

data Resource = Light | Food deriving (Show, Eq)

-- strength runs from 0.0 -> 1.0
data Field = StaticSource
    { ssCentre :: D.Pt
    , ssStrength :: Double
    , ssWidth :: Double
    , ssHeight :: Double
    , ssResource :: Resource
    }
    deriving (Show)

instance D.Drawable Field where
    centre = ssCentre
    theta _ = 0
    width = ssWidth
    height = ssHeight
    localLines sc@(StaticSource _ _ _ _ _) = staticSourceLines sc

fieldAt :: Resource -> D.Pt -> Field -> Double
fieldAt wantedResource pt (StaticSource centre strength _ _ resource)
    | wantedResource /= resource = 0
    | otherwise =
        let
            dist = (distanceA centre pt)
            scale = 1000.0 * strength
        in
            if dist == 0 then 0 else scale / dist

data Location = LocLeft | LocCentre | LocRight deriving (Show, Eq)

data Source = Sensor {sensorLoc :: Location, sensorResource :: Resource} deriving (Show)

data Driver = Driver {driverSource :: Source} | ZeroDriver
    deriving (Show)

data Hevicle = Hevicle
    { hvCentre :: D.Pt
    , hvTheta :: Double
    , hvWidth :: Double
    , hvHeight :: Double
    , hvLeftDriver :: Driver
    , hvCentreDriver :: Driver
    , hvRightDriver :: Driver
    }
    deriving (Show)

instance D.Drawable Hevicle where
    centre = hvCentre
    theta = hvTheta
    width = hvWidth
    height = hvHeight
    localLines = hevicleLines

data Scene = Scene {sceneFields :: [Field], sceneHevicles :: [Hevicle]}

main :: IO ()
main = sdlMain

sdlMain :: IO ()
sdlMain = do
    dc <- D.init D.Config{D.w = 1024, D.h = 768, D.appName = "Hevicle"}

    let sSize = D.screenSize dc

    let scene =
            Scene
                { sceneFields =
                    -- [StaticSource{ssCentre = fmap (/ 2.0) sSize, ssStrength = 1.0}]
                    [ StaticSource (0.5 * sSize) 1.0 20.0 20.0 Light
                    , StaticSource (0.75 * sSize) 1.0 20.0 20.0 Food
                    ]
                , sceneHevicles =
                    [ Hevicle
                        { hvCentre = V2 30 30
                        , hvTheta = -45
                        , hvWidth = 20.0
                        , hvHeight = 40.0
                        , hvLeftDriver = ZeroDriver
                        , hvCentreDriver = Driver (Sensor LocCentre Light)
                        , hvRightDriver = ZeroDriver
                        }
                    , Hevicle
                        { hvCentre = V2 830 200
                        , hvTheta = 45
                        , hvWidth = 20.0
                        , hvHeight = 40.0
                        , hvLeftDriver = ZeroDriver
                        , hvCentreDriver = Driver (Sensor LocCentre Food)
                        , hvRightDriver = ZeroDriver
                        }
                    , Hevicle
                        { hvCentre = V2 400 200
                        , hvTheta = 45
                        , hvWidth = 20.0
                        , hvHeight = 40.0
                        , hvLeftDriver = Driver (Sensor LocLeft Food)
                        , hvCentreDriver = ZeroDriver
                        , hvRightDriver = Driver (Sensor LocRight Food)
                        }
                    , Hevicle
                        { hvCentre = V2 400 200
                        , hvTheta = 45
                        , hvWidth = 20.0
                        , hvHeight = 40.0
                        , hvLeftDriver = Driver (Sensor LocRight Food)
                        , hvCentreDriver = ZeroDriver
                        , hvRightDriver = Driver (Sensor LocLeft Food)
                        }
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

staticSourceLines :: Field -> [[D.Pt]]
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

drawFields :: D.Ctx -> [Field] -> D.Colour -> IO ()
drawFields dc srcs colour = do
    mapM_ (D.draw dc colour) srcs

drawHevicles :: D.Ctx -> [Hevicle] -> D.Colour -> IO ()
drawHevicles dc hvs col = do
    mapM_ (D.draw dc col) hvs

senseFieldAt :: [Field] -> Resource -> D.Pt -> Double
senseFieldAt fields resource pt =
    sum $ map (fieldAt resource pt) fields

sensorLocalPt :: Location -> D.Pt
sensorLocalPt LocLeft = V2 (-1) 1
sensorLocalPt LocCentre = V2 0 1
sensorLocalPt LocRight = V2 1 1

driverSpeed :: (D.Pt -> D.Pt) -> [Field] -> Driver -> Double
driverSpeed locToWorld fields (Driver sensor) =
    senseFieldAt fields (sensorResource sensor) (locToWorld $ sensorLocalPt loc)
  where
    loc = sensorLoc sensor
driverSpeed _ _ ZeroDriver = 0

updateHevicle :: [Field] -> Hevicle -> Hevicle
updateHevicle fields hv =
    let
        leftSpeed = driverSpeed (D.toW hv) fields $ hvLeftDriver hv
        centreSpeed = driverSpeed (D.toW hv) fields $ hvCentreDriver hv
        rightSpeed = driverSpeed (D.toW hv) fields $ hvRightDriver hv

        -- For simplicity, we'll take the effective speed as the mean speed of the drivers
        -- we can assume slippage or something as the reason
        -- we should do some geometry.
        speed = (leftSpeed + centreSpeed + rightSpeed) / 3

        -- We'll turn a given angle for each +1 speed difference
        turnPerSpeedDiff = 360 / 40
        leftTurnAngle = (rightSpeed - leftSpeed) * turnPerSpeedDiff

        fwd = D.ptRotate leftTurnAngle $ D.forward hv
    in
        hv{hvCentre = (hvCentre hv) + D.ptScale speed fwd, hvTheta = (hvTheta hv) + leftTurnAngle}

updateScene :: Scene -> Scene
updateScene scene =
    scene{sceneHevicles = map (updateHevicle (sceneFields scene)) (sceneHevicles scene)}

drawScene :: D.Ctx -> Scene -> Integer -> IO ()
drawScene dc scene _ = do
    D.clearScreen dc D.black
    drawFields dc (sceneFields scene) D.red

    {-
    let fgCol = SDL.V4 0 255 0 255
    let x1 = tick `mod` (w conf)
    let fr = V2 (fromIntegral x1) 0
    let to = V2 (fromIntegral $ w conf) (fromIntegral $ h conf)
    drawLine dc fgCol fr to
    -}

    drawHevicles dc (sceneHevicles scene) D.green
    D.present dc

sdlLoop :: D.Ctx -> Scene -> Integer -> IO ()
sdlLoop dc scene tick = do
    shouldExit <- D.shouldExit dc
    -- Pass tick to allow animations
    drawScene dc scene tick
    let scene' = updateScene scene
    threadDelay (10 * 1000)
    unless shouldExit (sdlLoop dc scene' (tick + 1))
