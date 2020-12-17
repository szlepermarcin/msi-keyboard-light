{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MsiKeyboard
  ( run
  )
where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types              as A
import qualified Data.ByteString               as BS
import           Data.Functor
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Word
import qualified Data.Yaml                     as Y
import           GHC.Generics
import qualified Hid                           as H


type MsiKeyboardIO a = ReaderT H.Device IO a

data Mode = Normal | Gaming | Breathe | Demo | Wave
data Color =  Off | Red | Orange | Yellow | Green | Sky | Blue | Purple | White
data Region = KeybLeft | KeybMiddle | KeybRight | Logo | FrontLeft | FrontRight | Touchpad
data Intensity = High | Medium | Low | Light
data RGB = RGB Word8 Word8 Word8

data RegionPreset = RegionPreset {color :: Color, intensity :: Intensity} deriving Generic
data FullPreset = FullPreset {left :: RegionPreset, middle :: RegionPreset, right:: RegionPreset} deriving Generic


run :: FilePath -> Maybe String -> IO ()
run configFile preset =
  (readConfig configFile <&> maybe (listToMaybe . M.elems) M.lookup preset)
    >>= maybe (pure ()) (runMsiKeyboardIO . applyPreset)

readConfig :: FromJSON a => FilePath -> IO a
readConfig f | "yaml" `isSuffixOf` f = Y.decodeFileThrow f
             | otherwise = eitherDecodeFileStrict f >>= either fail pure

instance FromJSON RegionPreset
instance FromJSON FullPreset

modeLabels :: M.Map String Mode
modeLabels = M.fromList
  [ ("normal" , Normal)
  , ("gaming" , Gaming)
  , ("breathe", Breathe)
  , ("demo"   , Demo)
  , ("wave"   , Wave)
  ]

colorLabels :: M.Map String Color
colorLabels = M.fromList
  [ ("off"   , Off)
  , ("red"   , Red)
  , ("orange", Orange)
  , ("yellow", Yellow)
  , ("green" , Green)
  , ("sky"   , Sky)
  , ("blue"  , Blue)
  , ("white" , White)
  ]

regionLabels :: M.Map String Region
regionLabels = M.fromList
  [ ("keybLeft"  , KeybLeft)
  , ("keybMiddle", KeybMiddle)
  , ("keybRight" , KeybRight)
  , ("logo"      , Logo)
  , ("frontLeft" , FrontLeft)
  , ("frontRight", FrontRight)
  , ("touchpad"  , Touchpad)
  ]

intensityLabels :: M.Map String Intensity
intensityLabels = M.fromList
  [("high", High), ("medium", Medium), ("low", Low), ("light", Light)]

surround :: String -> String -> String
surround s t = s ++ t ++ s

errorMsgForMap :: String -> String -> M.Map String a -> String
errorMsgForMap name x m =
  "Invalid "
    ++ name
    ++ " "
    ++ surround "\"" x
    ++ ", expected one of: "
    ++ (intercalate ", " . map (surround "\"")) (M.keys m)

mapToParser :: String -> M.Map String a -> Value -> Parser a
mapToParser name m =
  withText name
    $ (\x -> maybe (fail $ errorMsgForMap name x m) pure (M.lookup x m))
    . T.unpack

instance FromJSON Mode where
  parseJSON = mapToParser "Mode" modeLabels

instance FromJSON Color where
  parseJSON = mapToParser "Color" colorLabels

instance FromJSON Region where
  parseJSON = mapToParser "Region" regionLabels

instance FromJSON Intensity where
  parseJSON = mapToParser "Intensity" intensityLabels

applyPreset :: FullPreset -> MsiKeyboardIO ()
applyPreset (FullPreset l m r) = do
  applyRegion KeybLeft   l
  applyRegion KeybMiddle m
  applyRegion KeybRight  r
  setMode Normal
  where applyRegion r (RegionPreset c i) = setColorPreset r c i

class AsReportPart a where toWord8 :: a -> Word8

instance AsReportPart Mode where
  toWord8 mode = case mode of
    Normal  -> 1
    Gaming  -> 2
    Breathe -> 3
    Demo    -> 4
    Wave    -> 5

instance AsReportPart Color where
  toWord8 color = case color of
    Off    -> 0
    Red    -> 1
    Orange -> 2
    Yellow -> 3
    Green  -> 4
    Sky    -> 5
    Blue   -> 6
    Purple -> 7
    White  -> 8

instance AsReportPart Region where
  toWord8 region = case region of
    KeybLeft   -> 1
    KeybMiddle -> 2
    KeybRight  -> 3
    Logo       -> 4
    FrontLeft  -> 5
    FrontRight -> 6
    Touchpad   -> 7

instance AsReportPart Intensity where
  toWord8 intensity = case intensity of
    High   -> 0
    Medium -> 1
    Low    -> 2
    Light  -> 3

toReportPart :: (AsReportPart a) => a -> Word8
toReportPart = toWord8


setColor :: Region -> RGB -> MsiKeyboardIO ()
setColor region (RGB red green blue) = do
  let reportData =
        BS.pack [1, 2, 66, toReportPart region, red, green, blue, 236]
  dev <- ask
  liftIO $ H.sendFeatureReport dev reportData
  return ()

setMode :: Mode -> MsiKeyboardIO ()
setMode mode = do
  let reportData = BS.pack [1, 2, 65, toReportPart mode, 0, 0, 0, 236]
  dev <- ask
  liftIO $ H.sendFeatureReport dev reportData
  return ()

setColorPreset :: Region -> Color -> Intensity -> MsiKeyboardIO ()
setColorPreset region color intensity = do
  let reportData = BS.pack
        [ 1
        , 2
        , 66
        , toReportPart region
        , toReportPart color
        , toReportPart intensity
        , 0
        , 236
        ]
  dev <- ask
  liftIO $ H.sendFeatureReport dev reportData
  return ()

vendorId :: H.VendorID
vendorId = 0x1770

productId :: H.ProductID
productId = 0xff00

runMsiKeyboardIO :: MsiKeyboardIO a -> IO a
runMsiKeyboardIO f = H.withHIDAPI $ do
  dev <- H.open vendorId productId Nothing
  a   <- runReaderT f dev
  H.close dev
  return a

