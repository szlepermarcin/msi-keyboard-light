{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, DeriveGeneric #-}

module Hid
  ( Hid.init
  , exit
  , withHIDAPI
  , Hid.error
  , check
  , checkWithHidError
  , Hid.open
  , Hid.close
  , Hid.getSerialNumberString
  , Hid.sendFeatureReport
  , Device
  , VendorID
  , ProductID
  )
where

import qualified Data.ByteString               as BS
import           Control.Monad
import           Control.Exception
import           Data.Word
import           Data.Maybe
import           Data.Typeable
import           Data.Data
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Numeric                        ( showHex )
import           GHC.Generics                   ( Generic )

type HidDevicePtr = Ptr ()
newtype Device = Device HidDevicePtr

type VendorID = Word16
type ProductID = Word16
type SerialNumber = String
type FeatureReport = BS.ByteString


data HIDAPIException = HIDAPIException String String
  deriving (Data, Typeable, Generic)

instance Show HIDAPIException where
  showsPrec _ (HIDAPIException a c) =
    showString a . showString ": " . showString c

instance Exception HIDAPIException


_SERIAL_NUMBER_MAX_LENGTH :: Int
_SERIAL_NUMBER_MAX_LENGTH = 32768


-- Hidapi call
foreign import ccall "hid_init"
  hid_init :: IO CInt

foreign import ccall "hid_exit"
  hid_exit :: IO CInt

foreign import ccall "hid_error"
  hid_error :: HidDevicePtr -> IO CWString

foreign import ccall "hid_open"
  hid_open :: CUShort -> CUShort -> CWString -> IO HidDevicePtr

foreign import ccall "hid_close"
  hid_close :: Device -> IO ()

foreign import ccall "hid_get_serial_number_string"
  hid_get_serial_number_string :: Device -> CWString -> CSize -> IO CInt

foreign import ccall "hid_send_feature_report"
  hid_send_feature_report :: Device -> Ptr CChar -> CSize -> IO CInt

init :: IO ()
init = do
  r <- hid_init
  check (r == 0) "HIDAPI initialization failed" "hid_init /= 0"

exit :: IO ()
exit = do
  r <- hid_exit
  check (r == 0) "HIDAPI shutdown failed" "hid_exit /= 0"

withHIDAPI :: IO a -> IO a
withHIDAPI = bracket_ Hid.init Hid.exit

error :: Device -> IO (Maybe String)
error (Device devicePtr) = do
  e <- hid_error devicePtr
  if e == nullPtr
    then return Nothing
    else do
      es <- peekCWString e
      free e
      return (Just es)

check :: Bool -> String -> String -> IO ()
check c msg reason = unless c $ throwIO $ HIDAPIException msg reason

checkWithHidError :: Bool -> Device -> String -> String -> IO ()
checkWithHidError c dev@(Device devPtr) msg defaultReason = unless c $ do
  reason <- if devPtr /= nullPtr
    then fromMaybe defaultReason <$> Hid.error dev
    else return defaultReason
  throwIO $ HIDAPIException msg reason

open :: VendorID -> ProductID -> Maybe SerialNumber -> IO Device
open vendor_id product_id serial = do
  let vid = fromIntegral vendor_id
  let pid = fromIntegral product_id
  dev@(Device dp) <- Device <$> case serial of
    Nothing -> hid_open vid pid nullPtr
    Just sn -> withCWString sn (hid_open vid pid)
  checkWithHidError (dp /= nullPtr)
                    dev
                    "Device open (by vendor/product id) failed"
                    "hid_open returned NULL"
  return dev

close :: Device -> IO ()
close = hid_close

getSerialNumberString :: Device -> IO SerialNumber
getSerialNumberString dev = do
  let bs = _SERIAL_NUMBER_MAX_LENGTH * sizeOf (undefined :: CWchar)
  allocaBytes bs $ \b -> do
    n' <- hid_get_serial_number_string
      dev
      b
      (fromIntegral _SERIAL_NUMBER_MAX_LENGTH)
    checkWithHidError (n' /= -1)
                      dev
                      "Getting serial number failed"
                      "hid_get_serial_number_string returned -1"
    peekCWString b

sendFeatureReport :: Device -> BS.ByteString -> IO Int
sendFeatureReport dev d = do
  n' <- BS.useAsCStringLen d
    $ \(cs, csLen) -> hid_send_feature_report dev cs (fromIntegral csLen)
  checkWithHidError (n' /= -1)
                    dev
                    "Write failed"
                    "hid_send_feature_report returned -1"
  return $ fromIntegral n'

