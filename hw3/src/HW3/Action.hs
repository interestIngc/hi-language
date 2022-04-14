module HW3.Action
  (
    HiPermission (..)
  , HIO (..)
  , PermissionException (..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Set as DS
import qualified Data.Text as T

import Control.Exception (throwIO)
import Control.Exception.Base (Exception)
import Control.Monad
import Control.Monad.Trans
import Data.Sequence (fromList)
import Data.Set (Set)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock
import HW3.Base
import System.Directory
import System.Random

-- | Represents access permission.
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Represents permission exception.
data PermissionException =
  PermissionRequired HiPermission
  deriving (Eq, Ord, Show)

instance Exception PermissionException

-- | Controls whether the program has the corresponding permission
-- to execute an action.
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Monad HIO where
  return x = liftIO (return x)

  (HIO g) >>= f = HIO $
    \set -> do
      value <- g set
      runHIO (f value) set

instance Applicative HIO where
  pure = return

  (<*>) = ap

instance Functor HIO where
  fmap = liftM

instance MonadIO HIO where
  liftIO ioa = HIO (const ioa)

-- | Checks whether the corresponding permission is present.
checkPermissions :: Maybe HiPermission -> HIO ()
checkPermissions Nothing = return ()
checkPermissions (Just permission) = HIO $
  \set ->
    if DS.member permission set
    then return ()
    else throwIO (PermissionRequired permission)

-- | Applies an action.
applyAction :: HiAction -> HIO HiValue
applyAction (HiActionRead path) = do
  exists <- liftIO $ doesFileExist path
  case exists of
    True -> do
      bytestring <- liftIO $ BS.readFile path
      case (decodeUtf8' bytestring) of
          (Left _)     -> return (HiValueBytes bytestring)
          (Right text) -> return (HiValueString text)
    False ->
      liftIO
        (HiValueList
        . (fromList . (map (HiValueString . T.pack)))
          <$> (listDirectory path))
applyAction HiActionCwd =
  liftIO  ((HiValueString . T.pack)
  <$> getCurrentDirectory)
applyAction (HiActionMkDir path) =
  liftIO (HiValueNull <$ (createDirectory path))
applyAction (HiActionChDir path) =
  liftIO (HiValueNull <$ (setCurrentDirectory path))
applyAction (HiActionWrite filepath content) =
  liftIO (HiValueNull <$ (BS.writeFile filepath content))
applyAction HiActionNow = liftIO (HiValueTime <$> getCurrentTime)
applyAction (HiActionRand start end) =
    liftIO $
      (HiValueNumber . toRational)
      <$> (randomRIO (start, end) :: IO Int)
applyAction (HiActionEcho text) =
  liftIO (HiValueNull <$ (putStrLn (T.unpack text)))

-- | Maps action to the corresponding permission.
-- | Returns @Just @HiPermission if action requires permission.
-- | Returns Nothing if it does not.
getPermission :: HiAction -> Maybe HiPermission
getPermission action =
  case action of
    (HiActionRead _)    -> return AllowRead
    HiActionCwd         -> return AllowRead
    (HiActionChDir _)   -> return AllowRead
    (HiActionMkDir _)   -> return AllowWrite
    (HiActionWrite _ _) -> return AllowWrite
    HiActionNow         -> return AllowTime
    (HiActionRand _ _)  -> Nothing
    (HiActionEcho _)    -> return AllowWrite

instance HiMonad HIO where
  runAction action = do
    checkPermissions (getPermission action)
    applyAction action

