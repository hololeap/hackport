module Hackport.Util
  ( getPortageDir
  , withHackportContext
  ) where

import Control.Monad.Trans.Control
import Data.Maybe (fromJust)
import qualified Network.URI as NU
import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )

import Distribution.Simple.Utils (warn)
import qualified Distribution.Simple.Setup as DSS
import qualified Distribution.Client.Config as DCC
import qualified Distribution.Client.GlobalFlags as DCG
import qualified Distribution.Client.Types as DCT
import qualified Distribution.Utils.NubList as DUN

import Portage.Host as Host ( getInfo, portage_dir )
import Overlays (getOverlayPath)

import Hackport.Env

import Debug.Trace

getPortageDir :: Env env FilePath
getPortageDir = do
  (GlobalEnv verbosity _ portagePathM, _) <- ask
  portagePath <- case portagePathM of
                   Nothing -> liftIO $ Host.portage_dir <$> Host.getInfo
                   Just path -> return path
  exists <- liftIO $ doesDirectoryExist $ portagePath </> "dev-haskell"
  unless exists $ liftIO $
    warn verbosity $ "Looks like an invalid portage directory: " ++ portagePath
  return portagePath

withHackportContext :: (DCG.RepoContext -> Env env a) -> Env env a
withHackportContext callback = do
    (GlobalEnv verbosity _ _, _) <- ask
    overlayPath <- getOverlayPath
    defaultConf <- liftIO DCC.commentSavedConfig
    let flags = DCC.savedGlobalFlags defaultConf
    control
      $ \runInIO -> DCG.withRepoContext verbosity (traceShowId flags)
      $ runInIO . (callback <=< restoreM)
