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
    let flags = (addKeys (DCC.savedGlobalFlags defaultConf))
                { DCG.globalCacheDir = DSS.Flag "/mnt/development/git/gentoo-haskell/.hackport"
                , DCG.globalLogsDir = DSS.NoFlag
                }
    control
      $ \runInIO -> DCG.withRepoContext verbosity (traceShowId flags)
      $ runInIO . (callback <=< restoreM)

-- | Unfortunately, 'DCC.commentSavedConfig' removes the keys and they need
-- | to be added back.
addKeys :: DCG.GlobalFlags -> DCG.GlobalFlags
addKeys flags =
    let repos = DCG.globalRemoteRepos flags
    in flags { DCG.globalRemoteRepos = DUN.overNubList (map addKeysRemote) repos }
  where
    addKeysRemote :: DCT.RemoteRepo -> DCT.RemoteRepo
    addKeysRemote r = r { DCT.remoteRepoRootKeys = defaultHackageRemoteRepoKeys }

defaultHackageRemoteRepoKeys :: [String]
defaultHackageRemoteRepoKeys =
    [ "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0",
      "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42",
      "2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3",
      "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d",
      "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
    ]

