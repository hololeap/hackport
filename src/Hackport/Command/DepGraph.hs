{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Hackport.Command.DepGraph where
--     ( depGraphAction
--     ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord (Down(..))
import Data.Semigroup (sconcat)
import qualified Data.Set as S
import System.FilePath (takeDirectory)
import Validation
import Witherable

import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
import Distribution.Client.IndexUtils (getSourcePackages)
import qualified Distribution.Client.SolverInstallPlan as SolverPlan
import Distribution.Compiler
import qualified Distribution.Package as Cabal
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageIndex
import Distribution.Types.InstalledPackageInfo
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.InstSolverPackage
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PkgConfigDb
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified Distribution.Verbosity as V

import Hackport.Env
import Hackport.Util (withHackportContext, getPortageDir)
import Overlays (getOverlayPath)
import Portage.EMeta
import qualified Portage.GHCCore as GHCCore
import Portage.Overlay (Overlay(..), ExistingEbuild(..), loadLazy)
import qualified Portage.PackageId as Portage
import Portage.Resolve (resolveCategories)
import Util (die, info)

-- import Debug.Pretty.Simple

-- | Default list of "leaf" packages, provided for ease of use
--
--   See: <https://github.com/gentoo-haskell/gentoo-haskell/wiki/Leaf-packages-in-main-gentoo-repo>
defaultPkgs :: NE.NonEmpty Portage.PackageName
defaultPkgs = fmap (uncurry Portage.mkPackageName) $
    ("app-admin", "haskell-updater") NE.:|
        [ ("app-portage", "hackport")
        , ("app-text", "pandoc-cli")
        , ("dev-haskell", "cabal-install")
        , ("dev-util", "shellcheck")
        , ("dev-vcs", "darcs")
        , ("dev-vcs", "git-annex")
        , ("dev-vcs", "git-repair")
        , ("x11-misc", "xmobar")
        , ("x11-wm", "xmonad")
        , ("x11-wm", "xmonad-contrib")
        , ("net-mail", "list-remote-forwards")
        , ("net-mail", "mailbox-count")
        , ("net-misc", "haeredes")
        , ("net-misc", "hath")
--         , ("app-arch", "pack") -- only depends on ghc
--         , ("app-emacs", "haskell-mode") -- only depends on ghc
--         , ("dev-lang", "whitespace") -- requires 'haskell98' ???
        , ("dev-util", "shelltestrunner")
        ]

data SourceRepo = HaskellRepo | GentooRepo
    deriving (Show, Eq)

type RepoMap = M.Map Portage.PackageName [ExistingEbuild]
type GHCVer = [Int]

type SortedEbuild = (SourceRepo, ExistingEbuild, EMeta)
type SortedGHC = (GHCVer, CompilerInfo, InstalledPackageIndex)

depGraphAction :: Env DepGraphEnv ()
depGraphAction = ask >>= \(GlobalEnv verbosity _ _, depGraphEnv) ->
    withHackportContext $ \_ repoContext -> do

        haskellPath <- getOverlayPath
        gentooPath <- getPortageDir
        haskellOverlay@(Overlay _ haskellEbuilds _) <- loadLazy haskellPath
        Overlay _ gentooEbuilds _ <- loadLazy gentooPath

        ghcsWithCompatEbuilds <-
            validateInput haskellEbuilds gentooEbuilds depGraphEnv

        -- For each GHC given on the command line, this will create a set of
        -- portage package ids that are needed to build all the target packages
        -- on that specific GHC version
        plans <- forM ghcsWithCompatEbuilds $ \(sortedGHC, ebuilds) -> do
            let (ghcVer, cInfo, iIndex) = sortedGHC

            -- Build up the various parts needed by resolveDependencies
            solver <- liftIO $ chooseSolver verbosity AlwaysModular cInfo
            srcPkgDb <- liftIO $ getSourcePackages V.silent repoContext
            let pcDb = pkgConfigDbFromList []
                namedPkgs = getNamedPackage <$> ebuilds
                pkgPrefs = do
                    (pkgName, verRange) <- getPkgNameVer <$> NE.toList ebuilds
                    [ PackageVersionPreference pkgName verRange
                        , PackageStanzasPreference pkgName [TestStanzas]
                        ]
                constraints = toLPC . fst <$> instPkgNameVers iIndex
                params
                      -- Don't try to resolve newer versions of core libs
                    = setPreferenceDefault PreferAllInstalled
                      -- Use exact versions of target packages
                    $ addPreferences pkgPrefs
                      -- Set scopeToplevel constraint for the InstalledPackageIndex
                    $ addConstraints constraints
                    $ setAvoidReinstalls (AvoidReinstalls True)
                    $ basicInstallPolicy iIndex srcPkgDb (NE.toList namedPkgs)

            -- Run resolveDependencies
            plan <- foldProgress
                (\s -> (info s *>))
                die
                pure
                (resolveDependencies GHCCore.platform cInfo pcDb solver params)

            -- For all the nodes of the generated solver plan, get ebuild data
            -- using findBestEbuild.
            -- TODO: This requires increasing the ulimit for maximum number of
            --       open files, probably because of unsafeInterleaveIO in
            --       loadLazy.
            let planPkgIds = fromSPP `map` SolverPlan.toList plan
            vEbuilds <- forM planPkgIds $ \pkgId -> do
                let pkgName = Cabal.pkgName pkgId
                case NE.nonEmpty (resolveCategories haskellOverlay pkgName) of
                     Nothing -> pure $ Failure $ NE.singleton $
                        (ghcVer, Left pkgName)
                     Just cats -> do
                        let portIds = flip Portage.fromCabalPackageId pkgId
                                <$> cats
                            pPkgNames = Portage.packageId <$> portIds

                        mEbuilds <- forM pPkgNames $ \pPkgName -> liftIO $ do
                            mEbuild <- findBestEbuild
                                haskellEbuilds
                                gentooEbuilds
                                ghcVer
                                pPkgName
                            pure $ case mEbuild of
                                Nothing -> Failure $ NE.singleton $
                                    (ghcVer, Right pPkgName)
                                Just (_,e,_) -> Success [e]
                        pure $ sconcat mEbuilds

            -- Dump info about which packages have no ebuilds
            forM_ (NE.nonEmpty (failures vEbuilds))
                $ info . missingEbuilds . sconcat

            pure $ ebuildId `S.map`
                        S.fromList (concat (successes vEbuilds))

        -- dump all the package ids from all the plans as one final list
        liftIO $ mapM_ (putStrLn . ("=" ++))
            $ S.map prettyShow $ S.unions $ NE.toList plans

  where
    getNamedPackage :: SortedEbuild -> PackageSpecifier pkg
    getNamedPackage s = NamedPackage (getPkgName s) []

    getPkgNameVer :: SortedEbuild -> (Cabal.PackageName, Cabal.VersionRange)
    getPkgNameVer s@(_,e,_) =
        let pkgName = getPkgName s
            portVer = Portage.pkgVersion (ebuildId e)
            cabalVer = Cabal.mkVersion (Portage.versionNumber portVer)
        in (pkgName, Cabal.thisVersion cabalVer)

    toLPC :: Cabal.PackageName -> LabeledPackageConstraint
    toLPC pkgName
        = LabeledPackageConstraint
            (PackageConstraint (scopeToplevel pkgName) PackagePropertyInstalled)
            ConstraintSourceUserTarget

    instPkgNameVers
        :: InstalledPackageIndex
        -> [(Cabal.PackageName, Cabal.VersionRange)]
    instPkgNameVers iIndex = do
        pkgInfo <- allPackages iIndex
        let pkgId = sourcePackageId pkgInfo

        pure (Cabal.pkgName pkgId, Cabal.thisVersion (Cabal.pkgVersion pkgId))

    defPkgName :: SortedEbuild -> Cabal.PackageName
    defPkgName (_,e,_) = Portage.cabalPkgName $ Portage.packageId $ ebuildId e

    getPkgName :: SortedEbuild -> Cabal.PackageName
    getPkgName s@(_,_,em) = maybe (defPkgName s) Cabal.mkPackageName (cabalPN em)

    fromSPP :: SolverPlan.SolverPlanPackage -> Cabal.PackageId
    fromSPP = \case
        SolverPlan.PreExisting isp ->
            sourcePackageId (instSolverPkgIPI isp)
        SolverPlan.Configured solverPkg ->
            srcpkgPackageId (solverPkgSource solverPkg)

--     fromSPPNoInstalled :: SolverPlan.SolverPlanPackage -> Maybe Cabal.PackageId
--     fromSPPNoInstalled = \case
--         SolverPlan.PreExisting _ -> Nothing
--         SolverPlan.Configured solverPkg -> Just $
--             srcpkgPackageId (solverPkgSource solverPkg)


-- | Filters out ebuilds that are @-9999@ live versions, or that require
--   too high a version of GHC. Immediately chooses the first ebuild that has
--   the given GHC version listed under
--
--   @@@
--   #hackport: preferred-ghcs: ...
--   @@@
--
--   or
--
--   @@@
--   CABAL_CORE_LIB_GHC_PV="..."
--   @@@
findBestEbuild
    :: RepoMap -- ^ haskell repo map
    -> RepoMap -- ^ gentoo repo map
    -> GHCVer -- ^ GHC target version
    -> Portage.PackageName -- ^ package target
    -> IO (Maybe SortedEbuild)
findBestEbuild haskellMap gentooMap ghcVer pkgName = runMaybeT $ do

        let haskellEbuilds = map (HaskellRepo,) <$> M.lookup pkgName haskellMap
            gentooEbuilds = map (GentooRepo,) <$> M.lookup pkgName gentooMap

        ebuilds <- liftMaybe $ haskellEbuilds <|> gentooEbuilds

        -- Use ExceptT as a way to exit immediately if we find a preferred ebuild
        eCompatEbuilds <- runExceptT $ forMaybe ebuilds $ \(repo, e) ->
            -- Use MaybeT here to allow for 'guard' as a way to filter
            -- ebuilds. We need to use a new level of MaybeT to avoid
            -- aborting 'findBestEbuild' completely.
            runMaybeT $ do

                -- Filter out live -9999 versions
                let ver = Portage.versionNumber
                        $ Portage.pkgVersion
                        $ ebuildId e
                guard $ ver /= [9999]

                let ePath = ebuildPath e
                    finish = do -- The final action
                        m <- liftIO (findExistingMeta (takeDirectory ePath))
                        pure (repo, e, m)

                -- Jump immediately out (as a success) if this is a preferred
                -- version for the current GHC being analyzed
                prefGHCs <- findPreferredGHCs ePath
                when (maybe False (elem ghcVer) prefGHCs)
                    (finish >>= throwError) -- not actually an error!

                -- No minimum ghc found; assume compatible??
                findMinimumGHC ePath >>= guard . maybe True (ghcVer >=)
                finish

        case eCompatEbuilds of
            -- A preferred ebuild was found
            Left pref -> do
                pure pref
            -- A (possibly empty) list of compatible ebuilds
            Right compatEbuilds -> do

                -- Sort the results in reverse order by package id and take the
                -- first element (maximum compatible ebuild)
                liftMaybe
                    $ NE.head
                    . NE.sortWith (\(_,e,_) -> Down (ebuildId e))
                        <$> NE.nonEmpty compatEbuilds

validateInput
    :: RepoMap
    -> RepoMap
    -> DepGraphEnv
    -> Env env (NonEmpty (SortedGHC, NonEmpty SortedEbuild))
validateInput haskellMap gentooMap (DepGraphEnv ghcs pkgs) = do
    let validatedGHCs = sconcat $ NE.map sortGHC ghcs

    flip (validation (die . invalidGHCs)) validatedGHCs $ \goodGHCs -> do
        forM goodGHCs $ \sortedGHC@(ghcVer, _, _) -> do
            validatedEbuilds <- liftIO $ getAp $ sconcat
                $ sortEbuild ghcVer <$> fromMaybe defaultPkgs pkgs
            flip (validation (die . missingEbuilds)) validatedEbuilds $
                pure . (sortedGHC,)

  where
    sortGHC
        :: GHCVer
        -> Validation (NonEmpty GHCVer) (NonEmpty SortedGHC)
    sortGHC ghc = bimapNonEmpty $
        -- Use WithUpgradeable package index
        (\g@(cInfo,_) -> (ghc, cInfo, GHCCore.withUpgradeable g))
            <$> maybeToSuccess ghc (M.lookup ghc GHCCore.ghcs)

    sortEbuild
        :: GHCVer
        -> Portage.PackageName
        -> Ap IO (Validation
                    (NonEmpty (GHCVer, Either Cabal.PackageName Portage.PackageName))
                    (NonEmpty SortedEbuild) )
    sortEbuild ghcVer pkg = Ap $ do
        best <- findBestEbuild haskellMap gentooMap ghcVer pkg
        pure $ bimapNonEmpty $ first (ghcVer,)
            $ maybeToSuccess (Right pkg) best

    invalidGHCs :: NonEmpty GHCVer -> String
    invalidGHCs bad = unwords
        $ "The following GHC versions are invalid:"
        : NE.toList (showGHCVer <$> bad)

    bimapNonEmpty :: Bifunctor f => f a b -> f (NE.NonEmpty a) (NE.NonEmpty b)
    bimapNonEmpty = bimap NE.singleton NE.singleton

missingEbuilds :: NonEmpty (GHCVer, (Either Cabal.PackageName Portage.PackageName)) -> String
missingEbuilds bad = unlines
    $ "Could not find ebuilds for the following:"
    : map (("    * " ++) . showIncompatible) (NE.toList bad)
  where
    showIncompatible :: (GHCVer, Either Cabal.PackageName Portage.PackageName) -> String
    showIncompatible = \case
        (_, Left p) ->
            prettyShow p
            ++ ": No ebuilds available"
        (i, Right p) ->
            prettyShow p
            ++ ": No versions compatible with ghc-"
            ++ showGHCVer i

showGHCVer :: GHCVer -> String
showGHCVer = L.intercalate "." . map show

liftMaybe :: Applicative f => Maybe a -> MaybeT f a
liftMaybe = MaybeT . pure
