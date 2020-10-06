#!/usr/bin/env stack
-- stack --resolver lts-16.16 script

-- will be used for oracles
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- convenience things
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE TupleSections #-}

-- it's a good idea to have this enabled
-- {-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Char
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Distribution.Package as Cabal
import qualified Distribution.Text as Cabal
import GHC.Generics
import qualified GHC.Paths
import System.Console.GetOpt
import System.Directory
import qualified System.Environment as IO
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Read (readMaybe)
import Text.Regex (matchRegex, mkRegex)

-- | The version and location of GHC that is running this script. It is also the version
-- of GHC we are going to build programs with.
ghcPath :: String
ghcPath = GHC.Paths.ghc

-- | Get the version of GHC being used.
ghcVersion :: String
ghcVersion = case matchVersion ghcPath of
        Just (version:_) -> version
        _ -> error $ "Cannot extract a GHC version from " ++ ghcPath
    where
        matchVersion =
            matchRegex (mkRegex "ghc-([0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?)")

-- | A directory to which all build artifacts go.
makeDir :: FilePath
makeDir = "_mk" </> ghcVersion


-- * EXTRA COMMAND LINE ARGUMENTS
data BuildOptions = BuildOptions
    { optimization :: Int
    , prof :: Bool
    , unsafeGhcOptions :: [String] -- do not use directly in rules, only via ghcOptions
    , cosmeticGhcOptions :: [String] -- should not affect compilation results
    } deriving (Eq, Show)

-- {{{
defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions
    { optimization = 2
    , prof = False
    , unsafeGhcOptions =
        [ "-msse4.2"
        , "-fforce-recomp" -- in some cases ghc avoids recompilation even if depencencies say otherwise
        , "-XBangPatterns"
        , "-XDeriveFunctor"
        , "-XDeriveGeneric"
        , "-XDeriveLift"
        , "-XDeriveTraversable"
        , "-XFlexibleContexts"
        , "-XFlexibleInstances"
        , "-XGeneralizedNewtypeDeriving"
        , "-XLambdaCase"
        , "-XMultiParamTypeClasses"
        , "-XViewPatterns"
        ]
    , cosmeticGhcOptions = []
    }

-- there are two types of ghc options - those that affect compilation results and those that
-- don't. For safe (cosmetic) options we don't need to recompile anything previously compiled
-- with different their combination
addGhcOption :: BuildOptions -> String -> BuildOptions
addGhcOption cfg opt
    | safeOption opt = cfg { cosmeticGhcOptions = cosmeticGhcOptions cfg ++ [opt] }
    | otherwise = cfg { unsafeGhcOptions = unsafeGhcOptions cfg ++ [opt] }
    where
        safeOption :: String -> Bool
        safeOption = flip Set.member (Set.fromList
            [ "-freverse-errors"
            , "-fdiagnostics-color=always"
            ])

optDescrs :: [OptDescr (Either String (BuildOptions -> BuildOptions))]
optDescrs =
    [ Option "O" []
        (ReqArg (\n -> pure $ \o -> o { optimization = read n }) "N")
        "optimization level to pass to GHC via -O"

    , Option "" ["prof"]
        (NoArg $ Right (\o -> o{ prof = True }))
        "enable profiling"

    , Option "" ["ghc-options"]
        (ReqArg (\str -> pure $ \o -> foldl' addGhcOption o (words str)) "OPTS")
        "options to pass to ghc"

    , Option "e" ["Werror"] (NoArg $ Right (\o -> o { cosmeticGhcOptions = cosmeticGhcOptions o ++ ["-Werror"] }))
        "add -Werror to ghc options"
    ]


getShakeVersion :: IO String
getShakeVersion = getHashedShakeVersion ["Build.hs"]

-- }}}

main :: IO ()
main = do
-- {{{
    -- disable environment file
    IO.setEnv "GHC_ENVIRONMENT" "-"
    version <- getShakeVersion

    istty <- queryTerminal stdOutput
    let shakeOpts = shakeOptions
            { shakeVersion = version
            , shakeThreads = 0
            , shakeFiles = makeDir
            , shakeChange = ChangeModtimeAndDigest
            , shakeColor = istty
            }
-- }}}
    shakeArgsAccumulate shakeOpts optDescrs defaultBuildOptions $
        \opts targets -> do
            pure . Just $ do
                colorize <- shakeColor <$> getShakeOptionsRules

                let opts' = if colorize
                    then opts { cosmeticGhcOptions = "-fdiagnostics-color=always"
                              : cosmeticGhcOptions opts }
                    else opts
                want targets
                knownExecutablesRules opts'
                executablesRules
                dependencyRules


executables :: Map String String
executables = Map.fromList [("greeter", "Greeter")]

-- {{{

data BuildType
    = BuildType
    { buildOptimization :: !Int
    , buildProfiling :: !Bool
    } deriving (Eq, Show)

fastestBuild :: BuildType
fastestBuild = BuildType { buildOptimization = 0, buildProfiling = False }

releaseBuild :: BuildType
releaseBuild = BuildType { buildOptimization = 2, buildProfiling = False }

getBuildType :: BuildOptions -> BuildType
getBuildType buildOptions = BuildType
    { buildOptimization = optimization buildOptions
    , buildProfiling = prof buildOptions
    }

buildTypeToString :: BuildType -> String
buildTypeToString BuildType{..} = 'O' : show buildOptimization


-- | Take a path into some build dir, and decompose it.
--
-- Since ghc needs non-profiling code to run TH in compilation with profiling enabled and it's
-- looking for said code in the same folder as profiling code - this parser assumes that
-- profiling state is stored as part of file name suffix. As such only a subset of filetypes is
-- supported
parseBuildPath
    :: FilePath
    -> (BuildType, FilePath{- leftover path from build dir-})
parseBuildPath path = fromMaybe (error $ "Not a valid path: " ++ path) $ do
    (_prefix : ('O':opt) : rest) <- splitDirectories <$> stripPrefix makeDir path
    buildOptimization <- readMaybe opt
    let buildProfiling = isProfiling path
    pure (BuildType{..}, joinPath rest)

isProfiling :: FilePath -> Bool
isProfiling fp = case takeExtension fp of
    ".hi" -> False
    ".o" -> False
    ".exe" -> False
    ".p_hi" -> True
    ".p_o" -> True
    ".p_exe" -> True
    unk -> error $ "unsupported file type: " ++ show fp ++ ", extension was " ++ show unk

ghcBuildOptions :: BuildType -> [String]
ghcBuildOptions bt@BuildType{..} = concat
    [ ["-j1"]
    , ["-O" ++ show buildOptimization]
    , if not buildProfiling then [] else
        words "-prof -fprof-auto-exported -osuf p_o -hisuf p_hi"
    , do opt <- [ "-odir", "-hidir", "-i", "-outputdir" ]
         pure $ opt ++ makeDir </> buildTypeToString bt </> "src"
    ]


-- }}}


-- used internally by module dependencies oracle
newtype DependencyMapQ = DependencyMapQ () deriving (Show, Eq, Hashable, Binary, NFData)
type instance RuleResult DependencyMapQ = DependencyMap

-- provide easy access and force recompilation when any of the module dependencies changes
newtype ModuleDeps = ModuleDeps Module deriving (Show, Eq, Hashable, Binary, NFData)
type instance RuleResult ModuleDeps = [Dependency]

type Module = String
type Package = String
newtype DependencyMap = DependencyMap { unDependencyMap :: HashMap.HashMap Module [Dependency] }
    deriving (Show, Eq, Hashable, NFData)
instance Binary DependencyMap where
    put = put . HashMap.toList . unDependencyMap
    get = DependencyMap . HashMap.fromList <$> get

data Dependency
    = LocalModuleDep !Module
    | SystemModuleDep !Package !Module
    deriving (Eq, Ord, Show, Read, Generic)
instance Binary Dependency
instance Hashable Dependency
instance NFData Dependency

stripDirectory :: FilePath -> FilePath -> Maybe FilePath
stripDirectory = stripPrefix . addTrailingPathSeparator

dependencyRules :: Rules ()
dependencyRules = do
    void $ addOracleCache $ \DependencyMapQ{} -> do

        sources <- getDirectoryFiles "" ["src//*.hs"]
        need sources

        liftIO $ createDirectoryIfMissing True "_mk/tmp"
        -- collect dependencies between modules known to ghc
        let tmp = "_mk/tmp/makefile"
        unit $ cmd "ghc -M -isrc -include-pkg-deps -dep-suffix=" [""]
            "-dep-makefile" tmp sources (Traced "ghc -M")
        DependencyMap . calcDependencies <$> liftIO (readFile tmp)

    void $ addOracle $ \(ModuleDeps modName) -> do
        DependencyMap m <- askOracle $ DependencyMapQ ()
        pure $ HashMap.lookupDefault [] modName m


-- | Calculate dependencies for modules by parsing a Makefile generated by GHC.
calcDependencies :: String -> HashMap Module [Dependency]
calcDependencies = shallowDependencyMap . parseMakefile

-- | Turn a list of dependency edges to a shallow DependencyMap.
shallowDependencyMap :: [(FilePath, [FilePath])] -> HashMap Module [Dependency]
shallowDependencyMap list = HashMap.fromListWith (++) $ do
    (modPath, depPaths) <- list
    let err = error $ "shallowDependencyMap: parse error: " ++ modPath
        modName = fromMaybe err (parseModPath modPath)
    return (modName, mapMaybe parseDepPath depPaths)
  where
    parseDepPath :: FilePath -> Maybe Dependency
    parseDepPath path = depKind path <$> parseModPath path

    depKind :: FilePath -> Module -> Dependency
    depKind path
        | Just _ <- stripDirectory "src" path = LocalModuleDep
        | Just package <- sysPathToPackage path = SystemModuleDep package
        | otherwise = error $ "depKind: don't know how to handle " ++ show path

    parseModPath path = do
        guard $ takeExtension path `elem` [".hi", ".o"]
        return $ pathToModule path

sysPathToPackage :: FilePath -> Maybe Package
sysPathToPackage hiPath = do
        fullname <- listToMaybe . dropWhile goodModuleName . reverse
                . splitDirectories $ dropExtension hiPath
        pure $ stripVersion . stripId $ fullname
    where
        stripId xs = case dropWhile isAlphaNum (reverse xs) of
            ('-':rest) -> reverse rest
            _ -> xs

pathToModule :: FilePath -> Module
pathToModule fp
    = checkEmpty $ replaceChar '/' '.' $ dropPrefix $ dropExtension fp
  where
    checkEmpty "" = error $ "not a module name: " ++ fp
    checkEmpty s = s
    dropPrefix
      = joinPath . reverse
      . takeWhile goodModuleName
      . reverse . splitDirectories

goodModuleName :: FilePath -> Bool
goodModuleName (x:xs) = isUpper x && all isAlphaNum xs
goodModuleName _ = False

replaceChar :: Char -> Char -> String -> String
replaceChar from to = map $ \c -> if c == from then to else c

-- | Drop the version part from a package name.
--
-- >>> stripVersion "foo"
-- "foo"
-- >>> stripVersion "foo-1.2.3"
-- "foo"
stripVersion :: String -> String
stripVersion str = maybe str (Cabal.unPackageName . Cabal.pkgName)
    $ Cabal.simpleParse str


knownExecutablesRules :: BuildOptions -> Rules ()
knownExecutablesRules opts = phonys $ \name -> do
    -- check if this executable is known to compiler
    _mainModule <- Map.lookup name executables

    pure $ do
        let buildType = getBuildType opts
            ext = if prof opts then "p_exe" else "exe"
        let exec = makeDir </> buildTypeToString buildType </> "execs" </> name <.> ext
        need [exec]
        liftIO $ createDirectoryIfMissing True "bin"
        cmd "ln" ["--symbolic", "--relative", "--force", "--no-target-directory"]
            exec ("bin" </> name)

executablesRules :: Rules ()
executablesRules = do
    makeDir </> "*" {- opt level -} </> "execs" </> "*"%> \f -> do
        let (buildType, path) = parseBuildPath f
        let Just mainModule = Map.lookup (takeBaseName path) executables

        allDeps <- askOracle (ModuleDeps mainModule)
        liftIO $ print allDeps

        cmd "ghc"
            [ "-o", f ]
            (ghcBuildOptions buildType)

