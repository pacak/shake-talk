#!/usr/bin/env stack
-- stack --resolver lts-16.16 script

-- will be used for oracles
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- convenience things
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- it's a good idea to have this enabled
-- {-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util
import qualified GHC.Paths
import System.Console.GetOpt
import System.Directory
import qualified System.Environment as IO
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Read
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
         pure $ opt ++ makeDir </> buildTypeToString bt
    ]


-- }}}

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

        -- don't do this
        unit $ cmd "ghc" "-c"
            (ghcBuildOptions buildType)
            ("src" </> "People.hs")

        -- don't do this
        unit $ cmd "ghc" "-c"
            (ghcBuildOptions buildType)
            ("src" </> "Greeter.hs")

        cmd "ghc"
            [ "-o", f ]
            (ghcBuildOptions buildType)
            ["_mk/8.8.4/O2/Main.o", "_mk/8.8.4/O2/People.o"]

