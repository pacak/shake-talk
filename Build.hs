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

import Data.Foldable
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util
import qualified GHC.Paths
import System.Console.GetOpt
import qualified System.Environment as IO
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
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

main :: IO ()
main = do
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

    shakeArgsAccumulate shakeOpts optDescrs defaultBuildOptions $
        \opts targets -> do
            putStrLn $ "I'm about to compile " ++ show targets ++ " using " ++ show opts
            pure . Just $ do
                colorize <- shakeColor <$> getShakeOptionsRules

                let opts' = if colorize
                    then opts { cosmeticGhcOptions = "-fdiagnostics-color=always"
                              : cosmeticGhcOptions opts }
                    else opts
                want targets
