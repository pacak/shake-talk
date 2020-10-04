module Main (main) where

import People
import Control.Monad

main :: IO ()
main = do
    forM_ names $ \person -> putStrLn $ "Hello " ++ person
