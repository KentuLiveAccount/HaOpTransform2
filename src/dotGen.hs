module Main where

import Utils (onStdInDoAsLines_, partitions, printAsDot)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

keywd = ".hs" ++ ":import"

-- Utils.hs:import Control.Monad (when)
sanitize :: String -> Maybe (String, String)
sanitize str = case (filter (isPrefixOf keywd . snd) $ partitions str) of
    []         -> Nothing
    ((a, b):_) -> Just (a, head $ tail $ words b)

main = onStdInDoAsLines_ (printAsDot . catMaybes . map sanitize)