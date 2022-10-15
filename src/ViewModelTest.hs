module Main where

import ViewModel

vm = initVM  "hello world\n\tand\nmore than 1 lines longer line\n\nkeged\n" 14 14

main :: IO ()
main = do
    print $ vcLineData $ vmVC vm
    putStrLn ""
    print $ ichXYsFromCPs (vmVC vm) 14 14 