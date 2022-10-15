module Main where

{-
    Analyze the difference between having functions and having values representing particular functions

    fn :: A -> B -> A

    applyer :: A -> (FID B) -> A
-}

import DataModel
import DataModelChange
import DataModelTransform
import ViewModel
import ViewModelChange
import ViewModelTransform

--DataModel -> other states -> ViewModel -- internally, viewModel contains dataModel as it's parts

getDM :: ViewModel -> DataModel -- internally, dataModel can be extracted from viewModel as it is contained within as a part
getDM vm = emptyDM

vmBefore :: ViewModel -> ViewModel -- internally, vm holds onto it's previous state for undo
vmBefore vm = vm

main = putStrLn "Hello"
