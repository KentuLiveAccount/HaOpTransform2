module DataModelChange
(
    DataModelChange (DMCReplace, dmcIch, dmcCch, dmcStr)
) where

data DataModelChange = DMCReplace {dmcIch :: Int, dmcCch :: Int, dmcStr :: String} deriving (Eq, Show) 