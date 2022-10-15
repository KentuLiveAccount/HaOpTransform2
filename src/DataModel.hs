module DataModel 
    (
        DataModel (DataModel, dmText),
        emptyDM,
        initDM
    )
    where

data DataModel = DataModel {dmText :: String} deriving (Eq, Show)

emptyDM :: DataModel
emptyDM = DataModel ""

initDM :: String -> DataModel
initDM str = DataModel str