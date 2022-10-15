module DataModelTransform
(
    onDM
) where

import DataModel (DataModel(DataModel))
import DataModelChange (DataModelChange(DMCReplace))

-- data DataModel = DataModel {dmText :: String} deriving (Eq, Show)
-- data DataModelChange = DMCReplace {dmcIch :: Int, dmcCch :: Int, dmcStr :: String} deriving (Eq, Show) 

onDM' :: DataModel -> DataModelChange -> DataModel
onDM' (DataModel txt) (DMCReplace ich cch str) = DataModel str'
    where
        (tm, tt) = splitAt (ich + cch) txt
        (tb, _) = splitAt (ich) tm
        str' = tb ++ str ++ tt

onDM :: DataModel -> [DataModelChange] -> DataModel -- publically, dataModel can be manipulated via dmChanges
onDM dm dmc = foldl onDM' dm dmc
    