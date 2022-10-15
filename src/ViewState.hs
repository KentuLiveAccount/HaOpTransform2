module ViewState (
    ViewState (..),
    notEmptyVS,
    minCpVS,
    maxCpVS,
    ccpVS,
    collapseToMinVS,
    makerangeIfEmpty,
    moveActiveVS,
    initVS,
    initCpCcpVS,
    setCpVS,
    zeroViewState
) where

data ViewState = ViewState {vsCpAnchor :: Int, vsCpActive :: Int} deriving (Eq, Show)

initVS :: Int -> Int -> ViewState
initVS cp1 cp2 = ViewState cp1 cp2

initCpCcpVS :: Int -> Int -> ViewState
initCpCcpVS cp ccp = ViewState cp (cp + ccp)

zeroViewState :: ViewState
zeroViewState = setCpVS 0

notEmptyVS :: ViewState -> Bool
notEmptyVS (ViewState cpAnch cpAct) = cpAnch /= cpAct

minCpVS :: ViewState -> Int
minCpVS  (ViewState cpAnch cpAct) = min cpAnch cpAct

maxCpVS :: ViewState -> Int
maxCpVS  (ViewState cpAnch cpAct) = max cpAnch cpAct

ccpVS :: ViewState -> Int
ccpVS (ViewState cpAnch cpAct) = abs (cpAnch - cpAct)

collapseToMinVS :: ViewState -> ViewState
collapseToMinVS vs = setCpVS $ minCpVS vs

moveActiveVS :: ViewState -> Int -> ViewState
moveActiveVS vs ccp = vs {vsCpActive = (vsCpActive vs) + ccp}

setCpVS :: Int -> ViewState
setCpVS cp = ViewState cp cp

makerangeIfEmpty :: Int -> ViewState -> (Int, Int)
makerangeIfEmpty cch vs
    | (notEmptyVS vs) = (minCpVS vs,  ccpVS vs)
    | otherwise       = (minCpVS vs', ccpVS vs')
    where
            vs' = moveActiveVS vs cch
