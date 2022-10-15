module ViewModel 
(
    LineDatum (LineDatum, ldILn, ldIchLn, ldCchLn, ldStrLn, ldHasEop),
    LineData,
    ViewCache (ViewCache, vcCaret, vcLineData),
    ViewModel (ViewModel, vmDM, vmVS, vmVC, vmPrev),
    emptyVC,
    findLn,
    cpMaxVC,
    ichXYFromCp,
    ichXYsFromCPs,
    iLnFromCp,
    iLnMaxVC,
    initViewModel,
    initVM,
    ldAtIln,
    ldCpLineEnd,
    ldIchLnEnd,
    linesFromVM,
    normalizeCp,
    zeroViewState
)
where

import DataModel (DataModel, dmText, initDM)
import Utils (linesOn, if', intersects, minmax)
import ViewState

data LineDatum = LineDatum {
    ldILn :: Int, 
    ldIchLn :: Int, 
    ldCchLn :: Int, 
    ldStrLn :: String, 
    ldHasEop :: Bool
    } deriving (Show, Eq)

type LineData = [LineDatum] -- [(iLine, ichLine, strLine)]

initLineData :: String -> LineData
initLineData "" = [LineDatum 0 0 0 "" False]
initLineData str = fixupLastPara $ map (ichStr) $ zip [0..] $ linesOn (snd) $ zip [0..] str
    where
        ichStr :: (Int, [(Int, Char)]) -> LineDatum
        ichStr (iLn, as@((ichLn, _):_)) = LineDatum iLn ichLn (length as) (map (snd) as) (hasEop as)
        hasEop [] = False
        hasEop str = (snd $ last str) == '\n'
        fixupLastPara :: LineData -> LineData
        fixupLastPara [a] = a : (if' (ldHasEop a) [LineDatum (1 + (ldILn a)) ((ldIchLn a) + (ldCchLn a)) 0 "" False] [])
        fixupLastPara (a:as) = a : (fixupLastPara as)

findLn :: LineData -> Int -> LineDatum
findLn lds cp = last $ filter (\ldm -> (ldIchLn ldm) <= cp) lds

ichRangeFromLD :: LineDatum -> (Int, Int, Int)
ichRangeFromLD (LineDatum iLn ichLn cchLn _ fEop) = (iLn, ichLn, ichLn + cchLn - (if' fEop 1 0))

ldAtIln :: LineData -> Int -> LineDatum
ldAtIln lds iLn = head $ filter (ldAt iLn) lds
    where
        ldAt :: Int -> LineDatum -> Bool
        ldAt iLn (LineDatum ldiln _ _ _ _) = iLn == ldiln

ldCpLineEnd :: LineDatum -> Int
ldCpLineEnd ld = dcpEop + (ldIchLnEnd ld)
        where
            dcpEop = if' (ldHasEop ld) (-1) 0 

ldIchLnEnd :: LineDatum -> Int
ldIchLnEnd (LineDatum _ ichLn cchLn _ _) = ichLn + cchLn


data ViewCache = ViewCache {
    vcCaret :: (Int, Int), 
    vcLineData :: LineData
    } deriving (Eq, Show)

initViewCache :: DataModel -> ViewState -> ViewCache
initViewCache dm vs = ViewCache (0, 0) (initLineData $ dmText dm)

ichXYFromCp :: ViewCache -> Int -> (Int, Int)
ichXYFromCp vc cp = (cchSub, iLn)
    where
        (LineDatum iLn ichLn _ strLn _) = findLn (vcLineData vc) cp
        cchSub = sum $ map (ch2cch) (take (cp - ichLn) strLn)
        ch2cch '\t' = 4
        ch2cch _ = 1

ichXYsFromCPs :: ViewCache -> Int -> Int -> [(Int, Int, Int)]
ichXYsFromCPs vc cpMin cpMax = filter (filter') $ map (ichRangeFromLD) (vcLineData vc)
    where
        filter' (_, ichB, ichE) = intersects (ichB, ichE) (cpMin, cpMax)

cpMaxVC :: ViewCache -> Int
cpMaxVC vc = if' (emptyVC vc) 0  $ (ldIchLn ldLast) + (ldCchLn ldLast)
    where
        ldLast = last (vcLineData vc)
        
iLnMaxVC :: ViewCache -> Int
iLnMaxVC vc = if' (emptyVC vc) 0 $ ldILn $ last $ (vcLineData vc)

normalizeCp :: ViewCache -> Int -> Int
normalizeCp vc cp = minmax  0 (cpMaxVC vc) cp

iLnFromCp :: ViewCache -> Int -> Int
iLnFromCp vc cp = ldILn $ findLn (vcLineData vc) cp

emptyVC :: ViewCache -> Bool
emptyVC vc = (vcLineData vc) == [LineDatum 0 0 0 "" False]


data ViewModel = ViewModel {vmDM::DataModel, vmVS::ViewState, vmVC::ViewCache, vmPrev :: [(DataModel, ViewState)]} deriving (Eq, Show)

initViewModel :: DataModel -> ViewState -> [(DataModel, ViewState)] -> ViewModel
initViewModel dm vs prevs = ViewModel dm vs (initViewCache dm vs) prevs

initVM :: String -> Int -> Int -> ViewModel
initVM str cpSelBegin cpSelEnd = initViewModel (initDM str) (initVS cpSelBegin cpSelEnd) []

linesFromVM :: ViewModel -> [String]
linesFromVM = map ldStrLn . vcLineData . vmVC

