module ViewModelTransform
(
    onVM
) where

import ViewModel
import ViewModelChange
import ViewState
import DataModelChange
import DataModelTransform (onDM)
import Utils (if', minmax)

onVS :: ViewState -> ViewModelChange -> ViewState
onVS vs vmc = case vmc of
    VMCMoveSelection ccp False -> setCpVS $ (minCpVS vs) + ccp
    VMCMoveSelection ccp True -> moveActiveVS vs ccp
    VMCReplace str -> setCpVS $ (minCpVS vs) + length str
    VMCDelete -> collapseToMinVS vs
    VMCBackspace -> if' (notEmptyVS vs) 
        (collapseToMinVS vs)
        (onVS vs (VMCMoveSelection (-1) False))

onVM' :: ViewModel -> ViewModelChange -> ViewModel -- publically, view model can be manipulated via vmChanges

onVM' vm@(ViewModel dm vs _ prevs) VMCUndo
    | prevs == [] = vm
    | otherwise = initViewModel dm' vs' (tail prevs)
    where  
        (dm', vs') = head prevs

onVM' vm@(ViewModel dm vs _ prevs) vmc = initViewModel dm' vs' ((dm, vs):prevs)
    where
        (vmc', dmcs) = toDMChange vm vmc
        dm' = onDM dm dmcs
        vs' = onVS vs vmc'

onVM :: ViewModel -> [ViewModelChange] -> ViewModel
onVM vm vmcs = foldl (onVM') vm vmcs

toDMChange :: ViewModel -> ViewModelChange -> (ViewModelChange, [DataModelChange])
toDMChange vm@(ViewModel dm vs vc _) vmc = case (normalizeVMC vm vmc) of
    VMCReplace str -> (vmc, [DMCReplace (minCpVS vs) (ccpVS vs) str])
    VMCDelete      -> (VMCDelete,    [(\(a, b) -> DMCReplace a b "") $ makerangeIfEmpty 1 vs])
    VMCBackspace   -> (VMCBackspace, [(\(a, b) -> DMCReplace a b "") $ makerangeIfEmpty (-1) vs])
    vmc' -> (vmc', [])

vMCNoOp = VMCMoveSelection 0 True

normalizeVMC :: ViewModel -> ViewModelChange -> ViewModelChange
normalizeVMC vm@(ViewModel dm vs vc _) (VMCReplace str)       = VMCReplace str
normalizeVMC vm@(ViewModel dm vs vc _) (VMCMoveSelBeginLine f)  = VMCMoveSelection ccp f
    where
        ccp = if' (emptyVC vc) 0 $ cpLinStart - cpCur
        cpLinStart = ldIchLn $ findLn (vcLineData vc) cpCur
        cpCur = minCpVS vs

normalizeVMC vm@(ViewModel dm vs vc _) (VMCMoveSelEndLine f)    = VMCMoveSelection ccp f
    where
        ccp = if' (emptyVC vc) 0 $ cpLinEnd - cpCur
        cpLinEnd = ldCpLineEnd ld
        ld = findLn (vcLineData vc) cpCur
        cpCur = minCpVS vs

normalizeVMC vm@(ViewModel dm vs vc _) (VMCDelete)            = if' (not deletable) vMCNoOp VMCDelete
    where
        deletable = ((ccpVS vs) > 0) || ((minCpVS vs) < (cpMaxVC vc))

normalizeVMC vm@(ViewModel dm vs vc _) (VMCBackspace)         =  if' (not backable) vMCNoOp VMCBackspace
    where
        backable = ((ccpVS vs) > 0) || (0 < minCpVS vs)

normalizeVMC vm@(ViewModel dm vs vc _) (VMCMoveSelection ccp False) = VMCMoveSelection ccp' False
    where
        ccp' = (normalizeCp vc $ (minCpVS vs) + ccp) - (minCpVS vs)

normalizeVMC vm@(ViewModel dm vs vc _) (VMCMoveSelection ccp True)  = VMCMoveSelection ccp' True
    where
        ccp' = if' (emptyVC vc) 0
            $ max 0 (normalizeCp vc $ (vsCpActive vs) + ccp) - (vsCpActive vs)

normalizeVMC vm@(ViewModel dm vs vc _) (VMCMoveSelectionVert cLn f) = 
        if' (cLn' == 0) vMCNoOp (VMCMoveSelection (cpNew - cpCur) f)
    where
        cpNew = minmax cpLineStartNew cpLineEndNew $ cpLineStartNew + cchWithinLine
        cpLineStartNew = ldIchLn ldNew
        cpLineEndNew   = ldCpLineEnd ldNew 
        cchWithinLine = cpCur - (ldIchLn ldCur)
        ldNew = ldAtIln (vcLineData vc) (iLn + cLn')
        cLn' = if' (emptyVC vc) 0
            $ (minmax 0 (iLnMaxVC vc) (iLn + cLn)) - iLn 
        iLn = ldILn ldCur
        ldCur = findLn (vcLineData vc) cpCur
        cpCur = if' f (vsCpActive vs) (minCpVS vs)