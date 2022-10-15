module ViewModelChange
(
    ViewModelChange(
        VMCMoveSelection, vmcCcp, vmcExtend,
        VMCReplace, vmcStr,
        VMCMoveSelectionVert, vmcCLn, vmcvExtend,
        VMCDelete,
        VMCBackspace,
        VMCMoveSelBeginLine, vmcBLExtend,
        VMCMoveSelEndLine, vmcELExtend,
        VMCUndo
    )
) where


data ViewModelChange =
    VMCMoveSelection {vmcCcp :: Int, vmcExtend :: Bool} | 
    VMCReplace {vmcStr :: String} | 
    VMCMoveSelectionVert {vmcCLn :: Int, vmcvExtend :: Bool} |
    VMCDelete |
    VMCBackspace |
    VMCUndo |
    VMCMoveSelBeginLine {vmcBLExtend :: Bool} |
    VMCMoveSelEndLine {vmcELExtend :: Bool}  deriving (Eq, Show) 
