module Main where

import ViewModel
import ViewModelChange
import ViewModelTransform
import FunctionalTestRunner

testCases :: [TestCase ViewModel ViewModelChange ViewModel]
testCases = [
    TestCase "insertNothingIntoNothing" (initVM "" 0 0) [] (initVM "" 0 0),
    TestCase "adjustExtentOnNothing" (initVM "" 0 0) [VMCMoveSelection 1 True] (initVM "" 0 0),
    TestCase "insertTextIntoNothing" (initVM "" 0 0) [VMCReplace "hello"] (initVM "hello" 5 5),
    TestCase "adjustExtent" (initVM "hello" 0 0) [VMCMoveSelection 1 True] (initVM "hello" 0 1),
    TestCase "adjustExtentInvalid" (initVM "hello" 0 0) [VMCMoveSelection (-1) True] (initVM "hello" 0 0),
    TestCase "adjustExtentPastEnd" (initVM "hello" 0 0) [VMCMoveSelection 10 True] (initVM "hello" 0 5),
    TestCase "deleteTextInMiddle" (initVM "hello" 1 2) [VMCReplace ""] (initVM "hllo" 1 1),
    TestCase "backSpaceAtBeginning" (initVM "hello" 0 0) [VMCBackspace] (initVM "hello" 0 0),
    TestCase "backSpaceAtMiddle" (initVM "hello" 1 1) [VMCBackspace] (initVM "ello" 0 0),
    TestCase "backSpaceSel" (initVM "hello" 0 1) [VMCBackspace] (initVM "ello" 0 0),
    TestCase "deleteAtEnd" (initVM "hello" 5 5) [VMCDelete] (initVM "hello" 5 5),
    TestCase "deleteAtMiddle" (initVM "hello" 1 1) [VMCDelete] (initVM "hllo" 1 1),
    TestCase "deleteSel" (initVM "hello" 0 1) [VMCDelete] (initVM "ello" 0 0),
    TestCase "moveSelBeginLineOnNothing" (initVM "" 0 0) [VMCMoveSelBeginLine False] (initVM "" 0 0),
    TestCase "moveSelBeginLine" (initVM "hello\nworld" 9 10) [VMCMoveSelBeginLine False] (initVM "hello\nworld" 6 6),
    TestCase "moveSelEndLineOnNothing" (initVM "" 0 0) [VMCMoveSelEndLine False] (initVM "" 0 0),
    TestCase "moveSelEndLine" (initVM "hello\nworld" 3 4) [VMCMoveSelEndLine False] (initVM "hello\nworld" 5 5),
    TestCase "moveSelEndLine" (initVM "hello\nworld" 9 10) [VMCMoveSelEndLine False] (initVM "hello\nworld" 11 11),
    TestCase "moveSelectionVertOnNothing" (initVM "" 0 0) [VMCMoveSelectionVert 1 False] (initVM "" 0 0),
    TestCase "moveSelectionVertBackOnNothing" (initVM "" 0 0) [VMCMoveSelectionVert (-1) False] (initVM "" 0 0),
    TestCase "moveSelectionVertDownAtLastLine" (initVM "hello\nworld" 9 10) [VMCMoveSelectionVert 1 False] (initVM "hello\nworld" 9 10),
    TestCase "moveSelectionVertDown" (initVM "hello\nworld" 3 4) [VMCMoveSelectionVert 1 False] (initVM "hello\nworld" 9 9),
    TestCase "moveSelectionVertUpIntoEmptyLine" (initVM "\nworld" 3 4) [VMCMoveSelectionVert (-1) False] (initVM "\nworld" 0 0),
    TestCase "moveSelectionVertDownIntoEmptyLastLine" (initVM "world\n" 3 4) [VMCMoveSelectionVert 1 False] (initVM "world\n" 6 6),
    TestCase "moveSelectionVertDownExtend" (initVM  "hello world\n\tand\nmore than 1 lines longer line\n\nkeged\n" 14 19) [VMCMoveSelectionVert 1 True] (initVM "hello world\n\tand\nmore than 1 lines longer line\n\nkeged\n" 14 47)
    ]

main :: IO ()
main = mapM_ (test onVM) testCases