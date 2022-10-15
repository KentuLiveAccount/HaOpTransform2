{-# LANGUAGE NoMonomorphismRestriction #-}

module Caret (
    showCaret',
    hideCaret',
    createCaret',
    CaretState (CaretState, csDxpLeftMargin, csDypLeftMargin, csDxpCh, csDypCh, csXCh, csYCh, csDxpCaret, csDypCaret),
    onKillFocusCaret,
    onSetFocusCaret
) where

import Graphics.Win32
import Graphics.Win32.Message
import Graphics.Win32.Window

foreign import ccall "winuser.h ShowCaret" _showCaret :: HWND -> IO Bool

data CaretState = CaretState {
    csDxpLeftMargin :: Int, 
    csDypLeftMargin :: Int,
    csDxpCh :: Int, 
    csDypCh :: Int, 
    csXCh :: Int, 
    csYCh :: Int,
    csDxpCaret :: Int,
    csDypCaret :: Int
    } deriving (Eq, Show)

csXPos :: CaretState -> LONG
csXPos cs = fromIntegral $ xBegin + (x * dxp)
    where
        xBegin = csDxpLeftMargin cs
        x      = csXCh cs
        dxp    = csDxpCh cs

csYPos :: CaretState -> LONG
csYPos cs = fromIntegral $ yBegin + (y * dyp)
    where
        yBegin = csDypLeftMargin cs
        y      = csYCh cs
        dyp    = csDypCh cs

showCaret' :: HWND -> CaretState -> IO ()
showCaret' hwnd cs = do
    c_SetCaretPos (csXPos cs) (csYPos cs) >> return ()
    _showCaret hwnd >>= \f -> if (f == False) then (getLastError >>= print) else (return ())

createCaret' :: HWND -> CaretState -> IO ()
createCaret' hwnd cs = createCaret hwnd nullPtr dxp dyp
    where
        dxp = Just $ fromIntegral (csDxpCaret cs)
        dyp = Just $ fromIntegral (csDypCaret cs)

onSetFocusCaret :: HWND -> CaretState -> IO ()
onSetFocusCaret hwnd cs = do
    createCaret' hwnd cs
    showCaret' hwnd cs

onKillFocusCaret :: HWND -> IO()
onKillFocusCaret hwnd = do
    hideCaret' hwnd
    destroyCaret

hideCaret' :: HWND -> IO()
hideCaret' hwnd = c_HideCaret hwnd >> return ()