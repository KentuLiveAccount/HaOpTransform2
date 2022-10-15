module Main where

import AppWindow (createSimpleWindowDo)
import Graphics.Win32
import Utils (doMsgPump)
import WindowProc (makeWndProc)

main = do
    curArrow <- loadCursor Nothing iDC_ARROW
    proc <- makeWndProc
    createSimpleWindowDo "Text Editor" (fromIntegral 0x303030) curArrow proc
        (\hwnd -> do
            showWindow hwnd sW_SHOWNORMAL
            updateWindow hwnd
            doMsgPump)

            