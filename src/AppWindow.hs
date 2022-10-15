module AppWindow 
(
    createSimpleWindowDo,
    doFinish
)
where

import Utils (createClassWindowDo)
import Graphics.Win32
import Graphics.Win32.Window
import System.Win32.DLL (getModuleHandle)

foreign import ccall "winuser.h PostQuitMessage" postQuitMessage :: Int -> IO ()

createSimpleWindowDo :: String -> COLORREF -> HCURSOR -> WindowClosure -> (HWND -> IO a) -> IO a
createSimpleWindowDo strTitle clrBg hcur wndProc fn = do
    hinst      <- getModuleHandle Nothing
    whiteBrush <- createSolidBrush clrBg

    createClassWindowDo
        (cS_DBLCLKS,
            hinst,      -- HINSTANCE
            Nothing,    -- Maybe HICON
            Just hcur,  -- Maybe HCURSOR
            Just whiteBrush,-- Maybe HBRUSH
            Nothing,    -- Maybe LPCTSTR
            mkClassName "My Window Class")

        strTitle
        (wS_THICKFRAME + wS_CAPTION + wS_SYSMENU + wS_VSCROLL + wS_MAXIMIZE + wS_MINIMIZE + wS_MAXIMIZEBOX + wS_MINIMIZEBOX)
        Nothing         -- Maybe Pos :: x
        Nothing         -- Maybe Pos :: y
        Nothing         -- Maybe Pos :: dx
        Nothing         -- Maybe Pos :: dy
        Nothing         -- Maybe HWND
        Nothing         -- Maybe HMENU
        (_wndProc wndProc) -- WindowClosure

        fn

doFinish :: HWND -> IO LRESULT
doFinish hwnd = sendMessage hwnd wM_CLOSE 1 0 >> return 0

_wndProc :: (HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT) -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
_wndProc f hwnd wm wp lp
    | wm == wM_DESTROY     = postQuitMessage 0 >> return 0
    | otherwise            = f hwnd wm wp lp
    

