module KeyState (
    getKeyState
) where

import System.Win32.Types (DWORD, WORD)
import Graphics.Win32.Key (VKey)

foreign import ccall "winuser.h GetKeyState" _getKeyState :: DWORD -> IO WORD

getKeyState :: VKey -> IO WORD
getKeyState vk = _getKeyState vk
