module CreateFont (
  withSimpleFontDo,
  withSelectSimpleFontDo,
  getCharWidthFloat,
  getTextExtent
  ) where

import Control.Exception (bracket)
import Foreign (Ptr, peekByteOff, allocaBytes)
import Graphics.Win32
import Graphics.Win32.GDI.Font
import Graphics.Win32.GDI.Types
import Graphics.Win32.GDI.HDC
import Utils (if')

foreign import ccall "windows.h GetCharWidthFloatW" c_getCharWidthFloat :: HDC -> UINT -> UINT -> Ptr FLOAT -> IO UINT
foreign import ccall "windows.h GetTextExtentPoint32W" c_getTextExtentPoint32W :: HDC -> LPCTSTR -> Int -> Ptr SIZE -> IO BOOL

sizeofFLOAT = 4

createFontSimple :: String -> Int -> IO HFONT
createFontSimple strFontFace height =
    createFont (fromIntegral height) 0 {-width-}
      0 {-escapement-} 0 {- oriententation -}
      fW_NORMAL
      False --ital
      False --under
      False --strike
      aNSI_CHARSET
      oUT_DEFAULT_PRECIS
      cLIP_DEFAULT_PRECIS
      dEFAULT_QUALITY
      fF_DONTCARE
      strFontFace

withSimpleFontDo :: String -> Int -> (HFONT -> IO a) -> IO a
withSimpleFontDo strFace height fn = bracket
      (createFontSimple strFace height) deleteFont fn

withSelectFontDo :: HDC -> HFONT -> IO a -> IO a
withSelectFontDo hdc hfont fn = bracket
      (selectFont hdc hfont)
      (selectFont hdc)
      (\_ -> fn)

withSelectSimpleFontDo :: String -> Int -> HDC -> IO a -> IO a
withSelectSimpleFontDo strFace height hdc fn =
      withSimpleFontDo strFace height $ \hfont ->
        withSelectFontDo hdc hfont fn

getCharWidthFloat :: HDC -> UINT -> UINT -> IO FLOAT
getCharWidthFloat hdc iFirstCh iLastCh =
      allocaBytes (sizeofFLOAT * (fromIntegral $ 1 + iLastCh - iFirstCh)) $ \ptrFloat -> do
            c_getCharWidthFloat hdc iFirstCh iLastCh ptrFloat
            peekByteOff ptrFloat sizeofFLOAT

getTextExtent :: HDC -> String -> IO (Maybe SIZE)
getTextExtent hdc str = withTString str $ \tstr ->
      allocaSIZE $ \psz -> do
            f <- c_getTextExtentPoint32W hdc tstr (fromIntegral $ length str) psz
            if' (f) (peekSIZE psz >>= return . Just) (return Nothing)