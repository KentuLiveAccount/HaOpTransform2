module WindowScroll
(
    SCROLLINFO,
    ScrollInfo (ScrollInfo, sizCur, sizMin, sizMax, sidzLine, sidzPage),
    sB_BOTH, sB_CTL, sB_HORZ, sB_VERT,
    sB_LINEUP, sB_LINEDOWN, sB_PAGEUP, sB_PAGEDOWN, sB_TOP, sB_BOTTOM,
    sB_LINELEFT, sB_LINERIGHT, sB_PAGELEFT, sB_PAGERIGHT, sB_LEFT,sB_RIGHT,
    sB_THUMBPOSITION, sB_THUMBTRACK, sB_ENDSCROLL,
    getScrollInfo,
    setScrollInfo,
    decodeVScroll,
    updateVScrollInfo,
    applyVScroll
)
where

import Foreign (Int32, Ptr, castPtr, nullPtr, peekByteOff, pokeByteOff, allocaBytes, (.&.))
import Graphics.Win32 (HWND, RECT, INT, WPARAM)
import System.Win32.Types(UINT, BOOL)
import Utils (if', unJust)

data ScrollInfo = ScrollInfo {
    sizCur :: Int,
    sizMin :: Int,
    sizMax :: Int,
    sidzLine :: Int,
    sidzPage :: Int
}

type SCROLLINFO = (UINT, UINT, Int32, Int32, UINT, Int32, Int32)

-- scroll bar
sB_HORZ = 0 :: Int32
sB_VERT = 1 :: Int32
sB_CTL = 2 :: Int32
sB_BOTH = 3 :: Int32

-- SCROLLINFO flag
sIF_DISABLENOSCROLL = 8 :: UINT
sIF_PAGE            = 2 :: UINT
sIF_POS             = 4 :: UINT
sIF_RANGE           = 1 :: UINT
sIF_TRACKPOS        = 16 :: UINT
sIF_ALL             = 31 :: UINT

sizeofSCROLLINFO = 28

-- wM_VSCROLL wParam values
sB_LINEUP           = 0 :: UINT
sB_LINELEFT         = 0 :: UINT
sB_LINEDOWN         = 1 :: UINT
sB_LINERIGHT        = 1 :: UINT
sB_PAGEUP           = 2 :: UINT
sB_PAGELEFT         = 2 :: UINT
sB_PAGEDOWN         = 3 :: UINT
sB_PAGERIGHT        = 3 :: UINT
sB_THUMBPOSITION    = 4 :: UINT
sB_THUMBTRACK       = 5 :: UINT
sB_TOP              = 6 :: UINT
sB_LEFT             = 6 :: UINT
sB_BOTTOM           = 7 :: UINT
sB_RIGHT            = 7 :: UINT
sB_ENDSCROLL        = 8 :: UINT

updateVScrollInfo :: HWND -> ScrollInfo -> IO ()
updateVScrollInfo hwnd (ScrollInfo siCur siMin siMax siLine siPage) = do
    (cbSize, fMask, nMin, nMax, nPage, nPos, nTrackPos) <- getScrollInfo hwnd sB_VERT >>= return . unJust
    setScrollInfo hwnd sB_VERT 
        (cbSize, fMask, fromIntegral siMin, fromIntegral siMax, fromIntegral siPage, fromIntegral siCur, nTrackPos) True
    return ()

applyVScroll :: WPARAM -> ScrollInfo -> ScrollInfo
applyVScroll wp si@(ScrollInfo yCur yMin yMax dyLine dyPage)
    | sb == sB_LINEUP        = si {sizCur = max yMin (yCur - dyLine)}
    | sb == sB_LINEDOWN      = si {sizCur = min (yMax - dyPage) (yCur + dyLine)}
    | sb == sB_PAGEUP        = si {sizCur = max yMin (yCur - dyPage + dyLine)}
    | sb == sB_PAGEDOWN      = si {sizCur = min (yMax - dyPage) (yCur + dyPage - dyLine)}
    | sb == sB_TOP           = si {sizCur = yMin}
    | sb == sB_BOTTOM        = si {sizCur = yMax - dyPage}
    | sb == sB_THUMBPOSITION = si {sizCur = fromIntegral thumbos}
    | sb == sB_THUMBTRACK    = si {sizCur = fromIntegral thumbos}
    | sb == sB_ENDSCROLL     = si
    where
        (sb, thumbos) = decodeVScroll wp

allocaScrollInfo :: (Ptr SCROLLINFO -> IO a) -> IO a
allocaScrollInfo = allocaBytes sizeofSCROLLINFO

peekScrollInfo :: Ptr SCROLLINFO -> IO SCROLLINFO
peekScrollInfo p = do
    cbSize <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
    fMask  <- (\hsc_ptr -> peekByteOff hsc_ptr 4) p
    nMin   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
    nMax   <- (\hsc_ptr -> peekByteOff hsc_ptr 12) p
    nPage  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) p
    nPos   <- (\hsc_ptr -> peekByteOff hsc_ptr 20) p
    nTrackPos <- (\hsc_ptr -> peekByteOff hsc_ptr 24) p
    return (cbSize, fMask, nMin, nMax, nPage, nPos, nTrackPos)

foreign import ccall "ScrollWindow" scrollWindow :: HWND -> Int32 -> Int32 -> Ptr RECT -> Ptr RECT -> IO INT
foreign import ccall "SetScrollPos" setScrollPos :: HWND -> Int32 -> Int32 -> BOOL -> IO Int32
foreign import ccall "SetScrollRange" setScrollRange :: HWND -> Int32 -> Int32 -> Int32 -> BOOL -> IO BOOL
foreign import ccall "GetScrollInfo" c_getScrollInfo :: HWND -> Int32 -> Ptr SCROLLINFO -> IO BOOL
foreign import ccall "SetScrollInfo" c_setScrollInfo :: HWND -> Int32 -> Ptr SCROLLINFO -> BOOL -> IO INT


getScrollInfo :: HWND -> Int32 -> IO (Maybe SCROLLINFO)
getScrollInfo hwnd nBar =
    allocaScrollInfo (\psc -> do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) psc sizeofSCROLLINFO
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) psc sIF_ALL
        fRet <- c_getScrollInfo hwnd nBar psc
        if' (fRet) (peekScrollInfo psc >>= return . Just) (return Nothing)
    )

decodeVScroll :: WPARAM -> (UINT, UINT)
decodeVScroll wp = (sb, thumbPos)
    where
        sb = fromIntegral (wp .&. 0xffff)
        thumbPos = if' (sb == sB_THUMBPOSITION || sb == sB_THUMBTRACK) (fromIntegral $ wp .&. 0xffff0000 `div` 0x10000) 0

setScrollInfo :: HWND -> Int32 -> SCROLLINFO -> BOOL -> IO (INT)
setScrollInfo hwnd nbar (_, _, nMin, nMax, nPage, nPos, nTrackPos) bRepaint =
    allocaScrollInfo (\psc -> do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) psc sizeofSCROLLINFO
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) psc sIF_ALL
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) psc nMin
        (\hsc_ptr -> pokeByteOff hsc_ptr 12) psc nMax
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) psc nPage
        (\hsc_ptr -> pokeByteOff hsc_ptr 20) psc nPos
        (\hsc_ptr -> pokeByteOff hsc_ptr 24) psc nTrackPos
        c_setScrollInfo hwnd nbar psc bRepaint
    )