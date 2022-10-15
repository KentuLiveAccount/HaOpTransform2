module WindowProc
(makeWndProc)
where

import AppWindow (doFinish)
import Caret (CaretState (CaretState), hideCaret', showCaret', onSetFocusCaret, onKillFocusCaret)
import CreateFont (getTextExtent, withSelectSimpleFontDo)
import Data.Char (chr)
import Data.IORef (IORef, newIORef)
import DrawLines (RenderMeasurements (RenderMeasurements), drawLinesRect)
import Utils (minmax, onIORefM, onIORefM_, withPaintPaintStruct, unJust)
import Foreign (Int32, (.&.))
import Graphics.Win32
import KeyState (getKeyState)
import Selection (SelState (SelState), drawSelection)
import WindowScroll
import ViewModel (ViewModel (ViewModel, vmVC), ViewCache(vcLineData), initVM, ichXYFromCp, ichXYsFromCPs, linesFromVM)
import ViewModelTransform (onVM)
import ViewModelChange
import ViewState (vsCpActive, minCpVS, maxCpVS)

onIORefInvalidateM :: IORef a -> HWND -> (a -> IO a) -> IO ()
onIORefInvalidateM st hwnd m = do
    onIORefM st m
    invalidateRect (Just hwnd) Nothing False

onIORefApplyVMC :: IORef AppData -> HWND -> [ViewModelChange] -> IO ()
onIORefApplyVMC st hwnd trans =
    onIORefInvalidateM st hwnd $ \as -> do
        as' <- return (as {adVM = onVM (adVM as) trans})
        return as'

data AppSetting = AppSetting {
    asLineHeight :: Int,
    asDxpLeftMargin :: Int,
    asDypTopMargin :: Int,
    asDxpCaret :: Int,
    asFontName :: String,
    asFgColor :: COLORREF,
    asBgColor :: COLORREF
} deriving (Show)

appsetting :: AppSetting
appsetting = (AppSetting
    20 -- asLineHeight
    4  -- asDxpLeftMargin
    4  -- asDypTopMargin
    1 -- asDxpCaret
    "Consolas" -- asFontName
    (rgb 230 230 230) -- asFgColor
    (fromIntegral 0x303030)) -- asBgColor

data ScrollData = ScrollData {dxScroll::Int, dyScroll::Int} deriving (Show, Eq) -- (x, y, lines)

data AppData = AppData {
    adDxpChar :: Int,
    adDypChar :: Int,
    adVM :: ViewModel,
    adScroll :: ScrollData,
    adWindowSize :: (Int, Int)
} deriving (Show)

printLns :: SelState -> IO ()
printLns (SelState _ _ _ _ lns) = do
    print lns
    putStrLn ""

printSel :: ViewModel -> IO ()
printSel (ViewModel _ vs vc _)  = do
    print vs
    putStrLn ""

printLn a = print a >> putStrLn ""

printWP str wp = do
    putStrLn (str ++ (show wp))
    getKeyState vK_SHIFT >>= printLn
    getKeyState vK_CAPITAL >>= printLn

initAppData :: String -> Int -> Int -> AppData
initAppData str ich cch = AppData lh lh (initVM str ich cch) (ScrollData 0 0) (0, 0)
    where
        lh = (asLineHeight appsetting)

appData2CaretState :: AppData -> CaretState
appData2CaretState ad = CaretState dxpLeftM dypTopM dxpCh dypCh ichX (ichY - iYScroll) dxpCaret dypCh
    where
        dxpCaret = (asDxpCaret appsetting)
        dxpLeftM = (asDxpLeftMargin appsetting)
        dypTopM  = (asDypTopMargin appsetting)
        dxpCh    = (adDxpChar ad)
        dypCh    = (adDypChar ad)
        (ichX, ichY) = ichXYFromCp vc (vsCpActive vs)
        iYScroll = (dyScroll (adScroll ad)) `div` dypCh
        (ViewModel _ vs vc _) = (adVM ad)

appData2SelState :: AppData -> SelState
appData2SelState ad = SelState dxpL dypT dxpCh dypCh (map foo $ ichXYsFromCPs vc ichMin ichMax)
    where
        dxpL   = asDxpLeftMargin appsetting
        dypT   = asDypTopMargin appsetting
        dxpCh  = adDxpChar ad
        dypCh  = adDypChar ad
        ichMin = minCpVS vs
        ichMax = maxCpVS vs
        iYScroll = (dyScroll $ adScroll ad) `div` dypCh
        (ViewModel _ vs vc _) = adVM ad
        foo (ichY, ichXS, ichXE) = (ichY - iYScroll, cchInLn $ minmax ichMin ichMax ichXS, cchInLn $ minmax ichMin ichMax ichXE)
        cchInLn cp = fst $ ichXYFromCp vc cp

appData2ScrollInfo :: AppData -> ScrollInfo
appData2ScrollInfo ad = (ScrollInfo (dyScroll $ adScroll ad) 0 (nMax) dypChar (snd $ adWindowSize ad))
    where
        dypChar = adDypChar ad
        nMax = dypChar * (length $ vcLineData $ vmVC $ adVM ad) + (asDypTopMargin appsetting) + dypChar

makeRM :: Int -> Int -> RenderMeasurements
makeRM dxpCh dypCh = RenderMeasurements (asDxpLeftMargin appsetting) (asDypTopMargin appsetting) dxpCh dypCh

setDxyChAD :: AppData -> (Int32, Int32) -> AppData
setDxyChAD ad (dxpCh, dypCh) = ad {adDxpChar = (fromIntegral dxpCh), adDypChar = (fromIntegral dypCh)}

updateScrollInfo :: HWND -> AppData -> IO ()
updateScrollInfo hwnd ad = updateVScrollInfo hwnd (appData2ScrollInfo ad)

render :: (IORef AppData) -> HWND -> (HDC, Bool, RECT) -> IO ()
render refAD hwnd (hdc, _, rect) = withSelectSimpleFontDo (asFontName appsetting) (asLineHeight appsetting) hdc $ onIORefM refAD $ \ad -> do
    setBkMode hdc oPAQUE
    setBkColor hdc (asBgColor appsetting)
    setTextColor hdc (asFgColor appsetting)

    hideCaret' hwnd
    ad'@(AppData _dxpChar _dypChar vm' sd' sz) <- getTextExtent hdc "A" >>= return . setDxyChAD ad . unJust
    drawLinesRect hdc (linesFromVM vm') ((dyScroll sd') `div` _dypChar) (makeRM _dxpChar _dypChar) rect
    drawSelection hdc (appData2SelState ad')
--    printLns ((appData2SelState ad'))
    showCaret' hwnd (appData2CaretState ad')
    updateScrollInfo hwnd ad'
    return ad'

handleChar :: (IORef AppData) -> HWND -> WPARAM -> IO ()
handleChar st hwnd 0x8  = onIORefApplyVMC st hwnd [VMCBackspace]
handleChar st hwnd 0xd  = onIORefApplyVMC st hwnd [VMCReplace "\n"]
handleChar st hwnd 0x1a = onIORefApplyVMC st hwnd [VMCUndo]
handleChar st hwnd wp   = printWP "handlerChar :" wp >> onIORefApplyVMC st hwnd [VMCReplace [chr $ fromIntegral wp]]

shiftState :: IO Bool
shiftState = getKeyState vK_SHIFT >>= return . (\x -> (x .&. 0x8000) /= 0)

updateSizeInfo :: HWND -> WPARAM -> LPARAM -> AppData -> IO AppData
updateSizeInfo hwnd wp lp ad = do
    -- putStrLn $ unwords ["wM_SIZE wParam: ", show wp, "lParam: ", show lp]
    ad' <- return $ ad {adWindowSize = sizeFromLParam lp}
    -- putStrLn $ unwords ["adWindowSize: ", show $ adWindowSize ad']
    updateScrollInfo hwnd ad'
    return ad'
    where
        sizeFromLParam :: LPARAM -> (Int, Int)
        sizeFromLParam lp = (fromIntegral $ (lp .&. 0xffff), fromIntegral $ (lp .&. 0xffff0000) `div` 0x10000)

onVScroll :: HWND -> WPARAM -> LPARAM -> AppData -> IO AppData
onVScroll hwnd wp lp ad = do
    si' <- return $ applyVScroll wp (appData2ScrollInfo ad)
    ad' <- return $ ad {adScroll = (adScroll ad) {dyScroll = sizCur si'}}
    return ad'

handleKey :: (IORef AppData) -> HWND -> VKey -> Bool -> IO ()
handleKey st hwnd vk fShift
    | vk == vK_UP     = onIORefApplyVMC st hwnd [VMCMoveSelectionVert (-1) fShift]
    | vk == vK_DOWN   = onIORefApplyVMC st hwnd [VMCMoveSelectionVert 1 fShift]
    | vk == vK_LEFT   = onIORefApplyVMC st hwnd [VMCMoveSelection (-1) fShift]
    | vk == vK_RIGHT  = onIORefApplyVMC st hwnd [VMCMoveSelection 1 fShift]
    | vk == vK_HOME   = onIORefApplyVMC st hwnd [VMCMoveSelBeginLine fShift]
    | vk == vK_END    = onIORefApplyVMC st hwnd [VMCMoveSelEndLine fShift]
    | vk == vK_DELETE = onIORefApplyVMC st hwnd [VMCDelete]
    | vk == vK_PRIOR  = onIORefInvalidateM st hwnd (onVScroll hwnd sB_PAGEUP 0)
    | vk == vK_NEXT   = onIORefInvalidateM st hwnd (onVScroll hwnd sB_PAGEDOWN 0)
    | otherwise = printWP "handleKey :" vk >> return ()
    
wndProc :: (IORef AppData) -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc refAD hwnd wm wp lp
    | wm == wM_KEYDOWN     = shiftState >>= \shift -> handleKey refAD hwnd (fromIntegral wp) shift >> return 0
    | wm == wM_CHAR        = handleChar refAD hwnd wp >> return 0
    | wm == wM_PAINT       = withPaintPaintStruct hwnd (render refAD) >> return 0
    | wm == wM_SIZE        = onIORefM refAD (updateSizeInfo hwnd wp lp) >> return 0
    | wm == wM_VSCROLL     = onIORefInvalidateM refAD hwnd (onVScroll hwnd wp lp) >> return 0
    | wm == wM_LBUTTONDOWN = return 0
    | wm == wM_SETFOCUS    = onIORefM_ refAD (onSetFocusCaret hwnd . appData2CaretState) >> return 0
    | wm == wM_KILLFOCUS   = onKillFocusCaret hwnd            >> return 0
    | otherwise            = defWindowProc (Just hwnd) wm wp lp

makeWndProc :: IO (HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT)
makeWndProc = newIORef (initAppData "hello world\n\tand\nmore than 1 lines longer line\n\nkeged\nhello world\n\tand\nmore than 1 lines longer line\n\nkeged\n" 14 14) >>= 
    \x -> return (wndProc x)