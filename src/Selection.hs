module Selection (
    drawSelection,
    SelState (..)
) where

import Control.Exception (bracket)
import Foreign (Int32)
import Graphics.Win32
import Graphics.Win32.GDI.Graphics2D
import Graphics.Win32.GDI.Pen

foreign import ccall "windows.h SetROP2" c_setROP2 :: HDC -> Int32 -> IO Int32

data SelState = SelState {
    dxLeft :: Int,
    dyTop  :: Int, 
    dxChar :: Int,
    dyChar :: Int,
    selLns ::  [(Int, Int, Int)] -- iln, ichStart, ichEnd
} deriving (Eq, Show)

rectFromDiag :: Int -> Int -> Int -> Int -> [(Int, Int)]
rectFromDiag x1 y1 x2 y2 = [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]

rectsFromSelState :: SelState -> [(Int, Int, Int, Int)]
rectsFromSelState (SelState dxLE dyTO dx dy lns) = map foo lns
    where
        foo :: (Int, Int, Int) -> (Int, Int, Int, Int)
        foo (iln, ichS, ichE) = (ichS * dx + dxLE, iln * dy + dyTO, ichE * dx + dxLE, iln * dy + dyTO + dy)

drawSelection :: HDC -> SelState -> IO ()
drawSelection hdc sel = withRop2 hdc rOP2_MERGEPEN $  
    withPen hdc pS_SOLID 1 selColor $ 
    withSolidBrush hdc selColor $ do
        setPolyFillMode hdc aLTERNATE
        mapM_ ((\(w, x, y, z) -> rectangle hdc w x y z) . convert) $ rectsFromSelState sel
    where
        convert :: (Int, Int, Int, Int) -> (Int32, Int32, Int32, Int32)
        convert (w, x, y, z) = (fromIntegral w, fromIntegral x, fromIntegral y, fromIntegral z)

rOP2_MERGEPEN :: Int32
rOP2_MERGEPEN = 15

withRop2 :: HDC -> Int32 -> IO a -> IO a
withRop2 hdc rop io = bracket (c_setROP2 hdc rop) (c_setROP2 hdc) $ \_ -> io

withCreatePen :: PenStyle -> INT -> COLORREF -> (HPEN -> IO a) -> IO a
withCreatePen ps width cl fn = bracket (createPen ps width cl) (deletePen) fn

withSelectPen :: HDC -> IO a -> HPEN -> IO a
withSelectPen hdc io pen = bracket (selectPen hdc pen) (selectPen hdc) $ \_ -> io

withPen :: HDC -> PenStyle -> INT -> COLORREF -> IO a -> IO a
withPen hdc ps width cl io = withCreatePen ps width cl $ withSelectPen hdc io

withCreateSolidBrush :: COLORREF -> (HBRUSH -> IO a) -> IO a
withCreateSolidBrush clr fn = bracket (createSolidBrush clr) (deleteBrush) fn

withSelectBrush :: HDC -> IO a -> HBRUSH -> IO a
withSelectBrush hdc io brush = bracket (selectBrush hdc brush) (selectBrush hdc) $ \_ -> io

withSolidBrush :: HDC -> COLORREF -> IO a -> IO a
withSolidBrush hdc clr io = withCreateSolidBrush clr $ withSelectBrush hdc io

selColor = (rgb 38 70 120)
