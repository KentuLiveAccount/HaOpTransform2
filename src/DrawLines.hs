module DrawLines(
    drawLinesRect,
    linesToDraw,
    RenderMeasurements (RenderMeasurements, rmDxpLeftMargin, rmDypTopMargin, rmDxpChar, rmDypChar)
)
where

import Foreign (Int32)
import Graphics.Win32 (HDC, textOut)
import Utils (uncurry3, takeRange)

data RenderMeasurements = RenderMeasurements {
    rmDxpLeftMargin :: Int,
    rmDypTopMargin :: Int,
    rmDxpChar :: Int,
    rmDypChar :: Int
}

treatTab :: String -> String
treatTab string = concatMap (tabToSpaces) string
    where
        tabToSpaces '\n' = ""
        tabToSpaces '\t' = "    "
        tabToSpaces a    = [a]
        
getClipRange :: Int32 -> Int32 -> Int32 -> Int32 -> (Int, Int)
getClipRange offset increment clipStart clipEnd = (fromIntegral iStart, fromIntegral iEnd)
    where
        iStart = max 0 ((clipStart - offset) `div` increment)
        iEnd   = max 0 (1 + ((clipEnd - offset) `div` increment))

getClipRanges :: Int32 -> Int32 -> Int32 -> Int32 -> (Int32, Int32, Int32, Int32) -> (Int32, Int32, Int, Int, Int, Int)
getClipRanges xOff yOff xInc yInc (lClip, tClip, rClip, bClip) = (x, y, ichStart, ichEnd, ilnStart, ilnEnd)
    where
        (ichStart, ichEnd) = getClipRange xOff xInc lClip rClip
        (ilnStart, ilnEnd) = getClipRange yOff yInc tClip bClip
        x = (fromIntegral ichStart) * xInc + xOff
        y = (fromIntegral ilnStart) * yInc + yOff
    
linesToDraw :: [String] -> Int -> Int32 -> Int32 -> Int32 -> Int32 -> (Int32, Int32, Int32, Int32) -> [(Int32, Int32, String)]
linesToDraw lines iLine xOff yOff xInc yInc rect = zip3 (repeat x) ys lines'
    where
        (x, y, ichStart, ichEnd, ilnStart, ilnEnd) = getClipRanges xOff yOff xInc yInc rect
        lines' = map (treatLine) $ takeRange (ilnStart + iLine) (ilnEnd + iLine) (lines ++ (repeat ""))
        ys = map (\y' -> fromIntegral $ (y' * yInc) + y) [0..]
        treatLine  str = takeRange ichStart ichEnd $ (treatTab str) ++ repeat ' '

drawLinesRect :: HDC -> [String] -> Int -> RenderMeasurements -> (Int32, Int32, Int32, Int32) -> IO ()
drawLinesRect hdc lines iLine  rmm rect = 
    mapM_ (uncurry3 $ textOut hdc) $ linesToDraw lines iLine dxpLeftMargin dypTopMargin dxp dyp rect
    where
        dxpLeftMargin = fromIntegral $ rmDxpLeftMargin rmm
        dypTopMargin = fromIntegral $ rmDypTopMargin rmm
        dxp = fromIntegral $ rmDxpChar rmm
        dyp = fromIntegral $ rmDypChar rmm