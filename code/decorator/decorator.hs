type Image = String

contrastFilter :: Double -> Image -> Image
contrastFilter factor img = img ++ " + Contrast factor " ++ show(factor)

monochromeFilter :: Image -> Image
monochromeFilter img = img ++ " + Monochrome conversion"

edgeDetectionFilter :: Image -> Image
edgeDetectionFilter img = img ++ " + Edge detection"

combineFilters :: [(Image -> Image)] -> (Image -> Image)
combineFilters fs = foldl (flip (.)) id fs

main :: IO()
main = do
  -- Static version
  putStrLn $ (edgeDetectionFilter . (contrastFilter 2.5) . monochromeFilter) "Mosaic image"
  
  -- dynamic version
  putStrLn $ filter "Mosaic Image"
  where
    filter = combineFilters [monochromeFilter, (contrastFilter 2.5), edgeDetectionFilter]