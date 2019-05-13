catAndMouse :: [Int] -> String
catAndMouse [x, y, z]
    | dx < dy = "Cat A"
    | dx > dy = "Cat B"
    | otherwise = "Mouse C"
  where
    dx = abs $ z - x
    dy = abs $ z - y

main :: IO ()
main =
    interact $
    unlines . map (catAndMouse . (map read . words)) . tail . lines
