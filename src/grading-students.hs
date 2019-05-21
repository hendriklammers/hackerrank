roundGrade :: Int -> Int
roundGrade x
    | x < 38 = x
    | diff > 2 = x + (5 - diff)
    | otherwise = x
  where
    diff = x `mod` 5

main :: IO ()
main = interact $ unlines . map (show . roundGrade) . tail . map read . words
