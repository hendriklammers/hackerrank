jump [] = 0
jump [_] = 1
jump (x:y:xs)
    | y == 0 = 1 + jump xs
    | otherwise = 1 + jump (y : xs)

main :: IO ()
main = interact $ show . jump . tail . map read . tail . words
