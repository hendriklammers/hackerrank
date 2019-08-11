fact :: Double -> Double
fact n
    | n < 1 = 1
    | otherwise = n * fact (n - 1)

solve :: Double -> Double
solve x = sum [x ** y / fact y | y <- [0 .. 9]]

main :: IO ()
main = interact $ unlines . map (show . solve . read) . tail . lines
