solve :: [Float] -> Int
solve [start, end] =
    length $
        (dropWhile (< start) . takeWhile (<= end))
        [a * a | a <- [1 ..]]

main :: IO ()
main = interact $ unlines . map (show . solve . map read . words) . tail . lines
