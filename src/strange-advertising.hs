solve :: Int -> Int
solve n = sum $ take n $ iterate (\x -> (x * 3) `div` 2) 2

main :: IO ()
main = interact $ show . solve . read
