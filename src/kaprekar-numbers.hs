isKaprekar :: Int -> Bool
isKaprekar x = x == x'
  where
    s = show x
    sq = show $ x * x
    d = length sq - length s
    x' = read ('0' : take d sq) + read (drop d sq)

solve :: [Int] -> String
solve [p, q]
    | length ns < 1 = "INVALID RANGE"
    | otherwise = unwords $ map show ns
  where
    ns = filter isKaprekar [p .. q]

main :: IO ()
main = interact $ solve . map read . lines
