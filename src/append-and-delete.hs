solve :: String -> String
solve input
    | diff == 0 = "Yes"
    | diff > 0 && (diff > (2 * start) || diff `mod` 2 == 0) = "Yes"
    | otherwise = "No"
  where
    [s, t, k] = lines input
    start = length $ takeWhile (uncurry (==)) $ zip s t
    diff = read k - length (drop start s) - length (drop start t)

main :: IO ()
main = interact solve
