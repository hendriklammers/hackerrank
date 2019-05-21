solve :: String -> Int
solve = snd . foldl step (0, 0)
  where
    step (a, n) 'U' = ( a + 1 , if a == (-1) then n + 1 else n)
    step (a, n) 'D' = (a - 1, n)

main :: IO ()
main = interact $ show . solve . head . tail . words
