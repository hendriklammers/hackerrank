main :: IO ()
main = interact $ unlines . (\n -> stairs n n) . head . map read . words
  where
    stairs _ 0 = []
    stairs s n =
        concat (replicate (s - (s - n + 1)) " " ++ replicate (s - (n - 1)) "#") :
        stairs s (n - 1)

