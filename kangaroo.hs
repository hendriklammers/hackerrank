main :: IO ()
main = interact $ kangaroo . map read . words
  where
    kangaroo [x1, v1, x2, v2] =
        if v2 < v1 && (x2 - x1) `mod` (v1 - v2) == 0
            then "YES"
            else "NO"

