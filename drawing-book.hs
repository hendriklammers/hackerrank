pageCount :: Int -> Int -> Int
pageCount n p = min front back
  where
    front = p `div` 2
    back = n `div` 2 - front

main :: IO ()
main = interact $ show . (\(n:p:_) -> pageCount n p) . map read . words
