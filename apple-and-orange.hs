countApplesOranges :: [Int] -> [Int]
countApplesOranges (s:t:a:b:m:n:rest) =
    [count a $ take m rest, count b $ drop m rest]
  where
    count l xs = length $ filter (\x -> x >= s && x <= t) $ map (+ l) xs

main :: IO ()
main = interact $ unlines . map show . countApplesOranges . map read . words
