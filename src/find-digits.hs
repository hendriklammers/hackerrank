solve :: String -> Int
solve n =
    length $
    filter (== 0) $
    map (\x ->
             if x == '0'
                 then 1
                 else read n `mod` read [x])
        n

main :: IO ()
main = interact $ unlines . map (show . solve) . tail . lines
