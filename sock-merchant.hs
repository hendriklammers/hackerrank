import Data.Set (Set, empty, member, insert, delete)

solve :: [Int] -> Int
solve =
    snd .
    foldr
        (\x (set, n) ->
             if x `member` set
                 then (delete x set, n + 1)
                 else (insert x set, n))
        (empty, 0)

main :: IO ()
main = interact $ show . solve . map read . tail . words
