import Data.List

solve :: [String] -> (Int, Int)
solve xs = (head maxNs, length maxNs `quot` 2)
  where
    maxNs =
        last $
        group $
        sort [compareTeams a b | a <- xs, b <- xs, a /= b]


compareTeams :: String -> String -> Int
compareTeams a b =
    foldr
        (\val acc ->
             if fst val == '1' || snd val == '1'
                 then acc + 1
                 else acc)
        0 $
    zip a b

main :: IO ()
main = interact $ unlines . (\(x, y) -> [show x, show y]) . solve . tail . lines
