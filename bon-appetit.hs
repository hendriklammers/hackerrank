import Control.Monad

getInts :: IO [Int]
getInts = map read . words <$> getLine

removeByIndex :: Int -> [a] -> [a]
removeByIndex _ [] = []
removeByIndex i xs = take i xs ++ drop (i + 1) xs

checkBill :: Int -> Int -> [Int] -> String
checkBill k b bill =
    if s == b
        then "Bon Appetit"
        else show $ b - s
  where
    s = (`quot` 2) $ sum $ removeByIndex k bill

main :: IO ()
main = do
    [[_, k], bill, [b]] <- replicateM 3 getInts
    putStrLn $ checkBill k b bill
