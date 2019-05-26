-- TODO: Try recursion and optimize performance
import Control.Monad

rotateList :: Int -> [Int] -> [Int]
rotateList n xs = drop r xs ++ take r xs
  where
    r = length xs - (n `mod` length xs)

circularArrayRotation :: [Int] -> Int -> [Int] -> [Int]
circularArrayRotation a k = map (\x -> rotateList k a !! x)

getInts :: IO [Int]
getInts = map read . words <$> getLine

main :: IO ()
main = do
    [_, k, q] <- getInts
    arr <- getInts
    q <- replicateM q $ fmap read getLine :: IO [Int]
    putStrLn $ unlines $ map show $ circularArrayRotation arr k q
