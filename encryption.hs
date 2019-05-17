import Data.List
import Data.List.Split

cols :: String -> Int
cols = ceiling . sqrt . fromIntegral . length

encrypt :: String -> String
encrypt s = unwords $ transpose $ chunksOf (cols s) s

main :: IO ()
main = interact $ encrypt . filter (/= ' ')
