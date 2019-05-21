import Data.List (intercalate)
import Data.List.Split (splitOn)

convert :: String -> [String]
convert t
    | f == "AM" && h == "12" = "00" : ms
    | f == "PM" && h == "12" = h : ms
    | f == "PM" = show (read h + 12) : ms
    | otherwise = h : ms
  where
    (h:ms) = splitOn ":" $ take 8 t
    f = drop 8 t

main :: IO ()
main = interact $ intercalate ":" . convert
