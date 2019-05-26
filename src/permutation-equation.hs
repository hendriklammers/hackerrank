import Data.List
import Data.Maybe

index :: Int -> [Int] -> Int
index x xs = fromMaybe (-1) $ elemIndex x xs

permutationEquation :: [Int] -> [Int]
permutationEquation xs =
    map (\i -> index (index i xs + 1) xs + 1) [1 .. length xs]

main :: IO ()
main =
    interact $
    unlines . map show . permutationEquation . map read . tail . words
