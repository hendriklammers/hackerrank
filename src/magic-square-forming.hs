import Data.List

type Square = [[Int]]

magicSquare :: Square
magicSquare =
    [ [ 8, 3, 4 ]
    , [ 1, 5, 9 ]
    , [ 6, 7, 2 ]
    ]

squares :: [Square]
squares = squares' ++ map transpose squares'
  where
    squares' = take 4 $ iterate (map reverse . transpose) magicSquare

cost :: Square -> Square -> Int
cost s1 s2 = sum $ map abs $ zipWith (-) (concat s1) (concat s2)

formingMagicSquare :: Square -> Int
formingMagicSquare s =
    minimum $ map (cost s) squares

main :: IO ()
main = interact $ show . formingMagicSquare . map (map read . words) . lines
