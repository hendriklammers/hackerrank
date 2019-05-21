-- Each new words starts with uppercase so we count all uppercase chars and add
-- 1 for the first word (which doesn't start with uppercase)
solve :: String -> Int
solve = (+) 1 . length . filter (`elem` ['A' .. 'Z'])

main :: IO ()
main = interact $ show . solve
