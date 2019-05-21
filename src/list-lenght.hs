len :: [a] -> Int
-- len = foldl (\x _ -> x + 1) 0
len = foldr (const (+1)) 0
-- len [] = 0
-- len (_:xs) = 1 + len xs
