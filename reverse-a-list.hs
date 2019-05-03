rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- fold = foldl (flip (:)) [] [1..5]
