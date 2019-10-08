mapl:: (a -> b) -> [a] -> [b]
mapl func xs = foldl (\local_xs b -> local_xs ++ [func b]) [] xs

mapr:: (a -> b) -> [a] -> [b]
mapr func xs = foldr (\b local_xs-> func b:local_xs) [] xs