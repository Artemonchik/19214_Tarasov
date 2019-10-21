
get' ::  [a] -> Integer -> a
get' [] n = error "index out of range"
get' (x:xs) 0 = x 
get' (x:xs) n | n >= 0 = get' xs (n-1)
              | otherwise = error "index is negative"

head' :: [a] -> a
head' [] = error "list is empty"
head' (x:xs) = x 

last' :: [a] -> a 
last' [] = error "list is empty"
last' [x] = x
last' (x:xs) = last' xs


tail' :: [a] -> [a]
tail' [] = error "list is empty"
tail' (x:xs) = xs
 
init' :: [a] -> [a] 
init' [] = error "list is empty"
init' [x] = []
init' (x:xs) = x:init' xs

foldl' ::(a -> b -> a) -> a -> [b] -> a
foldl' func acc xs = helper acc xs where
    helper acc [] = acc
    helper acc (x:xs) = helper (func acc x) xs

reverse' :: [a] -> [a]
reverse' = foldl' (\acc elem -> elem:acc) [] 

foldr' :: (b -> a -> a) -> a -> [b] -> a 
foldr' func acc xs = foldl' (\x y -> func y x) acc (reverse' xs)    

(+++) :: [a] -> [a] -> [a]
[] +++ xs = xs
(x:xs) +++ ys = x: (xs +++ ys)

length' :: [a] -> Integer
length' [] = 0
length' xs = helper 0 xs where
        helper acc [] = 0
        helper acc (x:xs) = 1 + helper acc xs 

append' :: [a] -> a -> [a]
append' xs x = xs +++ [x] 

concat' :: [a] -> [a] -> [a]
concat' xs1 xs2 = foldr' (\elem acc -> elem:acc) xs2 xs1

drop' :: Integer -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs


take' :: Integer -> [a] -> [a]
take' n [] = []
take' 0 xs = []
take' n (x:xs) = x:take' (n-1) xs

split' :: Integer -> [a] -> ([a],[a])
split' n xs | n <= length' xs && n > 0 = (take' n xs, drop' n xs)
            | otherwise = error "index is incorrect"

null' :: [a] -> Bool 
null' [] = True
null' xs = False

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] element = False
elem' (x:xs) element = if x == element then True else elem' xs element

filter' :: (a -> Bool) -> [a] -> [a]
filter' test xs = foldr' (\ elem acc -> if test elem then elem:acc else acc) [] xs

map':: (a -> b) -> [a] -> [b]
map' func xs = foldr' (\b local_xs-> func b:local_xs) [] xs

zip' :: [a] -> [b] -> [(a,b)] 
zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 