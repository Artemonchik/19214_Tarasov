get' :: (Eq b, Num b) => [a] -> b -> a
get' (x:xs) 0 = x 
get' (x:xs) n = get' xs (n-1)   

head' :: [a] -> a
head' (x:xs) = x 

last' :: [a] -> a 
last' [x] = x
last' (x:xs) = last' xs

tail' :: [a] -> [a]
tail' (x:xs) = xs

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x:init' xs

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc elem -> elem:acc) [] xs -- почему если убрать xs слева и справа, то не произойдет каррирование и будет ошибка

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

append' :: [a] -> a -> [a]
append' xs x = xs ++ [x]

concat' :: [a] -> [a] -> [a]
concat' xs1 xs2 = xs1 ++ xs2

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n (x:xs) = x:take' (n-1) xs

split' :: Int -> [a] -> ([a],[a])
split' n xs = (take' n xs, drop' n xs)

null' :: [a] -> Bool 
null' [] = True
null' xs = False

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] element = False
elem' (x:xs) element = if x == element then True else elem' xs element

filter' :: (a -> Bool) -> [a] -> [a]
filter' test xs = foldl (\ acc elem -> if test elem then elem:acc else acc) [] xs

map':: (a -> b) -> [a] -> [b]
map' func xs = foldr (\b local_xs-> func b:local_xs) [] xs

zip' :: [a] -> [b] -> [(a,b)] 
zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 