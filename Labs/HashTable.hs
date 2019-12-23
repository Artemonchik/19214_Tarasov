import Data.Char
import System.IO
import Control.Exception

hash :: Integer -> String -> Integer
hash max str = (foldr (\prev curr -> (prev + curr * 31)) 0 $ map (toInteger.ord) str) `mod` max

type Size = Integer
type Quantity = Integer
data HashTable k v = HashTable [[(k,v)]] Size Quantity

instance (Show k, Show v) => Show (HashTable k v) where
    show (HashTable rows size quantity) = "####" ++ (foldl (\str elem -> str ++ "\t\n" ++ show(elem)) "" rows) ++ "\n####"

defaultHashTable :: Integer -> HashTable k v
defaultHashTable size = HashTable (replicate (fromIntegral size) []) size 0 

reHash :: (Show k, Eq k) => HashTable k v -> HashTable k v
reHash (HashTable rows size quantity) | loadFactor < 0.75 = HashTable rows size quantity
                                      | otherwise = fromList $ foldl (++) [] rows where 
                                        loadFactor = (fromIntegral quantity) / (fromIntegral size)

insert::(Show k, Eq k) => HashTable k v -> k -> v -> HashTable k v
insert (HashTable rows size quantity) key value = reHash $ HashTable ((take currentHash rows) ++ ([(filter (\(k,v) -> k /= key) (rows !! currentHash)) ++ [(key, value)]]) ++ (drop (currentHash + 1) rows)) size (quantity + difference)
                                    where currentHash = fromIntegral (hash size (show key))
                                          difference = toInteger $ (length $ filter (\(k,v) -> k /= key) (rows !!  currentHash)) - (length $ rows !! currentHash)
fromList::(Show k, Eq k)=> [(k,v)]->HashTable k v
fromList pairs = foldl (\table (k, v) -> insert table k v) (defaultHashTable len)  pairs
                                        where len = ((*3).toInteger.length) pairs

clear::HashTable k v -> HashTable k v
clear (HashTable rows size quantity) = defaultHashTable (toInteger $ length rows)

-- erase::(Show k)=>HashTable k v -> k -> HashTable k v
erase::(Show k, Eq k)=>HashTable k v->k->HashTable k v
erase (HashTable rows size quantity) key = reHash $ HashTable ((take currentHash rows) ++ ([(filter (\(k,v) -> k /= key) (rows !!  currentHash)) ]) ++ (drop (currentHash + 1) rows)) size (quantity + difference)
                                    where currentHash = fromIntegral (hash size (show key))
                                          difference = toInteger $ (length $ filter (\(k,v) -> k /= key) (rows !!  currentHash)) - (length $ rows !! currentHash)
contains::(Show k, Eq k)=>HashTable k v -> k -> Bool
contains (HashTable rows size quantity) key = any (\(k, v) -> k == key) (rows !! currentHash) where 
                                        currentHash = fromIntegral (hash size (show key))

at::(Show k, Eq k)=>HashTable k v -> k -> Maybe v
at (HashTable rows size _) key = lookup key (rows !! currentHash) where 
                                        currentHash = fromIntegral (hash size (show key))

size::(Show k, Eq k)=>HashTable k v -> Integer
size (HashTable _ _ quantity) = quantity
hashTable = fromList [("a", "Artemchik"), ("b", "KreiziDanila"), ("m", "StopThisShit"), ("c", "I want to get A mark")]

empty::(Show k, Eq k)=>HashTable k v -> Bool;
empty (HashTable _ size _) = size == 0 

main = do
    input <- openFile "data.txt" ReadMode
    content <- map (\line -> ((words line)!! 0, (words line)!! 1)) <$> lines <$> hGetContents input
    writeFile "result.txt" (show $ fromList content)
    hClose input
