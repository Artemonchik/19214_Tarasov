import Data.Char
import System.IO
import Control.Exception

hash :: Integer -> String -> Integer
hash max str = (foldr (\prev curr -> (prev + curr * 31)) 0 $ map (toInteger.ord) str) `mod` max

type Size = Integer
data HashTable k v = HashTable [[(k,v)]]

instance (Show k, Show v) => Show (HashTable k v) where
    show (HashTable rows) = "####" ++ (foldl (\str elem -> str ++ "\t\n" ++ show(elem)) "" rows) ++ "\n####"

defaultHashTable :: Integer -> HashTable k v
defaultHashTable size = HashTable (replicate (fromIntegral size) []) 

reHash :: (Show k, Eq k) => HashTable k v -> HashTable k v
reHash (HashTable rows) | loadFactor < 0.75 = HashTable rows
                        | otherwise = fromList $ foldl (++) [] rows
                                where 
                                len = toInteger (length rows)
                                quantity = foldl (\count list -> count + (toInteger $ length list)) 0 rows 
                                loadFactor = (fromIntegral quantity) / (fromIntegral len)

insert::(Show k, Eq k) => HashTable k v -> k -> v -> HashTable k v
insert (HashTable rows) key value = reHash $ HashTable ((take currentHash rows) ++ ([(filter (\(k,v) -> k /= key) (rows !!  currentHash)) ++ [(key, value)]]) ++ (drop (currentHash + 1) rows))
                                    where len = toInteger $ length rows
                                          currentHash = fromIntegral (hash len (show key))

fromList::(Show k, Eq k)=> [(k,v)]->HashTable k v
fromList pairs = foldl (\table (k, v) -> insert table k v) (defaultHashTable len)  pairs
                                        where len = ((*3).toInteger.length) pairs

clear::HashTable k v -> HashTable k v
clear (HashTable rows) = defaultHashTable (toInteger $ length rows)

-- erase::(Show k)=>HashTable k v -> k -> HashTable k v
erase::(Show k, Eq k)=>HashTable k v->k->HashTable k v
erase (HashTable rows) key = reHash $ HashTable ((take currentHash rows) ++ ([(filter (\(k,v) -> k /= key) (rows !!  currentHash)) ]) ++ (drop (currentHash + 1) rows))
                                    where len = toInteger $ length rows
                                          currentHash = fromIntegral (hash len (show key))
contains::(Show k, Eq k)=>HashTable k v -> k -> Bool
contains (HashTable rows) key = any (\(k, v) -> k == key) (rows !!  currentHash) where 
                                        len = toInteger $ length rows
                                        currentHash = fromIntegral (hash len (show key))

at::(Show k, Eq k)=>HashTable k v -> k -> Maybe v
at (HashTable rows) key = lookup key (rows !! currentHash) where 
                                        len = toInteger $ length rows
                                        currentHash = fromIntegral (hash len (show key))

size::(Show k, Eq k)=>HashTable k v -> Integer
size (HashTable rows) = foldl (\count list -> count + (toInteger $ length list)) 0 rows
hashTable = fromList [("a", "Artemchik"), ("b", "KreiziDanila"), ("m", "StopThisShit"), ("c", "I want to get A mark")]

empty::(Show k, Eq k)=>HashTable k v -> Bool;
empty = ((==0) . size) 

main = do
    input <- openFile "data.txt" ReadMode
    content <- map (\line -> ((words line)!! 0, (words line)!! 1)) <$> lines <$> hGetContents input
    writeFile "result.txt" (show $ fromList content)
    hClose input