data Complex a = Complex {
    real :: a,
    imaginary :: a
}
instance Functor Complex where
    fmap f (Complex r i) = Complex (f r) (f i)

instance (Show a) => Show (Complex a) where
    show (Complex real imaginarity) = (show real) ++ " + " ++ (show imaginarity) ++ "i"

instance (Ord a) => Ord (Complex a) where
    compare _ _= error "we can\'t compare Complex"

instance (Eq a) => Eq (Complex a) where
    (==) (Complex r1 i1) (Complex r2 i2) = r1 == r2 && i1 == i2
    


    
data QuantumState a = QuantumState {
    complex :: Complex a,
    str:: String
} -- try to create complex num

instance (Show a) => Show (QuantumState a) where
    show (QuantumState complex str) = "Complex: " ++ (show complex) ++ ", label: " ++ str

instance (Ord a) => Ord (QuantumState a) where
    compare _ _ = error "we can't compare digits"
instance (Eq a) => Eq (QuantumState a) where
    (==) (QuantumState complex1 str1) (QuantumState complex2 str2) = (complex1 == complex2) && (str1 == str2) 

type Qubit a = [QuantumState a]


instance Functor QuantumState where
    fmap f (QuantumState complex str) = QuantumState (fmap f complex) str
-- instance Functor QuantumState where
--     fmap f (QuantumState complex str) = QuantumState (f complex) str   

toList::Qubit a -> [Complex a]
toList [] = []
toList ((QuantumState complex _):qubit) = complex: (toList qubit)   

toLabelList :: Qubit a -> [String]
toLabelList [] = []
toLabelList ((QuantumState _ str):qubit) = str: (toLabelList qubit) 

fromList:: [Complex a]->[String]->Qubit a
fromList _ [] = []
fromList [] _ = []
fromList (complex:complexes) (string:strings) = QuantumState complex string:(fromList complexes strings)

toPairList:: Qubit a->[(Complex a,String)]
toPairList = map (\(QuantumState complex string) -> (complex, string))

fromPairList:: [(Complex a,String)] -> Qubit a
fromPairList = map (\(complex,  string) -> QuantumState complex string)

-- scalarProduct:: (Num a) => Qubit a -> Qubit a -> a
-- scalarProduct  qubit2 qubit1 = foldl (*) 1 

list1 = [QuantumState (Complex 1 1) "hellow", QuantumState (Complex 2 11) "buy" ]
list2 = [Complex 0 12, Complex 23 26, Complex 112 213]
list3 = ["MAMA", "PAPA", "BATYA"]