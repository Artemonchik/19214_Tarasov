quadraticRoots :: (Eq a, Floating a, Ord a) => a -> a -> a -> (a, a)
quadraticRoots 0 0 _ = error "this is not equation at all"
quadraticRoots 0 b c = ((-c / b), (-c / b))
quadraticRoots a b c | d < 0 = error "this equation doesn't have roots"
                     | otherwise = (x1, x2) where
                        d = b*b - 4 * a * c
                        x1 = (-b + sqrt d) / 2 / a
                        x2 = (-b - sqrt d) / 2 / a 
                        

 
