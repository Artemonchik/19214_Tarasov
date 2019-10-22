quadraticRoots :: (Eq a, Floating a) => a -> a -> a -> (a, a)
quadraticRoots a b c | (a == 0) && (b /= 0) = ((-c / b), (-c / b))
                     | (a == 0) && (b == 0) = error "this is not equation at all"
                     | d < 0 = error "this equation doesn't have roots"
                     | otherwise = (x1, x2) where
                        d = b*b - 4 * a * c
                        x1 = (-b + sqrt d) / 2 / a
                        x2 = (-b - sqrt d) / 2 / a 
                        

 
