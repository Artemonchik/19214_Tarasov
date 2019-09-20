quadraticRoots :: Float -> Float -> Float -> (Float, Float)
quadraticRoots a b c | a == 0 = error "a koefficent shouldn\'t be a zero"
                     | otherwise = (x1, x2) where
                        d = b*b - 4 * a * c
                        x1 = (-b + sqrt d) / 2
                        x2 = (-b - sqrt d) / 2
                        

 