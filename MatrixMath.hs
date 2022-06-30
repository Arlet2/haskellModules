module MatrixMath (Matrix (Matrix), mulNum, calcSize, determinant) where
newtype Matrix = Matrix {values:: [[Int]]} deriving (Show)
instance Eq Matrix where
    (==) m1 m2 = (calcRows m1 == calcRows m2) && (calcCols m1 == calcCols m2) && (values m1 == values m2)
instance Num Matrix where
    (+) = add
    (*) = mul
    negate m = mulNum m (-1)
    abs = error "You can't use abs for matrix"
    signum = error "You can't get sign from matrix"
    fromInteger = error "You can't convert integer to matrix"
{--
class Num a => MatrixLogic a where
    (*) :: a -> Int -> a

instance MatrixLogic Matrix where
    (*) m num = Matrix v
        where
            v = map (map (Prelude.* num)) (values m)
--}

mulNum :: Matrix -> Int -> Matrix
mulNum m num = Matrix (map (map (Prelude.* num)) (values m))
            

add :: Matrix -> Matrix -> Matrix
add m1 m2 = 
    if calcCols m1 /= calcCols m2 || calcRows m1 /= calcRows m2 then error "Matrices have different sizes"
    else Matrix (map (map (+1)) (values m2))

mul :: Matrix -> Matrix -> Matrix
mul m1 m2 = 
    if calcCols m1 /= calcRows m2 then error "You can't multiple this matrices"
    else Matrix [[1]]

calcSize :: Matrix -> String
calcSize m = (show . calcRows $ m)++ "x" ++(show . calcCols $ m)

calcCols :: Matrix -> Int
calcCols m =
    if not . isMatrixCorrect $ m 
        then error "Matrix is incorrect"
    else 
        length . head $ values m

calcRows :: Matrix -> Int
calcRows m =
    if not . isMatrixCorrect $ m 
        then error "Matrix is incorrect"
    else length . values $ m


isMatrixCorrect :: Matrix -> Bool
isMatrixCorrect m = 
    let 
        rows = length . values $ m
        cols = length . head $ values m
    
    
determinant :: Matrix -> Int
determinant m = 0
