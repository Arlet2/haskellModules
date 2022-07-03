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
determinant :: Matrix -> Int
determinant m
    | not (isSquare m) = error "It's not square matrix"
    | isNullMatrix m = 0
    | otherwise = 1

isSquare :: Matrix -> Bool
isSquare m = calcCols m == calcRows m

isNullMatrix :: Matrix -> Bool
isNullMatrix m = calcCols m == 0 && calcRows m == 0

calcSize :: Matrix -> String
calcSize m = (show . calcRows $ m)++ "x" ++(show . calcCols $ m)

mulNum :: Matrix -> Int -> Matrix
mulNum m num = Matrix (map (map (* num)) (values m))


add :: Matrix -> Matrix -> Matrix
add m1 m2 =
    if calcCols m1 /= calcCols m2 || calcRows m1 /= calcRows m2 then error "Matrices have different sizes"
    else Matrix [[]]--[map (supplyFor2Elements $ (+) ( !! 0) ( !! 1)) [0..(calcCols m1)]]


{--getPair :: [[a]] -> [[a]] -> [[a]]
getPair [[]] [[]] = [[]]
getPair l1 l2 = [head l1, head l2] : getPair (tail l1) (tail l2)--}

supplyFor2Elements :: (a -> a -> a) -> [a] -> [a] -> [a]
supplyFor2Elements _ [] _ = []
supplyFor2Elements _ _ [] = []
supplyFor2Elements f (l1:l1s) (l2:l2s) =
    f l1 l2 : supplyFor2Elements f l1s l2s

mul :: Matrix -> Matrix -> Matrix
mul m1 m2 =
    if calcCols m1 /= calcRows m2 then error "You can't multiple this matrices"
    else Matrix [[1]]

calcCols :: Matrix -> Int
calcCols m =
    if not . isMatrixCorrect $ m
        then error "Matrix is incorrect"
    else
        length . head $ values m

calcRows :: Matrix -> Int
calcRows m
  | not . isMatrixCorrect $ m = error "Matrix is incorrect"
  | not (null (head (values m))) = length . values $ m
  | otherwise = 0


isMatrixCorrect :: Matrix -> Bool
isMatrixCorrect m = all (==cols) (f length (values m))
    where
        f :: (a->Int) -> [a] -> [Int]
        f _ [] = []
        f g sl = (g . head $ sl) : f g (tail sl)

        cols = length . head $ values m