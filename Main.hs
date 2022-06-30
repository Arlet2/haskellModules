module Main where
import MatrixMath(Matrix (Matrix), mulNum, calcSize)
main :: IO()
main = do
    print (calcSize m2)
    where
        m1 = Matrix [[2, 3, 4], [0, 1, 2]]
        m2 = Matrix [[2, 4, 6], [0, 0, 0], [1, 1 ,1]]