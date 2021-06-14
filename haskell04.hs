-- Prática 04 de Haskell
-- Nome: Robson Daniel Marchesan

faixaIdoso :: Int -> String
faixaIdoso f 
        | f >= 60 && f < 65 = "IDO64"
        | f >= 65 && f < 70 = "IDO69"
        | f >= 70 && f < 75 = "IDO74"
        | f >= 75 && f < 80 = "IDO79"
        | f >= 80 = "IDO80"
        | otherwise = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos lst = [(x,y,faixaIdoso y) | (x,y) <- lst]

classifIdosos' ::  [(String,Int)] -> [(String,Int,String)]
classifIdosos' lst1 = map (\(x,y) -> (x, y, faixaIdoso y))lst1 

strColor :: (Int,Int,Int) -> String
strColor (r,g,b) = "rgb" ++ show(r,g,b)

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = [(x,cy,r) | x <- take n [cx, cx + 2*r..]]

genReds :: Int -> [(Int,Int,Int)] 
genReds numr = [(x,0,0) | x <- take numr [10,10+5..]]