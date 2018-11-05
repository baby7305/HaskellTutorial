module Exercise
where

sort2 :: Ord a => a -> a -> (a, a)
sort2 x y | x<=y =(x,y)
          | otherwise =(y,x)
          

almostEqual (x1, y1) (x2, y2)
  | (x1 == x2) && (y1 == y2) = True
  | (x1 == y2) && (y1 == x2) = True
  | otherwise                = False
  
isLower :: Char -> Bool
isLower x|elem x ['a'..'z'] =True
         |otherwise =False
         
mangle :: String -> String
mangle x| x==[] =" "
        | otherwise = tail x ++ take 1 x 


divide :: Int -> Int -> Int
divide x y|x>=y =div x y
          |otherwise =div y x