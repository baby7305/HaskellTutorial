module Exercise.Exercise
where
 
fact:: Int->Int 
fact n
 | n<=0 =1
 | otherwise =n * fact(n-1)
 
test4::Int->Int->[Int]
test4  m n
  | m>n =[]
  | m==n =[m]
  | otherwise =m:(test4 (m+1) n)
  
countOdds:: [Int]->Int
countOdds []=0
countOdds (x:xs)
  | odd x = countOdds xs +1
  | otherwise =countOdds xs
  
removeOdd:: [Int]->[Int]
removeOdd []=[]
removeOdd (x:xs)
  | odd x =removeOdd xs
  | otherwise =x:(removeOdd xs)