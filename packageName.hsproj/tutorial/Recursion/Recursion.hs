module Recursion.Recursion
where
  
import Data.Char

allToUpper :: String -> String
allToUpper []                 = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString


-- we represent colours by strings
--
type Colour = String

-- new name for the type of colour points
--
type ColourPoint = (Int, Int, Colour)

-- compute the distance between two colour points
--
distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2) 
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1

distancesFromPoint :: ColourPoint -> [ColourPoint] -> [Float]
distancesFromPoint point []
  = []
distancesFromPoint point (p : ps)
  = distance point p : distancesFromPoint point ps
  

extractDigits :: String -> String
extractDigits []
  = []
extractDigits (chr : restString)
  | isDigit chr = chr : extractDigits restString
  | otherwise   =       extractDigits restString
  

inRadius :: ColourPoint -> Float -> [ColourPoint] -> [ColourPoint]
inRadius point radius []
  = []
inRadius point radius (p : ps)
  | distance point p <= radius = p : inRadius point radius ps
  | otherwise                  =     inRadius point radius ps
 
 
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product xs

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum xs

minList :: [Int] -> Int
minList (x:[]) = x
minList (x:xs) = x `min` minList xs

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

deductFromAccount balance []
  = balance
deductFromAccount balance (d : ds)
  | balance < d = error ("Your account balance is " ++ show balance ++
                         " - cannot deduct " ++ show d ++ " cents")
  | otherwise   = deductFromAccount (balance - d) ds


stringToInt :: String -> Int
stringToInt str = stringToIntAcc 0 str
  where
    stringToIntAcc :: Int -> String -> Int
    stringToIntAcc acc []
       = acc
    stringToIntAcc acc (chr : restString) 
       = stringToIntAcc (10 * acc + digitToInt chr) restString
       

fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs
  where
    reverseAcc :: [a] -> [a] -> [a]
    reverseAcc accList []     = accList
    reverseAcc accList (x:xs) = reverseAcc (x : accList) xs
    

sumOfSquareRoots xs = sum (allSquareRoots (filterPositives xs))
  where
    allSquareRoots []     = []
    allSquareRoots (x:xs) = sqrt x : allSquareRoots xs

    filterPositives [] 
      = []
    filterPositives (x:xs)
      | x > 0     = x : filterPositives xs
      | otherwise = filterPositives xs 
      

sumOfSquareRoots1 [] 
  = 0
sumOfSquareRoots1 (x:xs)
  | x > 0     = sqrt x + sumOfSquareRoots1 xs
  | otherwise = sumOfSquareRoots1 xs                         