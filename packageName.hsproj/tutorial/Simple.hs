-- Example module
-- Gabriele Keller, August 2015
--
-- This is a simple example of a module definition

module Simple
where

-- calculates the arithmetic mean of two numbers
--
arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean x y  = (x + y)  / 2

-- calculates the harmonic mean of two numbers
--
harmonic_mean :: Fractional a => a -> a -> a
harmonic_mean x y  = 2 * x * y / (x + y)


--max1 :: Ord a => a -> a -> a
--max1 x y = if x >= y then x else y

signum :: (Ord a, Num a) => a -> Int
signum x | x <  0     = -1
         | x == 0     = 0
         | otherwise  = 1
         
max1 :: Ord a => a -> a -> a
max1 x y | x >= y     = x 
         | otherwise  = y
         

pi1 :: Floating a => a
pi1 = 3.141592653589793

circleArea :: Floating a => a -> a
circleArea radius  = pi1 * radius * radius

circleArea' :: Floating a => a -> a
circleArea' diameter  = pi * radius * radius
  where
    radius = diameter / 2.0       -- local binding