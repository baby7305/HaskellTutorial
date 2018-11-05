module Simple2
where
  
addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)

type Point = (Int, Int)

-- origin of the coordinate system
--
origin :: Point
origin  = (0, 0)

-- move a given point to the right
--
moveRight :: Point -> Int -> Point
moveRight (x, y) distance  = (x + distance, y)

-- move a given point to upwards
--
moveUp :: Point -> Int -> Point
moveUp (x, y) distance  = (x, y + distance)