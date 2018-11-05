module Simple3
where
  
-- we represent colours by strings
--
type Colour = String

-- new name for the type of colour points
--
type ColourPoint = (Int, Int, Colour)

-- origin of the coordinate system in a given colour
--
origin :: Colour -> ColourPoint
origin colour  = (0, 0, colour)

-- move a colour point vertically and horizontally
--
move :: ColourPoint -> Int -> Int -> ColourPoint
move (x, y, colour) xDistance yDistance  
  = (x + xDistance, y + yDistance, colour)

-- compute the distance between two colour points
--
distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2) 
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1
    
--distance (x1, y1, colour1) (x2, y2, colour2) 
--  = sqrt (fromIntegral (dx * dx + dy * dy))  
--  where {dx = x2 - x1; dy = y2 - y1}