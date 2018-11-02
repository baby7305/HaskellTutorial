inc x = x + 1--function equation

double::Int -> Int
double x = 2 * x

exclaim::String -> String
exclaim sentence = sentence++"!"

average::Float -> Float -> Float
average a b = (a + b)/2.0

square::Int -> Int
square x = x * x

showResult::Int -> String
showResult x = "The result is "++show (x)

showAreaOfCircle x = "The area of a circle with radius "++show(x)++"cm"++" is about  "++show(showAreaOfCircle1(x))++" cm^2"

showAreaOfCircle1 x = 3.14 * (x)^2