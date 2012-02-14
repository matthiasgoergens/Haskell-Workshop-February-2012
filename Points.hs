-- type CountedString = (Integer, String)
-- type Name parameter1 parameter2 = (String, parameter1) -> parameter2


-- Name Integer Bool === (String, Integer) -> Bool

-- type WebAddress = String
-- type PostalAddress = String

-- Compiler couldn't help detecting mixing them up.
--           0              1
-- Maybe a = Nothing | Just a

-- struct Point { int x; int y; };
-- struct Point jennysPoint = {17, 23};

-- struct Point MakePoint (int x, int y) {
--              struct Point newPoint = {x, y};
--              return newPoint;
-- }

-- type NewType = OldType

-- data NewType = NewConstruct OldType OldType | NewConstruct2 OldType | NewConstruct3

-- data Point = MakePoint Integer Integer
type Point = (ParametricPoint Integer)
data ParametricPoint a = MakeParametricPoint a a

jennysPoint :: Point
jennysPoint = MakeParametricPoint 23 17

-- anotherPoint :: ParametricPoint String
anotherPoint = MakeParametricPoint "left" "right"

squaredDistanceFromZero :: Point -> Integer
squaredDistanceFromZero (MakeParametricPoint x y) = x*x + y*y

main = return ()
