data MyList a = Empty | Cons a (MyList a)
                deriving (Show, Read, Eq)

-- class Show a where
--     show :: a -> String

-- instance Show (MyList Int) where
--     show Empty = "Empty"
--     show (Cons number rest) = asRomanNumeral number ++ ":" ++ show rest

-- asRomanNumeral number = "XIM"

-- instance Show a => Show (MyList a) where
--     show Empty = "My own written: Empty"
--     show (Cons a rest) = "Starts with " ++ show a ++ "rest is: " ++ show rest

aList = Empty
bList = Cons "hello" Empty
cList = Cons "hello" (Cons "world" Empty)
-- dList = Cons 12 (Cons "doesn't work" Empty)

-- data MyStrangeList a b = Empty' | ConsA a (MyStrangeList a b)
--                        | ConsB b (MyStrangeList a b)

-- data Either a b = Left a | Right b

-- type MyStrangeList' a b = MyList (Either a b)

-- myStrangeList = ConsA "Bla" (ConsB 12 Empty)

-- myStrangeList' = Cons (Left "Bla") (Cons (Right 12) Empty)

myOtherFunction number =
    case number of
      0 | condition1 -> aeoueo
        | condition2 -> somethingOthueaueaueaer
      1 -> somethingElse
      Just x -> blabla
      Nothing -> eau
    were f x | guard1 = mylocalfunctiondefinition
             | guard2 = ueau
          g = negate 
myFunction number = let myNewStuff x = 1 + 2 + x
                        myNewStuffOther = 17
                    in case () of
                         () | condition1 -> something (myNewStuff 3)
                            | condition2 -> somethingOther



-- out ' ' = something
-- out '\t' = something
-- out '\n' = something
-- out c = if isSpace c
--         then PostState 0 out
--         else PostState 1 inside

-- function args | condition :: Boolean = value
--               | condition2 = value
--               | True = value

ourFunctionalIf True thenBranch elseBrach = thenBranch
ourFunctionalIf False thenBranch elseBranch = elseBranch

