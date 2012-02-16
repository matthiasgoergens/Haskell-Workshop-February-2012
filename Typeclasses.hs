module Main where
import Prelude hiding (Maybe(Just, Nothing), Functor (fmap))

data Maybe a = Nothing | Just a
 deriving (Show, Read, Eq, Ord)
data Tree a = Empty | Node a (Tree a) (Tree a)
 deriving (Show, Read, Eq, Ord)

tree = Node 1 (Node 2 Empty Empty) Empty


-- fmap show ([1,2,3] :: [Int]) == ["1", "2", "3"] :: [String]
-- show :: Int -> String
-- fmap show (Just 1 :: Maybe Int) == Just "1" :: Maybe String
-- fmap show Nothing = Nothing

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

--  flipped fmap :: f a -> (a -> b) -> f b

-- fmap (f . g) == fmap f . fmap g
-- fmap id == id

class (Functor pf) => PointedFunctor pf where
    singleton :: a -> pf a

instance Functor Maybe where
--    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs

instance PointedFunctor Maybe where
    singleton a = Just a

instance PointedFunctor [] where
    singleton a = [a]

-- ApplicativeFunctor
class PointedFunctor app => Applicative app where
    (<*>) :: app (a -> b) -> app a -> app b

instance Applicative Maybe where
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    (Just f) <*> (Just a) = Just (f a)

instance Applicative [] where
--    (<*>) :: [a -> b] -> [a] -> [b]
    [] <*> _ = []
    _ <*> [] = []

    (f:fs) <*> l = (fmap f l) ++
                   (fs   <*> l)

(<$>) :: Applicative app => (a -> b) -> app a -> app b
-- f <$> args = singleton f <*> args
(<$>) = fmap

--    (f:fs) <*> (x:xs)   = f x : (fs <*> xs)
--    (<*>) (f:fs) (x:xs) = f x : (<*>) fs xs

cartesian = (singleton (,) <*> "bob") <*> [0,1,2]
-- (singleton (,) <*> "bob"

-- a = hGetContents =<< openFile "/etc/fstab"

-- print :: Show a => a -> IO ()
-- getArgs :: IO [String]
-- (>>=) :: m a -> ((a -> m b) -> m b)
-- (>>=) :: IO [String] -> ([String] -> IO ()) -> IO ()

-- getArgs >>= print

-- single

-- class Applicative m => Monad m where
--     (>>=) :: m a -> ((a -> m b) -> m b)

    -- action >>= fAction
    -- action :: m a
    -- fAction :: a -> m b

instance Monad Maybe where
--    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= f = Nothing
    (Just a) >>= f = f a

    -- op :: Maybe a -> (a -> Maybe b) -> Maybe (Maybe b)
    -- op Nothing _ = Nothing
    -- op (Just a) f = Just (f a)

--    op a f = 
--                    fmap, singleton, <*>, <$>

main = getLine >>=
       (\line1 -> getLine >>=
                  (\line2 -> print (line1, line2)))

main' = do line1 <- getLine
           line2 <- getLine
           print (line1, line2 ++ " append something")

test a b = do x <- a
              y <- b
              return (x,y)
test' a b = a >>= \x ->
            b >>= \y ->
            return (x,y)

-- a :: IO String
-- take 5 :: String -> String
-- fmap (take 5) :: IO String -> IO String

-- length :: String -> c

data StateMonad 