module Main where
import Data.Char
import Control.Monad
import Control.Monad.Instances
import Control.Arrow
import System.IO
import System.Environment

-- -- Following two lines are equivalent:

-- data PostState input output = PostState output (input -> PostState input output)
data PostState input output = PostState output (State input output)

instance (Show input, Show output) => Show (PostState input output) where
    show (PostState output next) = "PostState " ++ show output ++ " (next)"

type State input output = input -> PostState input output

-- type annotation:
-- inside, out :: State Char Integer
inside, out :: Char -> PostState Char Integer
inside c = PostState 0 (if isSpace c then out else inside)
out c | isSpace c = PostState 0 out
      | otherwise = PostState 1 inside

runState :: State input output -> [input] -> [output]
-- runState :: (input -> PostState input output) -> [input] -> [output]
runState state [] = []
runState state (x:xs) = output : runState nextState xs
 where PostState output nextState = state x

countWords :: String -> Integer
countWords = sum . runState out

-- mySum = foldl (+) 0

-- foldl op start [] = start
-- foldl op start (x:xs) = foldl op (op start x) xs

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr op endPoint [] = endPoint
-- foldr op endPoint (x:xs) = op x (foldr op endpoint xs)

-- filter predicate [] = []
-- filter predicate (x:xs) = (if predicate x then (x:)
--                           else id) (filter predicate xs)
-- map f [] = []
-- map f (x:xs) = f x : map f xs

-- map' f = foldr operation []
--     where operation x rest = f x : rest

-- foldr (!) endPoint (1 : 2 : 3 : 4 : []) =
-- 1 ! (2 ! (3 ! (4 ! endpoint)))

-- sum [] = 0
-- sum (x:xs) x + sum xs

line :: State Char Integer
line '\n' = PostState 1 line
line _ = PostState 0 line

char :: State Char Integer
char _ = PostState 1 char

-- run _ [] = ouput
-- run state (x:xs) =
--     let (output, state') = state x in
--     output : run state xs

-- run == runState
run :: State input output -> [input] -> [output]
run state inputs = foldr op (const []) inputs state
    where -- op :: input -> ? -> ?
          op x rest = \state ->
              let PostState output nextState = state x in
              output : rest nextState

(><) :: State input outputA -> State input outputB -> State input (outputA, outputB)
stateA >< stateB = \input -> let PostState oA sA' = stateA input
                                 PostState oB sB' = stateB input in
                             PostState (oA, oB) (sA' >< sB')

-- (        line  ><  (        out   ><          char)) :: State Char (Integer, (Integer, Integer))
-- run (        line  ><  (  out   ><        char)) :: Sring -> [(Integer, (Integer, Integer)))]

-- sum3 :: [(Integer, (Integer, Integer)))] -> (Integer, (Integer, Integer)))

-- flop ((a,b),(c,d)) = ((a,c),(b,d))
-- x sA sB input = uncurry postX . flop . (sA &&& sB)

postX :: PostState i ol -> PostState i or -> PostState i (ol, or)
postX = undefined


(*$*) :: (Arrow a, Arrow b) => a l0 (b l1 l2) -> a r0 (b r1 r2) -> a (l0, r0) (b (l1, r1) (l2, r2))
fl *$* fr = uncurry (***) ^<< (fl *** fr)

f = do x <- a
       b x

-- a >>= \x -> b x

wcOnFile :: String -> IO ()
wcOnFile fileName
    = do handle <- openFile fileName ReadMode
         runKleisli (Kleisli print *** (Kleisli print *** Kleisli print))
                 . foldr (        plus  *$* (        plus  *$*         plus))
                         (        0     ,   (        0     ,           0))
                 .   run (        line  ><  (        out   ><          char))
                     =<< hGetContents handle
         hClose handle
    where plus = (+)

--- Horrible, horrible!
main = do args <- getArgs
          let fileName = head args
          wcOnFile fileName


-- sum = foldr (+) 0


-- Try to solve this madness with a combination of WriterMonad and
-- StateMonad, and perhaps some kind of requester monad.
-- Important is to support cartesian products of automata.