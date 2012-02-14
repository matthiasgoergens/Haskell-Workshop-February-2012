module Main where
import Data.Char
import Control.Monad
import Control.Monad.Instances
import Control.Arrow

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

line :: State Char Integer
line '\n' = PostState 1 line
line _ = PostState 0 line

char :: State Char Integer
char _ = PostState 1 char

-- run _ [] = ouput
-- run state (x:xs) =
--     let (output, state') = state x in
--     output : run state xs
run toState list = foldr op (const []) list toState
    where op x rest toState =
              let PostState output toState' = toState x in
              output : rest toState'

(><) :: State i oA -> State i oB -> State i (oA, oB)
stateA >< stateB = \input -> let PostState oA sA' = stateA input
                                 PostState oB sB' = stateB input in
                             PostState (oA, oB) (sA' >< sB')

-- flop ((a,b),(c,d)) = ((a,c),(b,d))
-- x sA sB input = uncurry postX . flop . (sA &&& sB)

postX :: PostState i ol -> PostState i or -> PostState i (ol, or)
postX = undefined


(*$*) :: (Arrow a, Arrow b) => a l0 (b l1 l2) -> a r0 (b r1 r2) -> a (l0, r0) (b (l1, r1) (l2, r2))
fl *$* fr = uncurry (***) ^<< (fl *** fr)

main = runKleisli (Kleisli print *** (Kleisli print *** Kleisli print))
       .    foldr (        plus  *$* (        plus  *$*         plus))
                  (        0     ,   (        0     ,           0))
       .      run (        line  ><  (        out   ><          char))
     =<< getContents

    where plus = (+)


-- Try to solve this madness with a combination of WriterMonad and
-- StateMonad, and perhaps some kind of requester monad.
-- Important is to support cartesian products of automata.