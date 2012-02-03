module Main where
import Data.Char
import Control.Monad
import Control.Monad.Instances
import Control.Arrow

data PostState input output = PostState output (input -> PostState input output)
type State input output = input -> PostState input output

inside, out :: State Char Integer
inside s = PostState 0 (if isSpace s then out else inside)
out s | isSpace s = PostState 0 out
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



(*$*) :: (Arrow a, Arrow b) => a l0 (b l1 l2) -> a r0 (b r1 r2) -> a (l0, r0) (b (l1, r1) (l2, r2))
fl *$* fr = uncurry (***) ^<< (fl *** fr)

main = runKleisli (Kleisli print *** (Kleisli print *** Kleisli print))
       .    foldr (        plus  *$* (        plus  *$*         plus))
                  (        0     ,   (        0     ,           0))
       .      run (        line  ><  (        out   ><          char))
     =<< getContents

    where plus = (+)