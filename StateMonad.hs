module StateMonad where

-- Maybe Int ~~ StateMonad s Int

data StateMonad s a = StateMonad (s -> (s, a))

instance Monad (StateMonad s) where
    return a = StateMonad (\state -> (state, a))
--    (>>=) :: StateMonad state a -> (a -> StateMonad state b) -> StateMonad state b
    (StateMonad sf) >>= f = StateMonad $ \oldState ->
                            let (state, a) = sf oldState in
                            let StateMonad f' = f a in
                            f' state

data WordState = Inside | Out

wordAutomaton c = do state <- get
                     case state of
                       Inside | isSpace c = do put Out
                                               return 0
                              | otherwise = return 0
                       Out | isSpace c = return 0
                           | otherwise = do put Inside
                                            return 1

put :: state -> StateMonad state ()
put state = StateMonad (\oldState -> (state, ()))

get :: StateMonad state state
get = StateMonad (\state -> (state, state))

runState :: s -> StateMonad s a -> (s, a)
runState s0 (StateMonad s) = s s0
evalState :: s -> StateMonad s a -> a
evalState s0 s = snd (runState s0 s)

stateFul = do put 8
              -- do lots of stuff here
              anotherSideEffect
              sideEffecting

anotherSideEffect = put 12
sideEffecting = do x <- get
                   if x >= 10 then
                       return "Hello, big"
                    else return "Hello, small"