module Main where
import System.Environment

-- class Functor f where
--       fmap :: (a -> b) -> f a -> f b


class Queue q where
      empty :: q a
      enqueue :: a -> q a -> q a
      dequeue :: q a -> (Maybe a, q a)
      isEmpty :: q a -> Bool

data SimplestQueue a = SimplestQueue [a]
 deriving (Read, Show, Eq, Ord)

instance Queue SimplestQueue where
    empty = SimplestQueue []

    enqueue a (SimplestQueue q) = SimplestQueue (a:q)

    dequeue (SimplestQueue []) = (Nothing, SimplestQueue [])
    dequeue (SimplestQueue q) = (Just (last q), SimplestQueue (take (length q - 1) q))

    isEmpty (SimplestQueue []) = True
    isEmpty _ = False

data SimpleQueue a = SimpleQueue { front :: [a]
                                 , back :: [a]
                                 }

instance Queue SimpleQueue where
    empty = SimpleQueue { front = []
                        , back = []
                        }
    enqueue a (SimpleQueue { front = f, back = b }) = 
        SimpleQueue { front = a : f, back = b }
                    
    dequeue (SimpleQueue { front = f, back = (x:xs) } ) =
        (Just x, SimpleQueue { front = f, back = xs } )

    dequeue (SimpleQueue { front = [], back = [] } ) =
        (Nothing, SimpleQueue { front = [], back = [] } )

    dequeue (SimpleQueue { front = f, back = [] } ) =
        dequeue (SimpleQueue { front = [], back = reverse f } )
        
                    
    isEmpty (SimpleQueue { front = [], back = []}) = True
    isEmpty _ = False

-- The structure of this function is an unfoldr
toList :: Queue q => q a -> [a]
toList q | isEmpty q = []
         | otherwise = let (Just element, q') = dequeue q in
                       element : toList q'

pop :: Queue q => q a -> q a
pop q = snd (dequeue q)

fromList :: Queue q => [a] -> q a
fromList = foldl (flip enqueue) empty

tail' :: [Char] -> Int -> [Char]
tail' text i = toList final_queue
 where set_up_queue = fromList first_part :: SimpleQueue Char
       first_part = take i text
       last_part = drop i text
       final_queue = let op queue nextElement = pop (enqueue nextElement queue) in
                     foldl op set_up_queue last_part

-- foldr enqueue empty (1:2:3:[])
-- -> enqeueu 1 (enqueue 2 (enqueue 3 empty))

-- enqueue characters, until we have i characters enqueued,
-- then for all other characters, enqueue one character, and dequeue one.

main = do args <- getArgs
          let i = read (head args)
          text <- getContents
          putStr (tail' text i)

-- main = getArgs >>= \args ->
--        let i = read (head args) in
--        getContents >>= \text ->
--        putStr (tail' text i)
