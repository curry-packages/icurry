module Control.Monad.State where 

data State s a =  State (s -> (a, s))

{- 
instance Applicative (State s) where
        pure x = State (\s -> (x,s))
        (State ff) <*> (State g) =  State (\s -> let (f,s1) = ff s
                                                     (a,s2) = g s1
                                                     result = (f a,s2)
                                                 in  result)

-}
instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a,s1) = g s in (f a,s1)


instance Monad (State s) where
  return x = state (\s -> (x, s))
  m >>= f = state (\s -> let (x, s') = runState m s
                         in runState (f x) s')



runState :: State s a ->  (s -> (a,s))
runState (State st)  = st  

state :: (s -> (a, s)) -> State s a
state = State

get :: State s s
get = state (\s -> (s, s))

put :: s -> State s ()
put s = state (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = state (\s -> ((), f s))

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)
