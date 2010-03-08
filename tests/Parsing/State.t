module State where

data State s a = SM (s -> (s,a))

runState :: State s a -> s -> (s,a)
runState (SM f) s = f s

get :: State s s
get = SM $ \s -> (s,s)

set :: s -> State s ()
set s = SM $ \_ -> (s,())

inState :: s -> State s a -> State s a
inState s m = 
  get >>= \s' ->
  set s >>
  m >>= \a ->
  set s' >>
  return a


instance functorState :: Functor (State s) = struct
  f $^ SM fa = SM $ \s -> let (s',a) = fa s in (s',f a)

instance applicativeState :: Applicative (State s) = Applicative {..} 
  where
  Functor {..} = functorState
  SM fs $* SM as = SM $ \s -> let (s',f) = fs s
                                  (s'',a) = as s'
                              in (s'',f a)
  pure a = SM $ \s -> (s,a)

instance monadState :: Monad (State s) = Monad {..} where
  SM m >>= f = SM $ \s -> let (s',a) = m s 
                              SM f' = f a
                          in f' s'
  return a = SM $ \s -> (s,a)
