module Observer where

import Data.Functional.List

updateAll a [] = do
updateAll a (o : os) = do o.update a
                          updateAll a os

struct Object where
  self :: OID

struct Observer a < Object where
  update :: a -> Action

instance eqObserver :: Eq (Observer a) where
  o1 == o2 = o1.self == o2.self
  o1 /= o2 = o1.self /= o2.self

struct Observable a where
  subscribe   :: Observer a -> Action
  unsubscribe :: Observer a -> Action
  
struct Notifier a where
  setChanged :: Action
  notify :: a -> Action

struct Publisher a < Observable a, Notifier a

publisher :: Class (Publisher a)
publisher = class

   olst := []
   changed := False

   subscribe obs = action
      olst := nub (obs : olst)

   unsubscribe obs = action
      olst := delete obs olst

   setChanged = action 
      changed := True

   notify a = action
      if changed then
         updateAll a olst
         changed := False

   result Publisher {..}


struct Counter where
   incr  :: Action
   read  :: Request Int
   reset :: Action

struct ObservableCounter < Observable Int, Counter

obsCounter :: Class ObservableCounter
obsCounter = class

   val := 0
   Publisher {..} = new publisher

   set v  = do 
      val := v
      setChanged
      notify v

   incr  = action set (val + 1)

   reset = action set 0

   read = request
      result val

   result ObservableCounter {..}
