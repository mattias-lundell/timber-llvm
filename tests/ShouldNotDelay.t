module ShouldNotDelay where

-- Possible target for the delay rule rewrite, but only if the restriction to subtype 
-- coercion selections is lifted (see Termred).  For now, evaluation of this binding
-- group is ill-founded.
x = g "abc"
g = head (map id [\s -> head x : s])

struct Apa where
    kaka :: String

-- Possible target for the delay rule rewrite, but only if the restriction to subtype 
-- coercion selections is lifted (see Termred).  For now, evaluation of this binding
-- is ill-founded.
xxx = { kaka = xxx.kaka }

typeclass AApplicative m where
  rret :: m a

-- Should not trigger the delay rule rewrite becuse the rret selector is locally 
-- polymorphic, and hence a value of arity > 0.
instance aapp :: AApplicative Maybe = AApplicative {..}
  where rret = aapp.rret

