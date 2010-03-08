module MonoValueRec where

import POSIX

-- Should trigger the ill-founded recursion error when run, because the ill-founded recursive
-- binding of str shadows the previous one.  A previous bug in the monomorphism restriction 
-- implementation made the recursive binding polymorphic (and thus of C arity > 0), causing 
-- a run-time stack overflow instead of an immediate bus error due to selection from a 
-- placeholder address.
root :: RootType
root w = do
   env = new posix w
   str = "abc\n"
   env.stdout.write str
   str = tail str
   env.stdout.write str
   env.exit 0