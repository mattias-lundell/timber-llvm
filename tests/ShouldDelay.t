module ShouldDelay where

import POSIX

struct A where
	methodA :: Action

struct ATypeExtended < A

struct B where
	methodB :: Action

struct BTypeExtended < B

objA b = class
	methodA = action
		a = 0
	result ATypeExtended {methodA = methodA}

objB a = class
	methodB = action
		b = 0
	result BTypeExtended {methodB = methodB}

-- This recursive instantiation should trigger the static delay rule rewrite, because of
-- the invisisble selection from oB caused by the subtyping coercion from BTypeExtended
-- to B.  Execution should print "OK" on stdout.
root :: RootType
root w = do
	oA = new objA oB
	oB = new objB oA
	env = new posix w
	env.stdout.write "OK\n"
	env.exit 0
