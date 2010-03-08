module PublicPrivateSubtyping where

    struct S1 where
        method1 :: Action

private
	
    struct S2 < S1 where
        method2 :: Action
