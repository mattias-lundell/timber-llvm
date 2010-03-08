module ARM where

type RootType = Env -> TFT -> Class Prog

type Prog = Action

type Addr = Int

struct Env where
    debug       :: String -> Request ()
    portwrite   :: Addr -> BITS32 -> Request ()
    portset     :: Addr -> BITS32 -> Request ()
    portclear   :: Addr -> BITS32 -> Request ()
    portread    :: Addr -> Request BITS32
    install     :: Int -> Cmd () a -> Request () 
    netmemread  :: Addr -> Int -> Request (Array BITS32)
    netmemwrite :: Array BITS32 -> Addr -> Int -> Request ()

struct TFT where
    drawchar    :: Char -> Int{-x-} -> Int{-y-} -> Request Int
    drawbox     :: Int{-color-} -> Int{-x-} -> Int{-y-} -> Int{-w-} -> Int{-h-} -> Request ()
    xsize       :: Int
    ysize       :: Int
    charwidth   :: Int
    charheight  :: Int

