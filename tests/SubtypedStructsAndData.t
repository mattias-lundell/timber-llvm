module SubtypedStructsAndData where

struct WormA where
    wormA :: Int
    
struct WormB < WormA where
    wormB :: Int
    
struct WormC < WormA where
    wormC :: Int

struct WormD < WormB, WormC where
    wormD :: Int

struct Apa < WormD where
    apa :: Int

struct Bepa < Apa where
    bepa :: Int
    
struct Cepa < Apa where
    cepa :: Int

struct Depa < Bepa,Cepa where
    dada :: Int

zzz = { apa = 1, bepa = 2, cepa = 3, dada = 4, wormC = 0, wormA = 0, wormB = 0, wormD = 0 }


data A = A

data B > A = B

data C > A = C

data D > B,C = D

fff A = 'a'
fff B = 'b'
--fff C = 'c'
fff D = 'd'
fff _ = 'x'
