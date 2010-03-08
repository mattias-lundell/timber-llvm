module Match where

import POSIX

f :: String -> Int
f cs = case cs of
        "Ab" -> 0
        'b':xs -> 1
        "C" -> 2
        'C':_ -> 4
        _ -> 3

x = head "abc"

p 1.5 = 'a'
p 3.2 = 'b'
p _ = 'c'

root :: RootType
root w = do
    env = new posix w
    env.stdout.write ("f \"Ab\"="++show (f "Ab") ++"\n")
    env.stdout.write ("f \"Ac\"="++show (f "Ac") ++"\n")
    env.stdout.write ("f \"C\"="++show (f "C") ++"\n")
    env.stdout.write ("f \"bxx\"="++show (f "bxx") ++"\n")
    env.stdout.write ("f \"CC\"="++show (f "CC") ++"\n")

    env.stdout.write ("p 3.2 ="++show (p 3.2) ++"\n")
    env.stdout.write ("p 1.5 ="++show (p 1.5) ++"\n")
    env.stdout.write ("p 4.5 ="++show (p 4.5) ++"\n")

    env.stdout.write ("x ="++show x++"\n")

    env.exit 0