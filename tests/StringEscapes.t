module StringEscapes where

import POSIX

root :: RootType
root w = do
    env = new posix w
    env.stdout.write ('\"' : '\'' : ";[~\o33\n' \"\\ \HTxy\x000dude"++"\n")
    env.exit 0