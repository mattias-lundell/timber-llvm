module NoInteraction where

import POSIX

trivial f env = class
  result action
    env.stdout.write (f env.argv++"\n")
    env.exit 0
