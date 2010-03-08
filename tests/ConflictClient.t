module ConflictClient where

import Conflict

a = length    -- Illegal: two clashing imports of name; none is chosen.
                -- Compiler gives a warning to highlight this.

a = Conflict.length + 1


