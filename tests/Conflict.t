module Conflict where


  -- import ConflictClient    -- illegal; recursive modules. Presently stupid error message (file locked).

  -- this takes priority over the Prelude definition
  length = 3

  -- Prelude def can be used qualified
  a = Prelude.length

private
  
  head = 5

