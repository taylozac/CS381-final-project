module StackLoops where

import Elevation

--
-- This for loop works in the following way:
--     (1) Push '0' and the current index onto the stack.
--     (3) Check if they are equal.
--     (4) If they are equal, run the loop again with the index decremented by one.
--         Else, run the last copy of the program.
--
for :: Int -> Prog -> Prog
for i p = [
           Push (TheInt 0),
           Push (TheInt (i - 1) ),
           Equ,
           IfElse p ( p ++ (for (i - 1) p) )
          ]


--
-- This program pushes the value one onto the stack five times.
--
multi_push :: Prog
multi_push = for 5 [Push (TheInt 1)]
