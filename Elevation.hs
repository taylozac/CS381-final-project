

-- Elevation stack language

module Elevation where

import Prelude


-- * Syntax of StackLang
--

-- Grammar for StackLang:
-- 
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num                         push a number on the stack
--         |  bool                        push a boolean on the stack
--         |  `add`                       add the top two numbers on the stack
--         |  `mul`                       multiply the top two numbers on the stack
--         |  `equ`                       check whether the top two elements are equal
--         |  `if` prog `else` prog `end` if the value on the top is true
--                                        then run the first program, else run the second

-- 1. Encode the above grammar as a set of Haskell data types


-- I think this can technically only take CoreCmd because sugar is just Core anyways?
--type Prog = [Either CoreCmd SugarCmd]
type Prog = [CoreCmd]

data StackType = TheInt    Int
               | TheBool   Bool
               | TheString String
               | TheFunc   Prog
  deriving (Eq,Show)

data CoreCmd = Push StackType
             | Pop
             | Add
             | Mul
             | Equ
             | IfElse Prog Prog
  deriving (Eq,Show)


data SugarCmd = Swap
              | Dup
              | Drop
  deriving (Eq,Show)

-- 2. Write the following StackLang program as a Haskell value:
--
--   add 3 to 4, then see if 7 == 8  change and see it work!
--
ex1 :: Prog
ex1 = [Push (TheInt 3), Push (TheInt 4), Add, Push (TheInt 8), Equ]



-- 3. Write a StackLang program that:
--     * checks whether 4 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 equ if 5 6 add else false end
--
--ex2 :: Prog
--ex2 = [PushN 4, PushN 4, Equ, IfElse [PushN 5, PushN 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
--genAdd2 :: Int -> Int -> Prog
--genAdd2 x y = [PushN x, PushN y, Add, Add]
           -- [PushN x, Add, PushN y, Add]
           -- [PushN (x+y), Add]  -- doing as much as possible at the metalanguage level


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
--genSum :: [Int] -> Prog
--genSum []     = [PushN 0]
--genSum (x:xs) = genSum xs ++ [PushN x, Add]
  -- [PushN x] ++ genSum xs ++ [Add]  -- this one works but is memory inefficient
  -- genSum xs = [PushN (sum xs)] -- doing as much as possible at the metalanguage level



--
-- * Semantics of StackLang
--


-- 6. Identify/define a semantics domain for Cmd and for Prog.
--
--    Things we need:
--      * stack
--        * int
--        * bool
--      * error

-- 
--type TheStack = [Either (Int,Bool,String) (Prog)]
type TheStack = [Either StackType CoreCmd]
--type Stack = [Either Int Bool]

type Domain = TheStack -> Maybe TheStack


-- 7. Define the semantics of a StackLang command
cmd :: CoreCmd -> Domain
cmd (Push t)     = \s -> Just (Left t : s)
-- static type check will ensure add is only applied to Ints or Strings, (String concatenation is required), maybe implement adding an Int to a String "aa" + 4 == "aa aa aa aa aa" (without spaces)?
cmd Add          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TheInt i') -> case j of 
                                                                     (TheInt j') -> Just (Left (TheInt (i'+j')) : s')
--                                                                            _ -> Nothing
--                                                              _ -> Nothing
cmd Mul          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TheInt i') -> case j of 
                                                                     (TheInt j') -> Just (Left (TheInt (i'+j')) : s')
cmd Equ          = \s -> case s of
                           (Left i  : Left j  : s') -> Just (Left (TheBool (i == j)) : s')
                           _ -> Nothing
                           
cmd (IfElse t e) = \s -> case s of  
                           ((Left (TheBool True))  : s') -> prog t s'
                           ((Left (TheBool False)) : s') -> prog e s'
                           _ -> Nothing


-- 8. Define the semantics of a StackLang program.
prog :: Prog -> Domain
prog []    = \s -> Just s
prog (c:p) = \s -> case cmd c s of
                     Just s' -> prog p s'
                     _ -> Nothing


-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right False]
--
--   >>> run (genSum [1..10])
--   Just [Left 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
run :: Prog -> Maybe TheStack
run p = prog p []
