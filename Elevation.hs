

-- Elevation stack language

module Elevation where

import Prelude





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

data StackValue =      TheInt    Int
                     | TheBool   Bool
                     | TheString String
                     | TheFunc   Prog
  deriving (Eq,Show)

data CoreCmd = Push StackValue
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


-- need to use this code inside the main program.
-- This for loop works in the following way:
--     (1) Push '0' and the current index onto the stack.
--     (2) Check if they are equal.
--     (3) If they are equal, run the loop again with the index decremented by one.
--         Else, run the last copy of the program.
--
for :: Int -> Prog -> Prog
for i p = case (i < 0) of
               False ->  [Push (TheInt 0), Push (TheInt (i - 1)), Equ, IfElse p ( p ++ (for (i - 1) p) )]
               True  -> []



--
-- This program pushes the value one onto the stack five times.
--
multi_push :: Prog
multi_push = for 5 [Push (TheInt 1)]


-- define the possible types
data StackType = TtheInt
               | TtheBool
               | TtheString
               | TtheFunc
               | TtheError String
    deriving (Eq,Show)

type TheTypeStack = [Either StackType PlaceHolder]

type TypeDomain = TheTypeStack -> Maybe TheTypeStack

-- produce the type of a program (not a command, you can type check a command but you must provide a built stack for that, so just type check entire program!), or error if its bogus
-- should I use \lambda, by using domain or just include stack in parameter, not even fully sure of the distinction.
-- seems we will have to run a type emulation at the least, in order to statically type check.



-- this function goes through and executes the commands as normal except it doesn't compute anything just works with types to ensure type correctness. It modifies the type stack accordingly.
getType :: CoreCmd -> TypeDomain

getType (Push t)     = \s -> case t of
                          (TheInt    whatever)    -> Just (Left TtheInt : s)
                          (TheBool   doesnt)      -> Just (Left TtheBool : s)
                          (TheString even)        -> Just (Left TtheString : s)
                          (TheFunc   matter)      -> Just (Left TtheFunc : s)

getType Pop          = \s -> case s of
                           []          -> Just [Left (TtheError "pop error")]
                           (s' : ss')  -> Just ss'

getType Add          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TtheInt) -> case j of 
                                                                     (TtheInt) -> Just (Left TtheInt : s')
                                                                     _         -> Just [Left (TtheError "add int error")]
                                                       (TtheString) -> case j of
                                                                     (TtheString) -> Just(Left TtheString : s')
                                                                     _         -> Just [Left (TtheError "add string error")]

                                                       _         -> Just [Left (TtheError "add general error")]
getType Mul          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TtheInt) -> case j of 
                                                                     (TtheInt) -> Just (Left TtheInt : s')
                                                                     _         -> Just [Left (TtheError "mul error 1")]
                                                       _         -> Just [Left (TtheError "mul error 2")]
getType Equ          = \s -> case s of
                           (Left b1 : Left b2 : s') -> case b1 of
                                                       TtheBool   -> case b2 of
                                                                       TtheBool -> Just (Left TtheBool : s')
                                                                       _        -> Just [Left (TtheError "equ bool error 1")]
                                                       TtheInt    -> case b2 of
                                                                       TtheInt  -> Just (Left TtheBool : s')
                                                                       _        -> Just [Left (TtheError "equ int error 1")]
                                                       TtheString -> case b2 of
                                                                       TtheString  -> Just (Left TtheBool : s')
                                                                       _        -> Just [Left (TtheError "equ string error 1")]
                                                       _          -> Just [Left (TtheError "equ string error 2")]
                           
getType (IfElse t e) = \s -> case s of  
                               (Left boolVal : s') -> case boolVal of
                                                       TtheBool -> case (fst (progStaticEval t s'), fst (progStaticEval e s')) of
                                                                (True, True) -> Just s'
                                                                _            -> Just [Left (TtheError (snd (progStaticEval e s') ++ " " ++ snd (progStaticEval t s')))]
                               _          -> Just [Left (TtheError "ifElse type error")]


-- static evaluation of the program, its type correct or not: return a bool. This doesn't say if a program will return or compute bogus values, just that it is type correct.
-- what is this? just return nothing in getType instead of checking for Left TtheError.. was there a reason for this?
progStaticEval :: Prog -> TheTypeStack -> (Bool,String)
progStaticEval [] _ = (True,"end of prog.")
--progStaticEval _ [] = (True, "end of stack")
progStaticEval (c:cs) [] = case c of 
                              Push a     -> case (getType c []) of
                                                   Just (s') -> case s' of
                                                                       [Left (TtheError str)] -> (False,str)
                                                                       _                      -> progStaticEval cs (s')
                                                   _               -> (False, "waaddddddddddat")
                              Pop        -> (False, "empty stack pop")
                              Add        -> (False, "empty stack add") -- should we check that add has 2 arguments cause what if there is just 1?
                              Mul        -> (False, "empty stack mul")
                              Equ        -> (False, "empty stack equ")
                              IfElse a b -> (False, "empty stack ifel")
progStaticEval (c:cs) s = case (getType c s) of
                               Just (s') -> case s' of
                                                     [Left (TtheError str)] -> (False,str)
                                                     _                      -> progStaticEval cs (s')
                               _               -> (False, "waaat")

--
ex1 :: Prog
ex1 = for 1 [Push (TheInt 3), Push (TheInt 4), Add, Push (TheString "aaaa")]


-- this program adds 5 + (5*7), 10 times over and should produce 400
ex2 :: Prog
--ex2 = for 10 [Push (TheInt 5), Push (TheInt 5), Push (TheInt 7), Mul, Add, Add] this makes it to cmd, past the type check? need to see if stack has less than 2 values for add?
ex2 = (for 10 [Push (TheInt 5), Push (TheInt 5), Push (TheInt 7), Mul, Add]) ++ for 9 [Add]


ex3 :: Prog
ex3 = [Push (TheInt 5), Push (TheInt 5), Push (TheInt 5), Push (TheInt 5), Add, Add, Add]

ex4 :: Prog
ex4 = [Push (TheInt 5)]


exFailType1 :: Prog
exFailType1 = [Push (TheInt 3), Push (TheString "4"), Add, Push (TheInt 7), Equ]

--
-- Concatenating Strings on stack
--
concatString :: String -> String -> Prog
concatString s1 s2 = [
                      Push (TheString s2),
                      Push (TheString s1),
                      Add
                      ] 
--
-- Program to push two string on the stack and concatenate them
--
string_push :: Prog
string_push = concatString "Hello" "World"


type PlaceHolder = Int

-- Stacktype includes prog, so we don't need either here! Maybe make the Right side a stack itself, so stacks can be passed around for powerful features?
-- right was CoreCmd
type TheStack = [Either StackValue PlaceHolder]
--type Stack = [Either Int Bool]

type Domain = TheStack -> (Maybe StackValue, Maybe TheStack)


cmd :: CoreCmd -> Domain

cmd (Push t)     = \s -> (Nothing, Just (Left t : s))

cmd Pop          = \s -> case s of
                           (Left i : ss) -> (Just i, Just ss)
                           _             -> (Nothing, Just s)
    
-- add has to check types at runtime to use ++ or +
cmd Add          = \s -> case s of
                           (Left i : Left j : s') -> case (i,j) of
                                                       (TheString i', TheString j') -> (Nothing, Just (Left (TheString (i' ++ j')) : s'))
                                                       (TheInt i', TheInt j')       -> (Nothing, Just (Left (TheInt (i' + j')) : s'))                                                                        
cmd Mul          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TheInt i') -> case j of 
                                                                     (TheInt j') -> (Nothing, Just (Left (TheInt (i' * j')) : s'))
cmd Equ          = \s -> case s of
                           (Left i  : Left j  : s') -> (Nothing, Just (Left (TheBool (i == j)) : s'))
                           _ -> (Nothing, Nothing)
                           
cmd (IfElse t e) = \s -> case s of  
                           ((Left (TheBool True))  : s') -> prog t s'
                           ((Left (TheBool False)) : s') -> prog e s'
                           _ -> (Nothing, Nothing)





prog :: Prog -> Domain
prog []    = \s -> (Nothing, Just s)
prog (c:p) = \s -> case cmd c s of
                     (_, Just s') -> prog p s'
                     _ -> (Nothing, Nothing)


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
run :: Prog -> (Maybe StackValue, Maybe TheStack)
run p = case (progStaticEval p []) of
             (True,s)  -> prog p []
             (False,s) -> (Nothing, Just [Left (TheString s)])

