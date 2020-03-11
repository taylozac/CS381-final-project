

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




-- define the possible types
data StackType = TtheInt
               | TtheBool
               | TtheString
               | TtheFunc
               | TtheError
    deriving (Eq,Show)

type TheTypeStack = [Either StackType PlaceHolder]

type TypeDomain = TheTypeStack -> Maybe TheTypeStack

-- produce the type of a program (not a command, you can type check a command but you must provide a built stack for that, so just type check entire program!), or error if its bogus
-- should I use \lambda, by using domain or just include stack in parameter, not even fully sure of the distinction.
-- seems we will have to run a type emulation at the least, in order to statically type check.



-- 2133, code I have so far, touching up needs to be done and integration of this into the main code base but it should paint a picture of what is going to happen:
-- the program will run, using only types and just throw an error if an operation takes place on incorrect types, it doesn't "add" anything it just ensures
-- the types being added make sense (support strings eventually?), this type checking will occure before the semantics so semantics are safe to compute without checking.
getType :: CoreCmd -> TypeDomain
getTpe TtheError = []
getType (Push t)     = \s -> case t of
                          (TheInt ii)    -> Just (Left TtheInt : s)
                          (TheBool bb)   -> Just (Left TtheBool : s)
                          (TheString ss) -> Just (Left TtheString : s)
                          (TheFunc pp)   -> Just (Left TtheFunc : s)
--                          _              -> Just (Left TtheError : s)

getType Add          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TtheInt) -> case j of 
                                                                     (TtheInt) -> Just (Left TtheInt : s')
                                                                     _         -> Just (Left TtheError : s')
                                                       _         -> Just (Left TtheError : s')
getType Mul          = \s -> case s of
                           (Left i : Left j : s') -> case i of
                                                       (TtheInt) -> case j of 
                                                                     (TtheInt) -> Just (Left TtheInt : s')
                                                                     _         -> Just (Left TtheError : s')
                                                       _         -> Just (Left TtheError : s')
getType Equ          = \s -> case s of
                           (Left i : Left j : s') -> Just (Left TtheBool : s')
                           _                        -> Just (Left TtheError : s)
                           
getType (IfElse t e) = \s -> case s of  
                           (Left i : Left j : s') -> case i of
                                                       (TtheBool) -> case j of 
                                                                     (TtheBool) -> Just s'
                                                                     _         -> Just (Left TtheError : s')
                                                       _         -> Just (Left TtheError : s')




-- 2. Write the following StackLang program as a Haskell value:
--
--   add 3 to 4, then see if 7 == 8  change and see it work!
--
ex1 :: Prog
ex1 = [Push (TheInt 3), Push (TheInt 4), Add, Push (TheInt 7), Equ]


-- Concatenating Strings on stack
concatString :: String -> String -> Prog
concatString s1 s2 = [
                      Push (TheString s1),
                      Add,
                      Push (TheString s2),
                      Add
                      ] 

string_push :: Prog
string_push = concatString "Hello" "World"


type PlaceHolder = Int

-- Stacktype includes prog, so we don't need either here! Maybe make the Right side a stack itself, so stacks can be passed around for powerful features?
-- right was CoreCmd
type TheStack = [Either StackValue PlaceHolder]
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
