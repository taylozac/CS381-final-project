# CS381-final-project
Final project for CS381

Team Members:
Vince Bochsler - bochslev
Zachary Taylor - taylorzac
Thomas Tonini  - toninit

Introduction to our Language:
Our language, Elevation is a stack based language. We utilize core commands such as push, pop, add, mul, equ and if else statements. Outside of the core commands Elevation also utilizes commands such as swap, dup and drop that are outside of the core commands but add to the functionality of the language. 

How to execute:

In the terminal, 

    -> run GHCI 
        -> Elevation is only module that needs to be loaded.
        
    -> run ex1
        -> Runs an addition program 
        -> Expected output
        -> Push (TheInt 3), Push (TheInt 4), Add, Push (TheInt 8), Equ]
    
     -> run ex2
        -> Executes for loop
        -> Expected output
        -> [Push (TheInt 0),Push (TheInt 0),Equ,IfElse [Push (TheInt 5),Push (TheInt 5),Push (TheInt 7),Mul,Add] [Push
        (TheInt 5),Push (TheInt 5),Push (TheInt 7),Mul,Add,Push (TheInt 0),Push (TheInt (-1)),Equ,IfElse [Push (TheInt
        5),Push (TheInt 5),Push (TheInt 7),Mul,Add] [Push (TheInt 5),Push (TheInt 5),Push (TheInt 7),Mul,Add]]]
