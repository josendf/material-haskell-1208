{-- lesson-01.hs 

> ./lesson-01.sh
> ./lesson-01 1

http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/First_Steps

--}

{-- 
    Every Haskell program begins with an action called main in 
    a module named Main.
    That module may import others, but it must be present for the compiler 
    to generate an executable file.
    
    Haskell is case-sensitive: module names are always capitalized, 
    definitions always uncapitalized.
--}

module Main where
import System.Environment


{--

The IO type is an instance of something called a monad.

Basically, a monad is a way of saying "we'll be carrying along and combining,
in some specific manner, values with some extra information attached to them,
which most functions don't need to worry about".

How we carry along this extra information and combine our values is what
makes the particular Monad type to be what it is; underlying values might
get changed and converted from one type into another by regular 
functions (invoked by actions), oblivious to the extra stuff that goes on 
around them, but the "pipe" (the value-propagation mechanism) stays the same.

How will we define our main action? We know that it must be an IO () action, 
which we want to read the command line args and print some output, 
producing (), or nothing of value, eventually.

There are two ways to create an IO action (either directly or by calling 
a function that performs them):
    Lift an ordinary value into the IO monad, using the return function.
    Combine two existing IO actions.
    
Since we want to do two things, we'll take the second approach.

The built-in action getArgs reads the command-line arguments and passes 
them along as a list of strings.

The built-in function putStrLn takes a string and creates an action
that writes this string to the console.

To combine these actions, we use a do-block.

A do-block consists of a series of lines, all lined up with the first
non-whitespace character after the do.

Each line can have one of two forms:
    name <- action1
    action2
--}

{-- ghci
> :m +System.Environment
> :t getArgs
getArgs :: IO [String]
--}

{--

A full table of the standard operators and their precedences

Operator(s) Precedence  Associativity   Description
----------- ----------  -------------   ------------------------------
.           9           Right           Function composition
!!          9           Left            List indexing
^, ^^, **   8           Right           Exponentiation
*, /        7           Left            Multiplication, Division
+, -        6           Left            Addition, Subtraction
:           5           Right           Cons (list construction)
++          5           Right           List Concatenation
`elem`      4           Left            List Membership
`notElem`
==, /=      4           Left            Equals, Not-equals
<, <=, >=,> 4           Left            Relation operators
&&          3           Right           Logical And
||          2           Right           Logical Or
>>          1           Left            Monadic Bind
>>, >>=     1           Left            Monadic Bind piping value to next function
=<<         1           Right           Reverse Monadic Bind
$           0           Right           Infix Function Application right-associative

--}

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)