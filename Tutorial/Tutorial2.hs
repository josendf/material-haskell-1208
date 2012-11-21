{-
  Tutorial1
-}
module Main where
import Library

main :: IO()
main = do 
  putStrLn "\n<Inicio>"
  putStrLn "<ejemplo1>"
  ejemplo1
  putStrLn "\n<ejemplo2>"
  ejemplo2
  putStrLn "\n<ejemplo3>"
  ejemplo3
  putStrLn "\n<ejemplo4>"
  ejemplo4
  putStrLn "\n<ejemplo5>"
  ejemplo5
  putStrLn "\n<ejemplo6>"
  ejemplo6
  putStrLn "\n<ejemplo7>"
  ejemplo7
  putStrLn "\n<ejemplo8>"
  ejemplo8
  putStrLn "\n<Fin>"


{- ejemplo1
  The type constructor IO is an instance of the Monad class.
  The two monadic binding functions, methods in the Monad class, are used 
  to compose a series of I/O operations.
  
  The >> function is used where the result of the first operation is 
  uninteresting, for example when it is (). 
  
  The >>= operation passes the result of the first operation as an 
  argument to the second operation. -}
ejemplo1 :: IO()
ejemplo1 = readFile "Sample1.txt" >>= \ s ->
  putStr s

{- ejemplo2
  A do expression provides a more conventional syntax for monadic programming.
  It allows an expression such as
  
    putStr "x: " >>
    getLine >>= \l ->
    return (words l)
  
  to be written in a more traditional way as:
  
    do putStr "x: "
    l <- getLine
    return (words l) -}
ejemplo2 :: IO()
ejemplo2 = do
  s <- readFile "Sample1.txt"
  putStr s

{- ejemplo3
  Haskell provides a built-in function, lines, that lets us split a text string on line boundaries.
  While lines looks useful, it relies on us reading a file in "text mode" in order to work.
  When we read a file in text mode, the file I/O library translates the 
  line-ending sequence "\r\n" (carriage return followed by newline) to "\n", and it 
  does the reverse when we write a file. On Unixlike
  systems, text mode does not perform any translation. As a result of this difference, if
  we read a file on one platform that was written on the other, the line endings are likely to
  become a mess. (Both readFile and writeFile operate in text mode.) -}
ejemplo3 :: IO()
ejemplo3 = do
  s <- readFile "Sample1.txt"
  print (splitLines s)
 
ejemplo4 :: IO()
ejemplo4 = do
  s <- readFile "Sample1.txt"
  mapM_ putStr (splitLines s)
  
ejemplo5 :: IO()
ejemplo5 = do
  s <- readFile "Sample1.txt"
  let ls = splitLines s in
    mapM_ putStr ls

{-
  length Tells us how many elements are in a list.
  null   If you need to determine whether a list is empty.
  head   To access the first element of a list.
  last   Returns the very last element of a list.
  init   Returns a list of all but the last element of its input.
  tail   Returns all but the head of a list.
  concat Takes a list of lists, all of the same type, and concatenates them into single list.
  take   Returns a sublist consisting of the first k elements from a list.
  drop   Drops k elements from the start of the list.
  
  When we care only whether or not a list is empty, calling length isn't a good
  strategy. It can potentially do a lot more work than we want, if the list we're working with
  is finite. Since Haskell lets us easily create infinite lists, a careless use of length may even
  result in an infinite loop.
  A more appropriate function to call here instead is null, which runs in constant time. Better
  yet, using null makes our code indicate what property of the list we really care about.
-}
ejemplo6 :: IO()
ejemplo6 = do
  s <- readFile "Sample2.txt"
  let ls = splitLines s in do
    mapM_ putStr ["head ", (head ls), "\n"]
    mapM_ putStr ["last ", (last ls), "\n"]
    putStr "init "; print (init ls)
    putStr "tail "; print (tail ls)

{-
  splitAt   combines the functions take and drop, returning a pair of the input
            lists, split at the given index.
          
  takeWhile takes elements from the beginning of a list as long as the predicate returns True.
  dropWhile drops elements from the list as long as the predicate returns True.
          
  elem      indicates whether a value is present in a list.
  notElem   indicates whether a value is not present in a list.

  filter    takes a predicate and returns every element of the list on
            which the predicate succeeds.
          
-}
ejemplo7 :: IO()
ejemplo7 = do
  s <- readFile "Sample2.txt"
  let ls = splitLines s in do
    print (fst (splitAt 5 ls))
    print (takeWhile (\x -> (head x) == '0') ls)
    print (dropWhile (\x -> (head x) == '0') ls)
    print (elem "10" ls)
    print (notElem "XX" ls)
    print (filter (\x -> (head x) == '0')  ls)
    

{-
  foldl Takes a "step" function, an initial value for its accumulator, and a list.
        The "step" takes an accumulator and an element from the list and returns a new
        accumulator value. All foldl does is call the "stepper" on the current accumulator and an
        element of the list, and then passes the new accumulator value to itself recursively to
        consume the rest of the list.
          
-}
ejemplo8 :: IO()
ejemplo8 = do
  s <- readFile "Sample2.txt"
  let ls = splitLines s in do
    print (foldl step 0 (map asInt ls))
      where step acc x = acc + x
