{-
	Tutorial1
-}
module Main where
import Library

main::IO()
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
	putStrLn "\n<Fin>"

-- ejemplo1
ejemplo1::IO()
ejemplo1 = putStr "Hello Haskell"

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
ejemplo2::IO()
ejemplo2 = do
	putStr "Hello"
	putStr " "
	putStr "Haskell"

{- ejemplo3
	The type constructor IO is an instance of the Monad class.
	The two monadic binding functions, methods in the Monad class, are used 
	to compose a series of I/O operations.
	
	The >> function is used where the result of the first operation is 
	uninteresting, for example when it is (). 
	
	The >>= operation passes the result of the first operation as an 
	argument to the second operation. -}
ejemplo3::IO()
ejemplo3 = 
	putStr "Hello" >>
	putStr " "     >>
	putStr "Haskell"


{- ejemplo4
	The Prelude provides the following auxiliary functions over Monads:
	sequence  :: Monad m => [m a] -> m [a]
	sequence_ :: Monad m => [m a] -> m ()
	mapM      :: Monad m => (a -> m b) -> [a] -> m [b]
	mapM_     :: Monad m => (a -> m b) -> [a] -> m ()
	(=<<)     :: Monad m => (a -> m b) -> m a -> m b -}
ejemplo4::IO()
ejemplo4 = sequence_ [putStr "Hello", putStr " ", putStr "Haskell"]
	