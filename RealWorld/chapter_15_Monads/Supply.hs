{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


{- GeneralizedNewtypeDeriving

Usually, we can only automatically derive instances of a handful of standard 
typeclasses, such as Show and Eq. As its name suggests, the 
GeneralizedNewtypeDeriving extension broadens our ability to derive typeclass 
instances, and it is specific to newtype declarations. If the type we're 
wrapping is an instance of any typeclass, the extensions can automatically make 
our new type an instance of that typeclass as follows.
-}

{- MultiParamTypeClasses

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)

How should we read the snippet MonadSupply s m in the typeclass? If we add 
parentheses, an equivalent expression is (MonadSupply s) m, which is a little 
clearer. In other words, given some type variable m that is a Monad, we can make 
it an instance of the typeclass MonadSupply s. unlike a regular typeclass, this 
one has a parameter. 3 comments

As this language extension allows a typeclass to have more than one parameter, 
its name is MultiParamTypeClasses. The parameter s serves the same purpose as 
the Supply type's parameter of the same name: it represents the type of the 
values handed out by the next function.

-}

{- FunctionalDependencies

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
    
The purpose behind us declaring a relationship is to help the type checker. 
Recall that a Haskell type checker is essentially a theorem prover, and that it 
is conservative in how it operates: it insists that its proofs must terminate. A 
non-terminating proof results in the compiler either giving up or getting stuck 
in an infinite loop. 3 comments

With our functional dependency, we are telling the type checker that every time 
it sees some monad m being used in the context of a MonadSupply s, the type s is 
the only acceptable type to use with it. If we were to omit the functional 
dependency, the type checker would simply give up with an error message.

-}

{- FlexibleInstances 

The FlexibleInstances extension is necessary so that the compiler will accept 
our instance declaration. This extension relaxes the normal rules for writing 
instances in some circumstances, in a way that still lets the compiler's type 
checker guarantee that it will terminate. Our need for FlexibleInstances here is 
caused by our use of functional dependencies, but the details are unfortunately 
beyond the scope of this book.

-}

{-

To hide our plumbing, in our module declaration we only export the type 
constructor, the execution function, and the next action.

Since a module that imports the library can't see the internals of the monad, it 
can't manipulate them.

Our plumbing is exceedingly simple: we use a newtype declaration to wrap the 
existing State monad.

Our use of newtype for the Supply type and our module header join forces to 
prevent our clients from using the State monad's get and set actions. Because 
our module does not export the S data constructor, clients have no programmatic 
way to see that we're wrapping the State monad, or to access it.

-}

module Supply
    (
      MonadSupply(..)
    , Supply
    , nextSup
    , runSupply
    ) where


import Control.Monad.State

newtype Supply s a = S (State [s] a)
    deriving (Monad) -- GeneralizedNewtypeDeriving

{-

At this point, we've got a type, Supply, that we need to make an instance of the 
Monad type class. We could follow the usual pattern of defining (>>=) and 
return, but this would be pure boilerplate code. All we'd be doing is wrapping 
and unwrapping the State monad's versions of (>>=) and return using our S value 
constructor. Here is how such code would look.

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Monad (Supply s) where
    s >>= m = S (unwrapS s >>= unwrapS . m)
    return = S . return

with GeneralizedNewtypeDeriving we can eliminate the boilerplate code

newtype Supply s a = S (State [s] a)
deriving (Monad) -- GeneralizedNewtypeDeriving

This takes the underlying type's implementations of (>>=) and return, adds the 
necessary wrapping and unwrapping with our S data constructor, and uses the new 
versions of those functions to derive a Monad instance for us.

What we gain here is very useful beyond just this example. We can use newtype to 
wrap any underlying type; we selectively expose only those typeclass instances 
that we want; and we expend almost no effort to create these narrower, more 
specialised types.

-}


{-

To widen our scope, we'll move beyond random numbers, and implement a monad that 
supplies unique values of any kind. The name we'll give to our monad is Supply. 
We'll provide the execution function, runSupply, with a list of values; it will 
be up to us to ensure that each one is unique.

-}
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs


{-

Within the monad, every time a consumer asks for a value, the next action will 
take the next one from the list and give it to the consumer. Each value is 
wrapped in a Maybe constructor in case the list isn't long enough to satisfy the 
demand.

-}
nextSup :: Supply s (Maybe s)
nextSup = S $ do 
                st <- get
                case st of
                    [] -> return Nothing
                    (x:xs) -> do put xs
                                 return (Just x)

{-

Another important way to make code more modular involves separating its 
interface—what the code can do—from its implementation—how it does it.

We will separate the actions we can perform with the monad from how it works 
using a typeclass.

This typeclass defines the interface that any supply monad must implement. It 
bears careful inspection, since it uses several unfamiliar Haskell language 
extensions. We will cover each one in the sections that follow.

-}

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
    
instance MonadSupply s (Supply s) where
    next = nextSup