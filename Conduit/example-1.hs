{-# LANGUAGE  OverloadedStrings #-}

{-
http://www.yesodweb.com/book/conduits
https://github.com/snoyberg/conduit
-}

module Main where

import Data.Conduit
import qualified Data.Conduit.List as CL


{-
Source
A producer of data.
The data could be in a file, coming from a socket, or in memory as
a list. To access this data, we pull from the source.

Sink
A consumer of data.
Basic examples would be a sum function (adding up a stream of numbers fed in),
a file sink (which writes all incoming bytes to a file),
or a socket. We push data into a sink.
When the sink finishes processing , it returns some value.

Conduit
A transformer of data.
The simplest example is a map function, though there are many
others.
Like a sink, we push data into a conduit. But instead of returning a
single value at the end, a conduit can return multiple outputs every
time it is pushed to.

Fuse
A conduit can be fused with a source to produce a new, modified source
(the $= operator).
For example, you could have a source that reads bytes from a file,
and a conduit that decodes bytes into text.
If you fuse them together, you would now have a source that reads text
from a file.
Likewise, a conduit and a sink can fuse into a new sink (=$), and two
conduits can fuse into a new conduit (=$=).

Connect
You can connect a source to a sink using the $$ operator.
Doing so will pull data from the source and push it to the
sink, until either the source or sink signals that they are "done."

-}


{-
Let's write a source. We'll use sourceState.
The state will contain the next two numbers in the sequence.
We also need to provide a pull function, which
will return the next number and update the state.

sourceStateSource :: Monad m
      => state      -- Initial state
      -> (state -> m (SourceStateResult state output)) -- Pull function
      -> Source m output

Construct a Source with some stateful functions.
This function addresses threading the state value for you.
-}

mySource :: Monad m => Source m Int
mySource = sourceState init pull
 where
   init = 0  -- initial state
   pull state = return $ StateOpen  (next state) state
   next state = state + 1

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    -- Take a single value from the stream, if available.
    -- You can connect a source to a sink using the $$ operator.
    v1 <- mySource $$ CL.head
    putStrLn $ show v1
    putStrLn ""

    -- Take some values from the stream and return as a list.
    -- You can connect a source to a sink using the $$ operator.
    v2 <- mySource $$ CL.take 10
    putStrLn $ show v2
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""
