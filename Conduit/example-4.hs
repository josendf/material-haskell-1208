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

mySource :: Monad m => Int -> Source m Int
mySource count = sourceState init pull
 where
   init = 0  -- initial state

   pull state = return $ if state >= count then StateClosed
                         else StateOpen  (next state) state

   next state = state + 1

myConduit :: Monad m => Int -> Int -> Conduit Int m Int
myConduit init count = conduitState init push close
  where
    push state input  = return $ if state >= count then done state input
                                 else prod state input
    prod state input  = StateProducing (next state) [trnsf state input]
    done state input  = StateFinished (Just input) []
    trnsf state input = input + state
    next state        = state + 1
    close state       = return []

{-- We can code our sink with the sinkState function.
This function takes three parameters: an initial state,
a push function (receive some more data), and a close function.

sinkState :: Monad m
  => state -- initial state
  -- Push
  -> (state -> input -> m (SinkStateResult state input output))
  -- Close. Note that the state is not returned, as it is not needed.
  -> (state -> m output)
  -> Sink input m output

Construct a Sink with some stateful functions.
This function addresses threading the state value for you.
--}
mySink :: Monad m => Int -> Sink Int m Int
mySink count = sinkState init push close
  where
    init             = 0
    push state input = return $ if state >= count then done state input
                                else proc state
    proc state       = StateProcessing (next state)
    done state input = StateDone (Just input) state
    next state       = state + 1
    close state      = return state

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Start..."
    putStrLn ""

    -- Apply a transformation to all values in a stream.
    -- You can connect a source to a sink using the ($$) operator.
    -- ($=) Left fuse, combining a source and a conduit
    -- together into a new source.
    -- (=$) Right fuse, combining a conduit and
    -- a sink together into a new sink.
    v1 <- mySource 10 $$ myConduit 0 5 =$ CL.consume
    putStrLn $ "v1: " ++ show v1
    putStrLn ""

    v2 <- mySource 10 $$ myConduit 0 5 =$ mySink 2
    putStrLn $ "v2: " ++ show v2
    putStrLn ""

    putStrLn ""
    putStrLn "Done."
    putStrLn ""
