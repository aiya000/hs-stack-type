-- | The basic stack type
module Data.Stack
  ( Stack
  , push
  , pop
  , runStack
  , stack
  ) where

import Control.Monad.Trans.State.Lazy (State, get, put, runState, state)

type Stack s a = State [s] a


push :: s -> Stack s ()
push x = do
  xs <- get
  put $ x:xs
  return ()

pop :: Stack s s
pop = do
  (x:xs) <- get
  put xs
  return x

runStack :: Stack s a -> [s] -> (a, [s])
runStack = runState

stack :: ([s] -> (a, [s])) -> Stack s a
stack = state
