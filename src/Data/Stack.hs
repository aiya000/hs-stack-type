-- | The basic stack type
module Data.Stack
  ( Stack
  , push
  , pop
  , runStack
  , execStack
  , evalStack
  , stack
  , modifyStack
  ) where

import Control.Monad.Trans.State.Lazy (State, get, put, runState, state, modify)

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

execStack :: Stack s a -> [s] -> [s]
execStack = (snd .) . runState

evalStack :: Stack s a -> [s] -> a
evalStack = (fst .) . runState

stack :: ([s] -> (a, [s])) -> Stack s a
stack = state

modifyStack :: ([s] -> [s]) -> Stack s ()
modifyStack = modify
