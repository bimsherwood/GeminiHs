module Exception (
  catch,
  Exception(..),
  TryOutcome(..),
  (?$),
  ($?),
  (??),
  (<<?)) where

import Control.Applicative ()

data TryOutcome e a = Failure e | Success a
  deriving Show

class (Applicative f, Functor f) => Exception f where
  succeed :: a -> f a
  try :: f a -> TryOutcome (f b) a

instance Exception (Either a) where
  
  succeed = Right
  
  try (Right x) = Success x
  try (Left e)  = Failure (Left e)

instance Exception (Maybe) where

  succeed = Just

  try (Just x) = Success x
  try Nothing  = Failure Nothing

catch :: Exception f => (f a -> a) -> f a -> a
catch onFail failable =
  case try failable of
    Success a -> a
    Failure e -> onFail e

-- Continue a failable computation with a failable value
infixr 1 ??
(??) :: Exception e => e (a -> b) -> e a -> e b
(??) = (<*>)

-- Continue a failable computation
infixr 1 ?$
(?$) :: Exception e => e (a -> b) -> a -> e b
(?$) ef x = ef ?? succeed x

-- Continue a function with a failable value
infixl 4 $?
($?) :: Exception e => (a -> b) -> e a -> e b
($?) = (<$>)

-- Continue a failable computation inside a Monad
infixr 1 <<?
(<<?) :: (Monad m, Exception e) => (a -> m (e b)) -> e a -> m (e b)
(<<?) continuation tryable =
  case try tryable of
    Success x -> continuation x
    Failure e -> return e
