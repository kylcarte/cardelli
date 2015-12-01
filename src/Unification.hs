{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unification where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Except
import Data.Monoid
import Data.STRef
import qualified Data.Foldable as F

data Exp f a
  = MutVar (Var f a)
  | GenVar Int
  | Op (f (Exp f a))

-- Unify {{{

newtype Unify s a = Unify
  { unUnify :: ExceptT String (ST s) a
  } deriving
  ( Functor , Applicative , Monad
  , Alternative , MonadPlus
  )

liftST :: ST s a -> Unify s a
liftST = Unify . lift

instance MonadRef (STRef s) (Unify s) where
  newRef     = liftST . newRef
  readRef    = liftST . readRef
  writeRef r = liftST . writeRef r

-- }}}

-- Unification {{{

type Var f a = STRef a (Maybe (Exp f a))

unify :: Unifiable f => Exp f a -> Exp f a -> Unify a ()
unify r s = (,) <$> prune r <*> prune s >>= \case
  (MutVar x,u@(MutVar y)) -> unless (x == y) $ x ?~ u
  (MutVar x,u       )     -> occurs x u >>= throw "occurs check failed." ? (x ?~ u)
  (t       ,MutVar y)     -> occurs y t >>= throw "occurs check failed." ? (y ?~ t)
  (GenVar x,GenVar y)     -> when (x == y) $ throw "different GenVars"
  (Op t,Op u) -> unifyWith unify t u >>= \case
    Just v                -> return $ F.fold v
    _                     -> throw "subunification error"
  _                       -> throw "different types"

prune :: (MonadRef (STRef a) m, Functor f) => Exp f a -> m (Exp f a)
prune e = case e of
  MutVar r -> readRef r >>= \case
    Just t -> do
      u <- prune t
      r ?~ u
      return u
    _ -> return e
  _ -> return e

occurs :: (MonadRef (STRef a) m, Functor f, Foldable f) => Var f a -> Exp f a -> m Bool
occurs r = prune >=> \case
  MutVar s -> return $ r == s
  GenVar _ -> return False
  Op t     -> getAny <$> foldMapM (fmap Any . occurs r) t

instantiate :: Functor f => [Exp f a] -> Exp f a -> Exp f a
instantiate ts x = case x of
  MutVar _ -> x
  GenVar n -> ts !! n
  Op t -> Op $ instantiate ts <$> t

okay :: Unify s ()
okay = pure ()

throw :: String -> Unify s ()
throw = Unify . throwError

-- }}}

-- MonadRef {{{

class Monad m => MonadRef r m | m -> r where
  newRef    :: a -> m (r a)
  readRef   :: r a -> m a
  writeRef  :: r a -> a -> m ()
  modifyRef :: r a -> (a -> a) -> m ()
  modifyRef r f = do
    a <- readRef r
    writeRef r $ f a

instance MonadRef (STRef s) (ST s) where
  newRef   = newSTRef
  readRef  = readSTRef
  writeRef = writeSTRef

(?~) :: MonadRef r m => r (Maybe a) -> a -> m ()
(?~) x = writeRef x . Just
infixr 5 ?~

(.~) :: MonadRef r m => r a -> a -> m ()
(.~) = writeRef
infixr 5 .~

ref :: MonadRef r m => r a -> m a
ref = readRef

-- }}}

-- Unifiable {{{

class Traversable t => Unifiable t where
  unifyWith :: Applicative f => (a -> b -> f c) -> t a -> t b -> f (Maybe (t c))

instance Unifiable [] where
  unifyWith f = \case
    a:as -> \case
      b:bs -> fmap . (:) <$> f a b <*> unifyWith f as bs
      _    -> bad
    [] -> \case
      []    -> good []
      _     -> bad

instance Unifiable Maybe where
  unifyWith f = \case
    Just a -> \case
      Just b -> Just . Just <$> f a b
      _      -> bad
    Nothing -> \case
      Nothing -> good Nothing
      _       -> bad

instance Eq e => Unifiable (Either e) where
  unifyWith f = \case
    Left x -> \case
      Left y | x == y -> good $ Left x
      _               -> bad
    Right a -> \case
      Right b         -> Just . Right <$> f a b
      _               -> bad

instance Eq e => Unifiable ((,) e) where
  unifyWith f (x,a) (y,b) = if x == y
    then Just . (,) x <$> f a b
    else bad

good :: Applicative f => a -> f (Maybe a)
good = pure . pure

bad :: Applicative f => f (Maybe a)
bad = pure empty

-- }}}

-- Util {{{

(?) :: a -> a -> Bool -> a
(?) t f b = if b then t else f
infixl 3 ?

foldMapM :: (Monad f, Foldable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapM f = F.foldrM (\a m -> mappend <$> f a <*> pure m) mempty

-- }}}

