{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Bool

pingPongM :: IO ()
pingPongM = getLine >>= \s -> if s == "ping" then putStrLn "pong" else pure ()   

pingPongA :: IO ()
pingPongA = (\_ -> id) <$> getLine <*> putStrLn "pong"

whenS_IO :: IO Bool -> IO () -> IO ()
whenS_IO m a = m >>= \s -> if s then a else pure ()

pingPongS_IO :: IO ()
pingPongS_IO = whenS_IO ((== "ping") <$> getLine) (putStrLn "pong")

class Applicative f => Selective f where
  select :: f (Either a b) -> f (a -> b) -> f b

infixl 4 <*?
(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = select

selectM :: Monad m => m (Either a b) -> m (a -> b) -> m b
selectM eab f = eab >>= \case Left a  -> ($a) <$> f 
                              Right b -> pure b
                              
selectA :: Applicative f => f (Either a b) -> f (a -> b) -> f b                              
selectA eab f = (\e g -> either g id e) <$> eab <*> f

apS :: Selective f => f (a -> b) -> f a -> f b
apS f x = select (Left <$> f) (flip ($) <$> x)

whenS :: Selective f => f Bool -> f () -> f ()
whenS b x = selector <*? effect
  where
    selector = bool (Right ()) (Left ()) <$> b
    effect = const <$> x

unlessS :: Selective f => f Bool -> f () -> f ()    
unlessS b = whenS (not <$> b)

branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch eab ac bc = fmap (fmap Left) eab <*? fmap (fmap Right) ac <*? bc

ifS :: Selective f => f Bool -> f a -> f a -> f a
ifS b t e = branch (bool (Right ()) (Left ()) <$> b) (const <$> t) (const <$> e)

(<||>) :: Selective f => f Bool -> f Bool -> f Bool
l <||> r = ifS l (pure True) r

(<&&>) :: Selective f => f Bool -> f Bool -> f Bool
l <&&> r = ifS l r (pure False)

fromMaybeS :: Selective f => f a -> f (Maybe a) -> f a
fromMaybeS a ma = select (maybe (Left ()) Right <$> ma) (const <$> a)

anyS :: Selective f => (a -> f Bool) -> [a] -> f Bool
anyS p = foldr (\b fb -> p b <||> fb) (pure False)

allS :: Selective f => (a -> f Bool) -> [a] -> f Bool
allS p = foldr (\b fb -> p b <&&> fb) (pure True)

-- run a computation while it yields True
whileS :: Selective f => f Bool -> f ()
whileS b = whenS b (whileS b) 


-------------------

bindBool :: Selective f => f Bool -> (Bool -> f a) -> f a
bindBool b f = ifS b (f True) (f False)

-------------------

instance Selective IO where
  select = selectM

pingPongS :: IO ()
pingPongS = whenS ((== "ping") <$> getLine) (putStrLn "pong")
  
newtype Over m a = Over { getOver :: m }
  deriving (Eq, Show, Functor)

newtype Under m a = Under { getUnder :: m } 
  deriving (Eq, Show, Functor)

instance Monoid m => Applicative (Over m) where
  pure _ = Over mempty
  Over l <*> Over r = Over (l <> r)
instance Monoid m => Applicative (Under m) where
  pure _ = Under mempty
  Under l <*> Under r = Under (l <> r)

instance Monoid m => Selective (Over m) where
  select (Over l) (Over r) = Over (l <> r)

instance Monoid m => Selective (Under m) where
  select (Under l) _ = Under l  

data Validation e a = Failure e | Success a  
  deriving (Eq, Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success a   = Success (f a)
  Success _ <*> Failure e   = Failure e
  Failure e1 <*> Success _  = Failure e1
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)

instance Semigroup e => Selective (Validation e) where
  select (Success (Right b)) _ = Success b
  select (Success (Left a)) f  = ($a) <$> f
  select (Failure e) _         = Failure e

type Radius = Word
type Width = Word
type Height = Word

data Shape = Circle Radius | Rectangle Width Height

shape :: Selective f => f Bool -> f Radius -> f Width -> f Height -> f Shape
shape b r w h = ifS b (Circle <$> r) (Rectangle <$> w <*> h)

twoShapes :: Selective f => f Shape -> f Shape -> f (Shape, Shape)
twoShapes s1 s2 = (,) <$> s1 <*> s2

----------------------


newtype Task k v = Task { run :: forall f. Selective f => (k -> f v) -> f v }

dependenciesOver :: Task k v -> [k]
dependenciesOver task = getOver $ run task (\k -> Over [k])

dependenciesUnder :: Task k v -> [k]
dependenciesUnder task = getUnder $ run task (\k -> Under [k])

type Script k v = k -> Maybe (Task k v)

script :: Script FilePath String
script "release.tar" = Just $ Task $ \fetch -> tar [fetch "LICENSE", fetch "EXE"]
script "exe" = Just $ Task $ \fetch ->
  let src   = fetch "src.ml" 
      cfg   = fetch "config" 
      libc  = fetch "lib.c" 
      libml = fetch "lib.ml"
  in compile [src, ifS (parse cfg) libc libml]
script _ = Nothing  

tar :: Selective f => [f FilePath] -> f FilePath
tar = undefined

compile :: Selective f => [f FilePath] -> f String
compile = undefined

parse :: Selective f => f FilePath -> f Bool
parse = undefined