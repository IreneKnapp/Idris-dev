module Control.Monad.Identity

import Data.Simple.Functor
import Data.Simple.Applicative
import Data.Simple.Monad 

public record Identity : Type -> Type where
    Id : (runIdentity : a) -> Identity a

instance Functor Identity where
    fmap fn (Id a) = Id (fn a)

instance Applicative Identity where
    pure x = Id x
    
    (Id f) <$> (Id g) = Id (f g)

instance Monad Identity where
    return x = Id x
    (Id x) >>= k = k x
