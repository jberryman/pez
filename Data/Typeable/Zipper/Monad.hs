{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell, 
GADTs, DeriveDataTypeable #-}
module Data.Typeable.Zipper.Monad (

    -- * State Monadic functions
      pathHere 
    , moveToM
    , moveUpM

    -- * Convenience functions, types, and exports
    -- ** Export main Zipper module
    , module Data.Typeable.Zipper

    ) where


import Data.Typeable.Zipper

 -- for our accessors, which are a category:
import Control.Category         
import Prelude hiding ((.), id) -- take these from Control.Category

--import Control.Monad.State.Class
--import Control.Monad.Trans
import Control.Monad.State
import Data.Dynamic
import Control.Applicative





    ---------------------
    -- MONADIC INTERFACE
    ---------------------


{- 
 - NOTE:
 -  Use same monad transformer package as 'fclabels': monads-fd
 -}
{-
 - NOTE:
 -  either make an existential State monad library
-   or... ?

--moveToM :: (Typeable a, MonadState (Zipper1 a) m)=> (a :-> a) -> m a
moveToM l = modify (moveTo l) >> getM focus

--moveUpM :: (Typeable a)=> Int -> StateT (Zipper1 a) Maybe a
moveUpM n = get >>= lift . moveUp n >>= put >> getM focus
-}


{-
 -TODO: it would be useful here for users to make the transformer polymorphic
 - in any Monad m, instead of Maybe, especially if we can provide some nice
 - error messages in 'fail'
 -  WHAT IF WE NEEDED TO INCORPORATE ErrorT INTO THE MONAD TRANSFORM ER STACK 
 -  HERE. YIKES.
 -}
newtype ZipperM t a = ZM { stateT :: StateT Dynamic Maybe a } deriving Monad


moveToM :: (Typeable a, Typeable b, Typeable t)=> (a :-> b) -> ZipperM t b
moveToM l = undefined

moveUpM :: (Typeable a, Typeable t)=> Int -> ZipperM t a
moveUpM n = undefined

moveUpSavingM :: (Typeable a, Typeable b, Typeable t)=> Int -> ZipperM t (a, SavedPath a b)
moveUpSavingM n = undefined


runZipper :: (Typeable t, Typeable b)=>ZipperM t a -> t -> Maybe (a, Zipper t b)
runZipper = undefined

evalZipper :: (Typeable t)=>ZipperM t a -> t -> Maybe a
evalZipper = undefined

-- | Run a function on the entire data structure we are inside of. If this 
-- alters the shape of the structure such that the path from the top to
-- our current focus gets broken, our computation will fail (int the Maybe
-- monad). Returns the Zipper focus after applying modify function.
modifyGlobal :: (Typeable t, Typeable a)=> (t -> t) -> ZipperM t a
modifyGlobal = undefined

    -- QUERYING THE ZIPPER STATE:
    -----------------------------

-- | returns the current zipper focus. This type is casted and will cause the 
-- computation to fail (in the Maybe monad) if used as a value of a different 
-- type:
viewfM :: (Typeable a, Typeable t)=> ZipperM t a
viewfM = undefined

pathHere :: (Typeable t, Typeable b)=> ZipperM t (SavedPath t b)
pathHere = undefined


