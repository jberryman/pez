{-# LANGUAGE TypeOperators, TemplateHaskell, GADTs, DeriveDataTypeable #-}
module Zipper (

    -- * Basic Zipper functionality:
      Zipper()
    -- ** Creating and closing Zippers:
    , zipper 
    , close
    -- ** Moving around:
    , moveTo
    , moveUp
    -- ** Querying:
    , focus
    , viewf
    , atTop       

    -- * Advanced functionality:
    -- ** Saving positions in a Zipper:
    , Saved       
    , save        
    , savedLens   
    , closeSaving
 -- , moveUpSaving
    -- ** Recalling positions:
    , restore     
 -- , moveBack

    --Here ** State Monadic functions:
    , moveToM
    , moveUpM

    -- * Convenience functions, types, and exports:
    , Zipper1
    -- ** Export Typeable and fclabels:
    , module Data.Record.Label
    , Data.Typeable.Typeable     

    ) where


 -- this is where the magic happens:
import Data.Record.Label
import Data.Typeable
import Data.Thrist

 -- for our accessors, which are a category:
import Control.Category         
import Prelude hiding ((.), id) -- take these from Control.Category

--import Control.Monad.State.Class
--import Control.Monad.Trans
import Control.Monad.State
import Data.Dynamic



{- 
 - DESCRIPTION:
 -
 - we use a Thrist to create a type-threaded stack of continuations
 - allowing us to have a polymorphic history of where we've been.
 - by using Typeable, we are able to "move up" by type casting in
 - the Maybe monad. This means that the programmer has to know
 - what type a move up will produce, or deal with unknowns.
 -
 -TODO: consider instead of using section, use head form of parent with
 -      the child node set to undefined. Any performance difference?
 -}


{- 
 - TODO maybe define:
 -  moveUpSaving :: Int -> Zipper a c -> Maybe (Saved b c, Zipper a b)
 -     (then define moveUp in terms of moveUpSaving)
 -  moveBack :: Saved b c -> Zipper a b -> Zipper a c
 -     (then define 'restore' in terms of moveBack)
 -  monadic versions of above
 -
 -}

    -------------------------
    -- TYPES: the real heros
    ------------------------


 -- We store our history in a type-threaded list of pairs of lenses and
 -- continuations (parent data-types with a "hole" where the child fits):
 --    Use GADT to enforce Typeable constraint
data HistPair b a where 
    H :: (Typeable a, Typeable b)=> { hLens :: (a :-> b),
                                      hCont :: (b -> a) } -> HistPair b a

type ZipperStack b a = Thrist HistPair b a

data Zipper a b = Z { stack  :: ZipperStack b a,
                      _focus :: b                                  
                    } deriving (Typeable)
    

 -- | stores the path used to return to the same location in a data structure
 -- as the one we just exited. You can also extract a lens from a Saved that
 -- points to that location:
newtype Saved a b = S { savedLenses :: Thrist TypeableLens a b }

-- We need another GADT here to enforce the Typeable constraint within the
-- hidden types in our thrist of lenses above:
data TypeableLens a b where
    TL :: (Typeable a,Typeable b)=> {tLens :: (a :-> b)} -> TypeableLens a b



    ---------------------------
    -- Basic Zipper Functions:
    ---------------------------
 
 
-- | a fclabel lens for setting, getting, and modifying the zipper's focus:
$(mkLabelsNoTypes [''Zipper])


 -- | Move down the structure to the label specified. Return Nothing if the
 -- label is not valid for the focus's constructor:
moveTo :: (Typeable b, Typeable c)=> (b :-> c) -> Zipper a b -> Zipper a c
moveTo l (Z stck b) = let h = H l (b `missing` l) 
                          c = getL l b      
                       in Z (Cons h stck) c

 -- | Move up a level as long as the type of the parent is what the programmer
 -- is expecting and we aren't already at the top. Otherwise return Nothing.
moveUp :: (Typeable c, Typeable b)=> Int -> Zipper a c -> Maybe (Zipper a b)
moveUp 0  z                        = gcast z
moveUp n (Z (Cons (H _ f) stck) c) = moveUp (n-1) (Z stck $ f c)
moveUp _  _                        = Nothing  


zipper :: (Typeable a)=> a -> Zipper a a
zipper = Z Nil


close :: Zipper a b -> a
close = snd . closeSaving



    ------------------------------
    -- ADVANCED ZIPPER FUNCTIONS:
    ------------------------------


closeSaving :: Zipper a b -> (Saved a b, a)
closeSaving (Z stck b) = (S ls, a)
    where ls = getReverseLensStack stck
          a  = compStack (mapThrist hCont stck) b


-- | Return a Saved type encapsulating the current location in the Zipper.
-- This lets you return to a location in your data type after closing the 
-- Zipper.
save :: Zipper a b -> Saved a b
save = fst . closeSaving

-- | Extract a composed lens that points to the location we Saved. This lets 
-- us modify, set or get a location that we visited with our Zipper after 
-- closing the Zipper.
savedLens :: (Typeable a, Typeable b)=> Saved a b -> (a :-> b)
savedLens = compStack . mapThrist tLens . savedLenses


 --TODO: add error handling in Maybe monad for when we hit a bad constructor
 --      a safe get function provided by fclabels would be excellent here.
 --      How can we catch non-exhaustive pattern errors outside of the IO
 --      monad ? If impossible, then we need it done in 'fclabels'
-- | Return to a previously Saved location within a data-structure. 
-- Saving and restoring lets us, for example: find some location within our 
-- structure using a Zipper, save the location, fmap over the entire structure,
-- and then return to where we were:
restore :: (Typeable a)=> a -> Saved a b -> Maybe (Zipper a b)
restore a = foldMThrist res (Z Nil a) . savedLenses  where
    res (Z t a') (TL l) = let h = H l (a' `missing` l)
                              b = getL l a'
                           in Just $ Z (Cons h t) b

--TODO: consider whether we should provide a 'level' function and define
-- this as (==0) . lengthThrist . stack
-- | returns True if Zipper is at the top level of the data structure:
atTop :: Zipper a b -> Bool
atTop = nullThrist . stack



    ---------------------
    -- MONADIC INTERFACE
    ---------------------


{-

---- MAYBE WE SHOULD MAKE OUR FUNCTIONS POLYMORPHIC AND NOT USE
---- THESE TYPE SYNONYMS. (so people can use stateT.lazy, etc.)

-- | A synonym for the StateT / Maybe monad transformer: a state monad where 
-- the Zipper is passed around as the state:
type ZipperM a b r = StateT (Zipper a b) Maybe r

-- | Same as ZipperM except for passing a Zipper1:
type ZipperM1 a r = StateT (Zipper1 a) Maybe r


{- 
 - NOTE:
 -  Use same monad transformer package as 'fclabels': monads-fd
 -}

--setM :: MonadState s m => (s :-> b) -> b -> m ()

moveToM :: (Typeable s, Typeable c, MonadState s m) => 
           (s :-> c) -> Zipper a

-}


    ----------------
    -- CONVENIENCE
    ----------------

-- | a view function for a Zipper's focus. Defined simply as: `getL` focus
viewf :: Zipper a b -> b
viewf = getL focus

-- | a simple type synonym for a Zipper where the type at the focus is the
-- same as the type of the outer (unzippered) type. Cleans up type signatures
-- for simple recursive types:
type Zipper1 a = Zipper a a


    ------------
    -- HELPERS
    ------------


 -- make a hole in a type corresponding to the passed lens, forming a section:
 --  TODO: MAYBE GIVE THE GC SOME HINTS HERE?:
missing :: a -> (a :-> b) -> (b -> a)
missing a l = flip (setL l) a

 -- fold a thrist into a single category by composing the stack with (.)
 -- Here 'cat' will be either (->) or (:->):
compStack :: (Category cat)=> Thrist cat b a -> cat b a
compStack = foldrThrist (flip(.)) id


 -- Takes the zipper stack and extracts each lens segment, and recomposes
 -- them in reversed order, forming a lens from top to bottom of a data 
 -- structure:
getReverseLensStack :: ZipperStack b a -> Thrist TypeableLens a b
getReverseLensStack = unflip . foldlThrist rev (Flipped Nil)
    where rev (Flipped t) (H l _) = Flipped $ Cons (TL l) t

