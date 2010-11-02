{-# LANGUAGE TypeOperators, TemplateHaskell, GADTs, DeriveDataTypeable, RankNTypes #-}
module Zipper (
    -- * Export Typeable and fclabels, for convenience:
      module Data.Record.Label
    , module Data.Typeable     -- WE SHOULD CONSTRAIN THIS

    , Zipper()
    , moveTo
    , moveUp
    , focus

    , zipper 
    , close
    , closeSaving

    , Saved -- stores our lens
    , save  --extracts a Saved from zipper
    , savedLens  -- extracts the Lens from Saved wrapper
    , restore    -- re-enters a data type catching any errors in a cool way

    , atTop     -- returns True if at top (can't moveUp)
    ) where


 -- this is where the magic happens:
import Data.Record.Label
import Data.Typeable
import Language.Haskell.TH
import Data.Thrist

 -- for our accessors, which are a category:
import Control.Category         
import Prelude hiding ((.), id) -- take these from Control.Category


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


-- TODO: - make below use some error handling for Maybe:
 --
 -- | Move down the structure to the label specified. Return Nothing if the
 -- label is not valid for the focus's constructor:
moveTo :: (Typeable b, Typeable c)=> (b :-> c) -> Zipper a b -> Maybe (Zipper a c)
moveTo l (Z stck b) = let h = H l (b `missing` l) 
                          c = getL l b      
                       in Just $ Z (Cons h stck) c

 -- | Move up a level as long as the type of the parent is what the programmer
 -- is expecting and we aren't already at the top. Otherwise return Nothing.
moveUp :: (Typeable c, Typeable b)=> Zipper a c -> Maybe (Zipper a b)
moveUp (Z (Cons (H _ f) stck) c) = gcast $ Z stck $ f c
moveUp _                         = Nothing  


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



save :: Zipper a b -> Saved a b
save = fst . closeSaving

savedLens :: (Typeable a, Typeable b)=> Saved a b -> (a :-> b)
savedLens = compStack . mapThrist tLens . savedLenses


 --TODO: add error handling in Maybe monad for when we hit a bad constructor
restore :: (Typeable a)=> a -> Saved a b -> Maybe (Zipper a b)
restore a = foldMThrist res (Z Nil a) . savedLenses  where
    res (Z t a') (TL l) = let h = H l (a' `missing` l)
                              b = getL l a'
                           in Just $ Z (Cons h t) b

-- | returns true if Zipper is at the top level of the data structure:
atTop :: Zipper a b -> Bool
atTop = nullThrist . stack



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




-----------------------------------------------------------------
-----------------------------------------------------------------
-- TO BE DEFINED IN NEW VERSION OF Data.Thrist:
--    when it is upgraded, we should remove these definitions and
--    also remove the RankNTypes extension from this file AND the
--    cabal file:

foldrThrist :: (forall i j . (i ~> j) -> (j +> c) -> (i +> c)) 
            -> (b +> c) 
            -> Thrist (~>) a b 
            -> (a +> c)
foldrThrist _ v Nil        = v
foldrThrist f v (Cons h t) = h `f` (foldrThrist f v t)

 -- THIS MAY HAVE TO STAY IN HERE:
newtype Flipped m a b = Flipped { unflip :: m b a }

foldlThrist :: (forall j k . (a +> j) -> (j ~> k) -> (a +> k)) 
               -> (a +> b) 
               -> Thrist (~>) b c 
               -> (a +> c)
foldlThrist f v Nil        = v
foldlThrist f v (Cons h t) = foldlThrist f (v `f` h) t 


foldMThrist :: Monad m=> 
               (forall j k . (a +> j) -> (j ~> k) -> m (a +> k)) 
               -> (a +> b) 
               -> Thrist (~>) b c 
               -> m (a +> c)
foldMThrist _ a Nil        = return a
foldMThrist f a (Cons h t) = f a h >>= \fah -> foldMThrist f fah t


nullThrist :: Thrist (~>) a b -> Bool
nullThrist Nil = True
nullThrist _   = False


