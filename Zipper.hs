{-# LANGUAGE TypeOperators, TemplateHaskell, GADTs, DeriveDataTypeable #-}
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
{-    

    , Saved -- stores our lens
    , save  --extracts a Saved from zipper
    , savedLens  -- extracts the Lens from Saved wrapper
    , restore    -- re-enters a data type catching any errors in a cool way

      -- maybe include these:
    , stepUp    -- :: Zipper a b -> Maybe (Zipper a a)
    , dropTo    -- like moveTo, but doesn't save history?
    , level     -- returns our depth in tree, useful because we might
                -- use Maybe for Type casting AND for trying to moveUp
                -- on head
-}
    ) where



import Data.List (unzip)

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
    H :: (Typeable a, Typeable b)=> (a :-> b) -> (b -> a) -> HistPair b a

type ZipperStack b a = Thrist HistPair b a

data Zipper a b = Z { stack  :: ZipperStack b a,
                      _focus :: b                                  
                    } deriving (Typeable)
    
 -- generate the lens we export for 'focus' using fclabel's TH:
$(mkLabels [''Zipper])


 -- | stores the path used to return to the same location in a data structure
 -- as the one we just exited. You can also extract a lens from a Saved that
 -- points to that location:
data Saved a b = S (Thrist (:->) a b)



    ---------------------------
    -- Basic Zipper Functions:
    ---------------------------


-- TODO: - make below use some error handling for Maybe:
 --
 -- | Move down the structure to the label specified. Return Nothing if the
 -- label is not valid for the focus's constructor:
moveTo :: (Typeable b, Typeable c)=> (b :-> c) -> Zipper a b -> Maybe (Zipper a c)
moveTo l (Z stck b) = let f = b `missing` l 
                          c = getL l b      
                       in Just $ Z (Cons (H l f) stck) c

 -- | Move up a level as long as the type of the parent is what the programmer
 -- is expecting and we aren't already at the top. Otherwise return Nothing.
moveUp :: (Typeable c, Typeable b)=> Zipper a c -> Maybe (Zipper a b)
moveUp (Z Nil _)                 = Nothing --already at top
moveUp (Z (Cons (H _ f) stck) c) = gcast $ Z stck $ f c


zipper :: (Typeable a)=> a -> Zipper a a
zipper = Z Nil


close :: Zipper a b -> a
close = snd . closeSaving


closeSaving :: Zipper a b -> (Saved a b, a)
--closeSaving (Z lThr cThr b) = (S lThr, compStack cThr b)
closeSaving = undefined




    ------------
    -- HELPERS
    ------------


 -- make a hole in a type corresponding to the passed lens, forming a section:
 --  MAYBE GIVE THE GC SOME HINTS HERE?:
missing :: a -> (a :-> b) -> (b -> a)
missing a l = flip (setL l) a


 -- fold a thrist into a single category by composing the stack with (.)
 -- Here 'cat' will be either (->) or (:->):
compStack :: (Category cat)=> Thrist cat b a -> cat b a
compStack = foldThrist (flip(.)) id
