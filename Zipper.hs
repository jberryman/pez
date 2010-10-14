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

    , Saved -- stores our lens
    , save  --extracts a Saved from zipper
    , savedLens  -- extracts the Lens from Saved wrapper
    , restore    -- re-enters a data type catching any errors in a cool way

    , atTop     -- returns True if at top (can't moveUp)

    {-  -- maybe include these:
    , stepUp    -- :: Zipper a b -> Maybe (Zipper a a)
    , dropTo    -- like moveTo, but doesn't save history?
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

 -- for convenient tuple functions:
import Control.Arrow


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
    


data SavedLens b a where 
    SL :: (Typeable a, Typeable b)=> (a :-> b) -> SavedLens b a

type LensStack b a = Thrist SavedLens b a

 -- | stores the path used to return to the same location in a data structure
 -- as the one we just exited. You can also extract a lens from a Saved that
 -- points to that location:
newtype Saved a b = S { savedLenses :: LensStack b a }



    ---------------------------
    -- Basic Zipper Functions:
    ---------------------------
 
 
 -- generate the lens we export for 'focus' using fclabel's TH:
$(mkLabels [''Zipper])


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
moveUp _ = Nothing  


zipper :: (Typeable a)=> a -> Zipper a a
zipper = Z Nil


close :: Zipper a b -> a
close = snd . closeSaving




    ------------------------------
    -- ADVANCED ZIPPER FUNCTIONS:
    ------------------------------


closeSaving :: Zipper a b -> (Saved a b, a)
closeSaving (Z stck b) = (S lnss, a)
    where (lnss, fs) = unzipThrist stck 
          a          = compStack fs b



save :: Zipper a b -> Saved a b
save = fst . closeSaving

savedLens :: (Typeable a, Typeable b)=> Saved a b -> (a :-> b)
savedLens = getLens . foldThrist compLenses (SL id) . savedLenses
    where compLenses (SL l) (SL l') = SL (l . l')
          getLens (SL l) = l

restore :: Saved a b -> a -> Maybe (Zipper a b)
restore s a = undefined


atTop :: Zipper a b -> Bool
atTop (Z Nil _) = True
atTop _         = False

{-
-- maybe include these:
stepUp :: Zipper a b -> Maybe (Zipper a a)
dropTo :: (Typeable b, Typeable c)=> (b :-> c) -> Zipper a b -> Maybe (Zipper a c)
-}



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

 -- seperates the paired Thrist levels into thrists representing the lenses
 -- and the one-hole continuations seperately:
unzipThrist :: ZipperStack b a -> (LensStack b a , Thrist (->) b a)
unzipThrist = mapThrist (\(H l _)-> SL l) &&& mapThrist (\(H _ f)->f)
