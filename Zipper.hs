{-# LANGUAGE TypeOperators, TemplateHaskell, DeriveDataTypeable #-}
module Zipper (
    -- * Export Typeable and fclabels, for convenience:
      module Data.Record.Label
    , module Data.Typeable     -- WE SHOULD CONSTRAIN THIS

    , Zipper()
    , moveTo
    , moveUp
    , focus

{-    
    , zipper 
    , close
    , closeSaving

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
 - the 'a' parameter is both the root and the "step" type. Meaning
 - moving up will always yield a 'Zipper a a', however moving down
 - polymorphically is allowed. Thus 'b' is the type of the current 
 - focus.
data Zipper a b = 
         -- a stack that lets us ascend a step at a time up the data structure
    Z { lensesConts :: [ (a :-> a, a -> a) ],                  
         -- as we descend we compose types here until they can be placed on
         -- the lensesConts stack:                                                  
        typeBuffer  :: Maybe ( a :-> b, a -> b ),                
        _focus      :: b                                  
       } deriving (Typeable)
 -}

{- 
 - we use a Thrist to create a type-threaded stack of continuations
 - allowing us to have a polymorphic history of where we've been.
 - by using Typeable, we are able to "move up" by type casting in
 - the Maybe monad. This means that the programmer has to know
 - what type a move up will produce, or deal with unknowns.
 -}
data Zipper a b = 
         -- stacks that let us ascend a step at a time up the data structure
    Z { lenses ::  Thrist (:->) a b,                  
        conts  :: Thrist (->) b a,
        _focus  :: b                                  
      } deriving (Typeable)


 -- generate the lens we export for 'focus' using fclabel's TH:
$(mkLabels [''Zipper])

moveTo :: (Typeable c)=> (b :-> c) -> Zipper a b -> Zipper a c
moveTo = undefined
{-
moveTo l (Z cs mb f) = do let c  = (l , f `missing` l) 
                              f' = getL l f
                          a <- gcast ...
-}

moveUp :: Zipper a b -> Zipper a a
moveUp z = undefined


zipper :: (Typeable a)=> a -> Zipper a a
zipper a = undefined


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
