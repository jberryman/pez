{-# LANGUAGE TypeOperators, TemplateHaskell, DeriveDataTypeable #-}
module Zipper (
    -- * Export Typeable and fclabels, for convenience:
      module Data.Record.Label
    , module Data.Typeable -- WE SHOULD CONSTRAIN THIS

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
-}
    ) where


import Control.Category         
import Prelude hiding ((.), id) -- take these from Control.Category

import Data.List (unzip)

import Data.Record.Label
import Data.Typeable
import Language.Haskell.TH


{-
 - the 'a' parameter is both the root and the "step" type. Meaning
 - moving up will always yield a 'Zipper a a', however moving down
 - polymorphically is allowed. Thus 'b' is the type of the current 
 - focus.
 -}
data Zipper a b = 
         -- a stack that lets us ascend a step at a time up the data structure
    Z { lensesConts :: [ (a :-> a, a -> a) ],                  
         -- as we descend we compose types here until they can be placed on
         -- the lensesConts stack:                                                  
        typeBuffer  :: Maybe ( a :-> b, a -> b ),                
        _focus      :: b                                  
       } deriving (Typeable)

 -- generate the lens we export for 'focus' using fclabel's TH:
$(mkLabels [''Zipper])

moveTo :: (Typeable c)=> (b :-> c) -> Zipper a b -> Zipper a c
moveTo l z = undefined

moveUp :: Zipper a b -> Zipper a a
moveUp z = undefined

zipper :: (Typeable a)=> a -> Zipper a a
zipper a = undefined
