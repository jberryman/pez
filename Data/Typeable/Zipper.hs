{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell, 
GADTs, DeriveDataTypeable #-}
module Data.Typeable.Zipper (

    -- * Basic Zipper functionality
      Zipper() 
    -- ** Creating and closing Zippers
    , zipper , close
    -- ** Moving around
    , ZPath(..) , moveUp
    -- ** Querying
    , focus , viewf , atTop       

    -- * Advanced functionality
    -- ** Saving positions in a Zipper
    , SavedPath       
    , save        
    , saveFromAbove
    , savedLens   
    , closeSaving
    , moveUpSaving
    -- ** Recalling positions:
    , restore     

    -- * Convenience operators, types, and exports
    , Zipper1
    -- ** Operators
    , (.+) , (.>) , (.-) , (?+) , (?>) , (?-)
    -- ** Export Typeable class and fclabels package
    , module Data.Record.Label
    , Data.Typeable.Typeable     
) where

{- 
 -   DESCRIPTION:
 -
 -   we use a Thrist to create a type-threaded stack of continuations
 -   allowing us to have a polymorphic history of where we've been.
 -   by using Typeable, we are able to "move up" by type casting in
 -   the Maybe monad. This means that the programmer has to know
 -   what type a move up will produce, or deal with unknowns.
 -
 -
 - TODO NOTES
 -
 -   - Include as part of package a module: Data.Record.Label.Prelude that
 -   exports labels for haskell builtin types
 -
 -   - Create a 'moveUntil' function, or something else to capture the ugly:
 -          descend z@(viewf -> Gong) = z
 -          descend z                 = descend $ moveTo tock z
 -    ...perhaps we can make something clever using property of pattern match
 -     failure in 'do' block?
 -
 -   - When the 'fclabels' package supports failure handling a.la the code on
 -   Github, then these functions will take advantage of that by returning
 -   Nothing when a lens is applied to an invalid constructor:
 -       * moveTo
 -       * restore
 -   
 -   - consider instead of using section, use head form of parent with
 -   the child node set to undefined. Any performance difference?
 -
 -   - actually look at how this performs in terms of space/time
 -
 -   ROADMAP:
 -    Pink Elephant
 -    Patiently Expectant
 -    Probably ??
 -
 -}

 -- this is where the magic happens:
import Data.Record.Label
import Data.Typeable
import Data.Thrist

 -- for our accessors, which are a category:
import Control.Category         
import Prelude hiding ((.), id) -- take these from Control.Category
import Control.Applicative


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
-- as the one we just exited. You can also extract a lens from a SavedPath that
-- points to that location:
newtype SavedPath a b = S { savedLenses :: Thrist TypeableLens a b } 
    deriving (Typeable, Category)

-- We need another GADT here to enforce the Typeable constraint within the
-- hidden types in our thrist of lenses above:
data TypeableLens a b where
    TL :: (Typeable a,Typeable b)=> {tLens :: (a :-> b)} -> TypeableLens a b



-- TODO: TRY USING FUNDEPS ALA THE MONAD TRANSFORMER LIBRARIES FOR CLASS
-- CONSTRAINTS HERE:
--class (Typeable b, Typeable c) => ZPath p b c | p -> b, p -> c where
--
-- | Types of the ZPath class act as references to "paths" down through a datatype.
-- Currently lenses from 'fclabels' and SavedPath types are instances
class ZPath p where
    -- | Move down the structure to the label specified. Return Nothing if the
    -- label is not valid for the focus's constructor:
    moveTo :: (Typeable b, Typeable c) => p b c -> Zipper a b -> Zipper a c



    ---------------------------
    -- Basic Zipper Functions:
    ---------------------------


-- | a fclabel lens for setting, getting, and modifying the zipper's focus:
$(mkLabelsNoTypes [''Zipper])


instance ZPath (:->) where
    moveTo = flip pivot . TL

instance ZPath SavedPath where
    moveTo = flip (foldlThrist pivot) . savedLenses  


-- | Move up n levels as long as the type of the parent is what the programmer
-- is expecting and we aren't already at the top. Otherwise return Nothing.
moveUp :: (Typeable c, Typeable b)=> Int -> Zipper a c -> Maybe (Zipper a b)
moveUp 0  z                        = gcast z
moveUp n (Z (Cons (H _ f) stck) c) = moveUp (n-1) (Z stck $ f c)
moveUp _  _                        = Nothing  


zipper :: a -> Zipper a a
zipper = Z Nil


close :: Zipper a b -> a
close = snd . closeSaving



    ------------------------------
    -- ADVANCED ZIPPER FUNCTIONS:
    ------------------------------

--- THIS FUNCTION GAVE ME THE MOST TROUBLE AND COULD PROBABLY BE SIMPLIFIED AND
--- 'moveUP' DEFINED IN TERMS OF IT, BUT FOR NOW I AM HAPPY WITH SOMETHING THAT
--- WORKS. 

-- | Move up a level as long as the type of the parent is what the programmer
-- is expecting and we aren't already at the top. Otherwise return Nothing.
moveUpSaving :: (Typeable c, Typeable b)=> Int -> Zipper a c -> Maybe (Zipper a b, SavedPath b c)
moveUpSaving n z = (,) <$> moveUp n z <*> saveFromAbove n z

data ZipperLenses a c b = ZL { zlStack :: ZipperStack b a,
                               zLenses :: Thrist TypeableLens b c }


-- | return a SavedPath from n levels up to the current level
saveFromAbove n = fmap (S . zLenses) . mvUpSavingL n . flip ZL Nil . stack
    where
        mvUpSavingL :: (Typeable b', Typeable b)=> Int -> ZipperLenses a c b -> Maybe (ZipperLenses a c b')
        mvUpSavingL 0 z                           = gcast z
        mvUpSavingL n (ZL (Cons (H l _) stck) ls) = mvUpSavingL (n-1) (ZL stck $ Cons (TL l) ls)
        mvUpSavingL _ _                           = Nothing



closeSaving :: Zipper a b -> (SavedPath a b, a)
closeSaving (Z stck b) = (S ls, a)
    where ls = getReverseLensStack stck
          a  = compStack (mapThrist hCont stck) b


-- | Return a SavedPath type encapsulating the current location in the Zipper.
-- This lets you return to a location in your data type after closing the 
-- Zipper.
save :: Zipper a b -> SavedPath a b
save = fst . closeSaving

-- | Extract a composed lens that points to the location we SavedPath. This lets 
-- us modify, set or get a location that we visited with our Zipper after 
-- closing the Zipper.
savedLens :: (Typeable a, Typeable b)=> SavedPath a b -> (a :-> b)
savedLens = compStack . mapThrist tLens . savedLenses


-- | Return to a previously SavedPath location within a data-structure. 
-- Saving and restoring lets us for example: find some location within our 
-- structure using a Zipper, save the location, fmap over the entire structure,
-- and then return to where we were:
restore :: (ZPath p, Typeable a, Typeable b)=> p a b -> a -> Zipper a b
restore s = moveTo s  . zipper


-- | returns True if Zipper is at the top level of the data structure:
atTop :: Zipper a b -> Bool
atTop = nullThrist . stack

{-
-- | Return our depth in the Zipper. if atTop z then level z == 0
level :: Zipper a b -> Int
level = foldlThrist (.) ...forgot how to do this :(
-}
----------------------------------------------------------------------------


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


-- bind higher than <$>. Is this acceptable?:
infixl 5 .+, .>, .-, ?+, ?>, ?-

-- | 'moveTo' with arguments flipped. Operator plays on the idea of addition of
-- levels onto the focus.
(.+) :: (ZPath p, Typeable b, Typeable c)=> Zipper a b -> p b c -> Zipper a c
(.+) = flip moveTo

-- | 'moveUp' with arguments flipped. Operator syntax comes from the idea of
-- moving up as subtraction.
(.-) :: (Typeable c, Typeable b)=> Zipper a c -> Int -> Maybe (Zipper a b)
(.-) = flip moveUp

-- | setL focus, with arguments flipped
(.>) :: Zipper a b -> b -> Zipper a b
(.>) = flip (setL focus)

(?+) :: (ZPath p, Typeable b, Typeable c)=> Maybe (Zipper a b) -> p b c -> Maybe (Zipper a c)
(?+)= flip (fmap . moveTo)

(?-) :: (Typeable c, Typeable b)=> Maybe (Zipper a c) -> Int -> Maybe (Zipper a b)
mz ?- n = mz >>= moveUp n

(?>) :: Maybe (Zipper a b) -> b -> Maybe (Zipper a b)
(?>) = flip (fmap . setL focus)


    ------------
    -- HELPERS
    ------------

 -- The core of our 'moveTo' function
pivot (Z t a') (TL l) = Z (Cons h t) b
    where h = H l (a' `missing` l)
          b = getL l a'
           --TODO: MAYBE GIVE THE GC SOME STRICTNESS HINTS HERE?:
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

