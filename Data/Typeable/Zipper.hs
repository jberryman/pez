{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell,
GADTs, DeriveDataTypeable, 
FlexibleInstances #-}

{- |
PEZ is a generic zipper library. It uses lenses from the "fclabels" package to
reference a \"location\" to move to in the zipper. The zipper is restricted to
types in the 'Typeable' class, allowing the user to \"move up\" through complex data
structures such as mutually-recursive types.
.
Both the Typeable class and "fclabels" lenses can be derived in GHC, making it
easy for the programmer to use a zipper with a minimum of boilerplate.
-}

module Data.Typeable.Zipper (
    -- * Usage
    {- |
     First import the library, which brings in the Typeable and "fclabels" modules.
     You will also want to enable a few extensions:
      
     > -- {-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeOperators #-}
     > module Main where
     >
     > import Data.Typeable.Zipper
      
     Create a datatype, deriving an instance of the Typeable class, and generate a
     lens using functions from fclabels:
      
     > data Tree a = Node { 
     >     _leftNode :: Tree a
     >   , _val      :: a 
     >   , _rightNode :: Tree a }
     >   | Nil  
     >   deriving (Typeable,Show)
     >
     > $(mkLabelsNoTypes [''Tree])
      
     Now we can go crazy using Tree in a 'Zipper':
      
     > treeBCD = Node (Node Nil 'b' Nil) 'c' (Node Nil 'd' Nil)
     > 
     > descendLeft :: Zipper1 (Tree a) -> Zipper1 (Tree a)
     > descendLeft z = case (viewf z) of
     >                      Nil -> z
     >                      _   -> descendLeft $ move leftNode z
     >
     > insertLeftmost :: a -> Tree a -> Tree a
     > insertLeftmost x = close . setL focus x . descendLeft . zipper
     >
     > treeABCD = insertLeftmost 'a' treeBCD
      
     Because of the flexibility of "fclabels", this zipper library can be used to
     express moving about in reversible computations simply by defining such a lens,
     for instance:
      
     > stringRep :: (Show b, Read b) => b :-> String
     > stringRep = lens show (const . read)
    -}

    -- * Basic Zipper functionality
      Zipper() 
    {- |
       /A note on failure in zipper operations:/

       Most operations on a 'Zipper' return a result of the Maybe type, for 
       various types of failures. Here is a list of failure scenarios:

         - a 'move' Up arrives at a type that could not be cast to the type
           expected

         - a @move (Up 1)@ when already 'atTop', i.e. we cannot ascend anymore

         - a @move@ to a label (e.g. @foo :: FooBar :~> FooBar@) causes a
           failure in the getter function of the lens, usually because the 
           'focus' was the wrong constructor for the lens

         - a @move (Up n)@ causes the /setter/ of the lens we used to arrive at
           the current focus to fail on the value of the current focus. This 
           is not something that happens for normal lenses, but is desirable 
           for structures that enforce extra-type-system constraints. 

         - a 'close' cannot re-build the structure because some setter failed,
           as above. Again, this does not occur for TH'generated lenses.

    -}
    -- ** Creating and closing Zippers
    , zipper , close
    -- ** Moving around
    , Motion(..) , Up(..)
    -- ** Querying
    -- | a "fclabels" lens for setting, getting, and modifying the zipper's focus:
    , focus 
    , viewf , atTop       

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
    {-
    -- ** Operators
    , (.+) , (.>) , (.-) , (?+) , (?>) , (?-)
    -}
    -- ** Export Typeable class and "fclabels" package
    , module Data.Label
    , Data.Typeable.Typeable     
) where

{- 
 -   IMPLEMENTATION NOTES:
 -
 -   we use a Thrist to create a type-threaded stack of continuations
 -   allowing us to have a polymorphic history of where we've been.
 -   by using Typeable, we are able to "move up" by type casting in
 -   the Maybe monad. This means that the programmer has to know
 -   what type a move up will produce, or deal with unknowns.
 -
 -
 -   TODO NOTES
 -
 -   - When the 'fclabels' package supports failure handling a.la the code on
 -   Github, then these functions will take advantage of that by returning
 -   Nothing when a lens is applied to an invalid constructor:
 -       * moveTo
 -       * restore
 -
 -   - see if Zipper monad looks more attractive now w/ partial types.
 -   
 -   - look at usability and re-define/remove/add functions as needed, e.g.:
 -       - Create a 'moveUntil' function, or something else to capture the ugly:
 -              descend z@(viewf -> Gong) = z
 -              descend z                 = descend $ moveTo tock z
 -         ...perhaps we can make something clever using property of pattern match
 -         failure in 'do' block?
 -         - SEE IF ArrowChoice MIGHT GET US CLOSE TO WHAT WE WANT
 -       - experiments with state monad interface (see above)
 -
 -
 -   - Separate module: Data.Record.Label.Prelude that
 -   exports labels for haskell builtin types. Ask S. V. if he wantd to include
 -   it with fclabels.
 -
 -   - consider instead of using section, use head form of parent with
 -   the child node set to undefined. Any performance difference?
 -
 -   - actually look at how this performs in terms of space/time
 -
 -   ROADMAP:
 -    Pink Elephant
 -    Placebo Effect
 -    Patiently Expectant
 -    Probably ??
 -
 -}

 -- this is where the magic happens:
import Data.Label
import qualified Data.Label.Maybe as M
import qualified Data.Label.Abstract as A
import Data.Typeable
import Data.Thrist

 -- for our accessors, which are a category:
import Control.Category         
import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow(Kleisli(..))
-- these required for creating a Motion instance for lenses:
import Control.Monad.Identity
import Control.Monad.Trans.Maybe

    -------------------------
    -- TYPES: the real heros
    ------------------------

{- *
 - It's interesting to note in our :~> lenses the setter also can fail, and can
 - fail based not only on the constructor 'f' but also for certain values of 'a'
 - This is kind of interesting; it lets lenses enforce constraints on a type
 - that the type system cannot, e.g. Foo Int, where Int must always be odd.
 -
 - So a module might export a type with hidden constructors and only lenses for
 - an interface. Our zipper could navigate around in the type, and all the
 - constraints would still be enforced on the unzippered type. Cool!
-}

 -- We store our history in a type-threaded list of pairs of lenses and
 -- continuations (parent data-types with a "hole" where the child fits), the
 -- lenses are kept around so that we can extract the "path" to the current
 -- focus and apply it to other data types. Use GADT to enforce Typeable.
data HistPair b a where 
    H :: (Typeable a, Typeable b)=> 
                { hLens :: (a M.:~> b)
                , hCont :: Kleisli Maybe b a -- *see above
                } -> HistPair b a

type ZipperStack b a = Thrist HistPair b a

-- TODO: this could be a contravariant functor, no?:
--
data Zipper a b = Z { stack  :: ZipperStack b a
                    , _focus :: b                                  
                    } deriving (Typeable)
    

-- TODO: when new 'thrist' supports arbitrary Arrow instance, we can derive
-- Arrow and ArrowChoice / ArrowZero here:

-- | stores the path used to return to the same location in a data structure
-- as the one we just exited. You can also extract a lens from a SavedPath that
-- points to that location:
newtype SavedPath a b = S { savedLenses :: Thrist TypeableLens a b } 
    deriving (Typeable, Category)

-- We need another GADT here to enforce the Typeable constraint within the
-- hidden types in our thrist of lenses above:
data TypeableLens a b where
    TL :: (Typeable a,Typeable b)=> { tLens :: (a M.:~> b)
                                    } -> TypeableLens a b



-- TODO: make this return Maybe, make instances be 
  -- :: (Control.Arrow.ArrowZero (~>)
  --     Control.Arrow.ArrowChoice (~>)) =>
  --       Lens (~>) a b
-- in addition to SavedPath.
--
-- Then create a separate function:
--   move' :: (Typeable b, Typeable c)=> (b :-> c) -> Zipper a b -> Zipper a c
--
-- | Types of the Motion class act as references to \"paths\" up or down 
-- through a datatype.
class Motion p where
    -- | Move through the structure to the label specified, returning 'Nothing'
    -- if the motion is invalid.
    move :: (Typeable b, Typeable c) => p b c -> Zipper a b -> Maybe (Zipper a c)

-- | a 'Motion' upwards in the data type. e.g. @move (Up 2)@ would move up to
-- the grandparent level, as long as the type of the focus after the motion is 
-- @b@.
newtype Up c b = Up Int

    ---------------------------
    -- Basic Zipper Functions:
    ---------------------------


$(mkLabels [''Zipper])


-- this is (:~>) from fclabels. An alternative is to create a newtype wrapper:
instance Motion (A.Lens (Kleisli (MaybeT Identity))) where
    move = flip pivot . TL

instance Motion SavedPath where
    move = flip (foldMThrist pivot) . savedLenses  

-- TODO: maybe Up can derive a Num instance??
instance Motion Up where
    move (Up 0)  z                  = gcast z
    move (Up n) (Z (Cons (H _ k) stck) c) = runKleisli k c >>= move (Up (n-1)) . Z stck
    move _  _                       = Nothing  


-- | create a zipper with the focus on the top level.
zipper :: a -> Zipper a a
zipper = Z Nil

-- | re-assembles the data structure from the top level, returning @Nothing@ if
-- the structure cannot be re-assembled.
--
-- /Note/: For standard lenses produced with 'mkLabels' this will never fail. 
-- However setters can be used to enforce arbitrary constraints on a data 
-- structure, e.g. thata type @Odd Int@ can only hold an odd integer. This
-- function returns @Nothing@ in such cases.
close :: Zipper a b -> Maybe a
close = snd . closeSaving



    ------------------------------
    -- ADVANCED ZIPPER FUNCTIONS:
    ------------------------------

-- TODO: this could sort of be polymorphic over Motion but that wouldn't really
-- have much meaning for moving down (would return the lens we passed)

-- | Move up @n@ levels as long as the type of the parent is what the programmer
-- is expecting and we aren't already at the top. Otherwise return Nothing.
moveUpSaving :: (Typeable c, Typeable b)=> Int -> Zipper a c -> Maybe (Zipper a b, SavedPath b c)
moveUpSaving n z = (,) <$> move (Up n) z <*> saveFromAbove n z

data ZipperLenses a c b = ZL { zlStack :: ZipperStack b a,
                               zLenses :: Thrist TypeableLens b c }


-- | return a 'SavedPath' from n levels up to the current level
saveFromAbove :: (Typeable c, Typeable b) => Int -> Zipper a c -> Maybe (SavedPath b c)
saveFromAbove n = fmap (S . zLenses) . mvUpSavingL n . flip ZL Nil . stack
    where
        mvUpSavingL :: (Typeable b', Typeable b)=> Int -> ZipperLenses a c b -> Maybe (ZipperLenses a c b')
        mvUpSavingL 0 z                     = gcast z
        mvUpSavingL n (ZL (Cons (H l _) stck) ls) = mvUpSavingL (n-1) (ZL stck $ Cons (TL l) ls)
        --mvUpSavingL n (ZL (Cons h stck) ls) = mvUpSavingL (n-1) (ZL stck $ Cons (TL $ hLens h) ls)
        mvUpSavingL _ _                     = Nothing



-- TODO: THIS HAS TO RETURN Maybe, SEE COMPSTACK

-- | Close the zipper, returning the saved path back down to the zipper\'s
-- focus. See 'close'
closeSaving :: Zipper a b -> (SavedPath a b, Maybe a)
closeSaving (Z stck b) = (S ls, ma)
    where ls = getReverseLensStack stck
          kCont = compStack $ mapThrist hCont stck
          ma = runKleisli kCont b


-- | Return a 'SavedPath' type encapsulating the current location in the 'Zipper'.
-- This lets you return to a location in your data type after closing the 
-- Zipper.
--
-- > save = fst . closeSaving
save :: Zipper a b -> SavedPath a b
save = fst . closeSaving

-- | Extract a composed lens that points to the location we saved. This lets 
-- us modify, set or get a location that we visited with our 'Zipper' after 
-- closing the Zipper.
savedLens :: (Typeable a, Typeable b)=> SavedPath a b -> (a M.:~> b)
savedLens = compStack . mapThrist tLens . savedLenses


-- | Return to a previously 'SavedPath' location within a data-structure. 
--
-- Saving and restoring lets us for example: find some location within our 
-- structure using a 'Zipper', save the location, 'fmap' over the entire structure,
-- and then return to where we were safely, even if the \"shape\" of our
-- structure has changed.
--
-- > restore s = move s . zipper
restore :: (Motion p, Typeable a, Typeable b)=> p a b -> a -> Maybe (Zipper a b)
restore s = move s . zipper


-- | returns 'True' if 'Zipper' is at the top level of the data structure:
atTop :: Zipper a b -> Bool
atTop = nullThrist . stack

{-
-- | Return our depth in the 'Zipper'. if 'atTop' z then 'level' z == 0
level :: Zipper a b -> Int
level = foldlThrist (.) ...forgot how to do this :(
-}
----------------------------------------------------------------------------


    ----------------
    -- CONVENIENCE
    ----------------

-- | a view function for a 'Zipper'\'s focus.
--
-- > viewf = getL focus
viewf :: Zipper a b -> b
viewf = get focus

-- | a simple type synonym for a 'Zipper' where the type at the focus is the
-- same as the type of the outer (unzippered) type. Cleans up type signatures
-- for simple recursive types:
type Zipper1 a = Zipper a a


{-
-- bind higher than <$>. Is this acceptable?:
infixl 5 .+, .>, .-, ?+, ?>, ?-

-- | 'move' with arguments flipped. Operator plays on the idea of addition of
-- levels onto the focus.
(.+) :: (Motion p, Typeable b, Typeable c)=> Zipper a b -> p b c -> Zipper a c
(.+) = flip move

-- | 'moveUp' with arguments flipped. Operator syntax comes from the idea of
-- moving up as subtraction.
(.-) :: (Typeable c, Typeable b)=> Zipper a c -> Int -> Maybe (Zipper a b)
(.-) = flip moveUp

-- | @'setL' 'focus'@, with arguments flipped
(.>) :: Zipper a b -> b -> Zipper a b
(.>) = flip (setL focus)

(?+) :: (Motion p, Typeable b, Typeable c)=> Maybe (Zipper a b) -> p b c -> Maybe (Zipper a c)
(?+) = flip (fmap . move)

(?-) :: (Typeable c, Typeable b)=> Maybe (Zipper a c) -> Int -> Maybe (Zipper a b)
mz ?- n = mz >>= moveUp n

(?>) :: Maybe (Zipper a b) -> b -> Maybe (Zipper a b)
(?>) = flip (fmap . setL focus)
-}

    ------------
    -- HELPERS
    ------------

 -- The core of our 'move' function
pivot (Z t a) (TL l) = Z (Cons h t) <$> mb
    where h = H l (Kleisli c)
          c = flip (M.set l) a 
          mb = M.get l a


 -- fold a thrist into a single category by composing the stack with (.)
 -- Here 'cat' will be either (->) or (:->):
compStack :: (Category cat)=> Thrist cat b a -> cat b a
compStack = foldrThrist (flip(.)) id


 -- Takes the zipper stack and extracts each lens segment, and recomposes
 -- them in reversed order, forming a lens from top to bottom of a data 
 -- structure:
getReverseLensStack :: ZipperStack b a -> Thrist TypeableLens a b
getReverseLensStack = unflip . foldlThrist revLocal (Flipped Nil)
-- MAKING THIS GLOBAL SHOULD PLEASE GHC 7.0 WITHOUT EXTRA EXTENSIONS. SEE:
--      http://hackage.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7
revLocal (Flipped t) (H l _) = Flipped $ Cons (TL l) t
