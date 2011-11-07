{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell,
GADTs, DeriveDataTypeable, TupleSections,
MultiParamTypeClasses, FunctionalDependencies #-}

{- |
PEZ is a generic zipper library. It uses lenses from the "fclabels" package to
reference a \"location\" to move to in the zipper. The zipper is restricted to
types in the 'Typeable' class, allowing the user to \"move up\" through complex 
data structures such as mutually-recursive types, where the compiler could not 
otherwise type-check the program.
.
Both the Typeable class and "fclabels" lenses can be derived in GHC, making it
easy for the programmer to use a zipper with a minimum of boilerplate.
-}

module Data.Label.Zipper (
    -- * Usage
    {- |
     First import the library, which brings in the Typeable and "fclabels" modules.
     You will also want to enable a few extensions:
      
     > -- {-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeOperators #-}
     > module Main where
     >
     > import Data.Label.Zipper
      
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

    -- * Zipper functionality
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
    , Motion(..) , ReturnMotion(..)
    , Up(..) , CastUp(..) , To() , to
    -- ** Querying
    -- | a "fclabels" lens for setting, getting, and modifying the zipper's focus:
    , focus 
    , viewf , atTop , level
    -- ** Saving and recalling positions in a Zipper
    , save , closeSaving
    , restore , flatten   

    -- * Convenience operators, types, and exports
    , Zipper1

    -- ** Export Typeable class and "fclabels" package
    --, module Data.Label
    --, Data.Typeable.Typeable     
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
 -   - figure out a way to encapsulate doing a movement repeatedly:
 -      - new function move1 :: (Motion1 b)=> m b -> Zipper a b -> Maybe (Zipper a b)   
 -          (opportunities for some good stuff (see notes))
 -   - decide on minimal exports from Category and fclabels
 -   - update tests
 -      - move (Up 0) == id
 -   - clean up documentation
 -   - release 0.1.0
 -
 -   - other motion ideas:
 -      - Up to the nth level of specified type
 -      - up to the level of a specified type with focus matching predicate
 -      - Up to topmost level matching type:
 -      - repeat descend a :~> a (ToLast?)
 -      - motion down a :~> a, until matching pred.
 -   - look at Arrow instance for thrist (in module yet)
 -   - make To an instance if Iso (if possible)
 -   - pure move functionality (either separate module/namespace or new
 -      function)
 -   - Kleisli-wrapped arrow interface that works nicely with proc notation
 -   
 -   - look at usability and re-define/remove/add functions as needed, e.g.:
 -       - Create a 'moveUntil' function, or something else to capture the ugly:
 -              descend z@(viewf -> Gong) = z
 -              descend z                 = descend $ moveTo tock z
 -         ...perhaps we can make something clever using property of pattern match
 -         failure in 'do' block?
 -         - SEE IF ArrowChoice MIGHT GET US CLOSE TO WHAT WE WANT
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
 -    Particularly Elegant
 -    Pink Elephant
 -    Placebo Effect
 -    Patiently Expectant
 -    Probably ??
 -
 -}

 -- this is where the magic happens:
import Data.Label
import qualified Data.Label.Maybe as M
import Data.Typeable
import Data.Thrist

 -- for our accessors, which are a category:
import Control.Category         
import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow(Kleisli(..))
import Data.Maybe


    -------------------------
    -- TYPES: the real heros
    ------------------------


-- ZIPPER TYPE --
-----------------

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
                , hCont :: Kleisli Maybe b a -- see above
                } -> HistPair b a

type ZipperStack b a = Thrist HistPair b a

-- TODO: this could be a contravariant functor, no?:

-- | Encapsulates a data type @a@ at a focus @b@, supporting various 'Motion'
-- operations
data Zipper a b = Z { stack  :: ZipperStack b a
                    , _focus :: b                                  
                    } deriving (Typeable)
    
$(mkLabels [''Zipper])


-- MOTION CLASSES --
--------------------


-- | Types of the Motion class act as references to \"paths\" up or down 
-- through a datatype.
class Motion p where
    -- | Move through the structure to the label specified, returning 'Nothing'
    -- if the motion is invalid.
    move :: (Typeable b, Typeable c) => 
                p b c -> Zipper a b -> Maybe (Zipper a c)

-- | Defines a relation between a 'Motion' type @p@ and its return motion @p'@.
class (Motion p, Motion p')=> ReturnMotion p p' | p -> p' where
    -- | like 'move' but saves the @Motion@ that will return us back to the 
    -- location we started from in the passed zipper.
    moveSaving :: (Typeable b, Typeable c) => 
                    p b c -> Zipper a b -> Maybe (p' c b, Zipper a c)


-- MOTIONS
-------------

-- | a 'Motion' upwards in the data type. e.g. @move (Up 2)@ would move up to
-- the grandparent level, as long as the type of the focus after moving is 
-- @b@.
newtype Up c b = Up { upLevel :: Int }
    deriving (Show,Eq,Num,Ord,Integral,Bounded,Enum,Real)

instance Category Up where
    (Up m) . (Up n) = Up (m+n)
    id              = 0

instance Motion Up where
    move (Up 0)  z = gcast z
    move (Up n) (Z (Cons (H _ k) stck) c) = 
        runKleisli k c >>= move (Up (n-1)) . Z stck
    move _       _ = Nothing  

instance ReturnMotion Up To where
    moveSaving p z = (,) <$> saveFromAbove p z <*> move p z


-- | indicates a 'Motion' upwards in the zipper until we arrive at a type which
-- we can cast to @b@, returning 'Nothing' if we hit the top without a cast
-- succeeding
data CastUp c b = CastUp

instance Motion CastUp where
    move m z = getFirst $ map (`move` z) $ take (level z) (castsUp m)
        where castsUp :: CastUp c b -> [Up c b]
              castsUp _ = [1..]

              getFirst = listToMaybe . catMaybes


-- TODO: when new 'thrist' supports arbitrary Arrow instance, we can derive
-- Arrow and ArrowChoice / ArrowZero here:

-- | A 'Motion' type describing an incremental path \"down\" through a data
-- structure. This can be used with 'restore' to return to a previously-visited
-- location in a zipper, so...
--
-- > (\(l,ma)-> move l <$> ma) (closeSaving z)  ==  Just z
--
-- Use 'flatten' to turn this into a standard fclabels lens, flattening the
-- incremental move steps.
newtype To a b = S { savedLenses :: Thrist TypeableLens a b } 
    deriving (Typeable, Category)

-- We need another GADT here to enforce the Typeable constraint within the
-- hidden types in our thrist of lenses above:
data TypeableLens a b where
    TL :: (Typeable a,Typeable b)=> { tLens :: (a M.:~> b)
                                    } -> TypeableLens a b

instance Motion To where
    move = flip (foldMThrist pivot) . savedLenses  

-- | use a "fclabels" label to define a Motion \"down\" into a data type.
to :: (Typeable a, Typeable b)=> (a M.:~> b) -> To a b
to = S . flip Cons Nil . TL

instance ReturnMotion To Up where
    moveSaving p z = (Up$ lengthThrist$ savedLenses p,) <$> move p z


-- | create a zipper with the focus on the top level.
zipper :: a -> Zipper a a
zipper = Z Nil

-- | re-assembles the data structure from the top level, returning @Nothing@ if
-- the structure cannot be re-assembled.
--
-- /Note/: For standard lenses produced with 'mkLabels' this will never fail. 
-- However setters can be used to enforce arbitrary constraints on a data 
-- structure, e.g. that a type @Odd Int@ can only hold an odd integer. This
-- function returns @Nothing@ in such cases.
close :: Zipper a b -> Maybe a
close = snd . closeSaving



    ------------------------------
    -- ADVANCED ZIPPER FUNCTIONS:
    ------------------------------


data ZipperLenses a c b = ZL { zlStack :: ZipperStack b a,
                               zLenses :: Thrist TypeableLens b c }


-- INTERNAL:
saveFromAbove :: (Typeable c, Typeable b) => 
                    Up c b -> Zipper a c -> Maybe (To b c)
saveFromAbove n = fmap (S . zLenses) . mvUpSavingL (upLevel n) . flip ZL Nil . stack
    where
        mvUpSavingL :: (Typeable b', Typeable b)=> 
                        Int -> ZipperLenses a c b -> Maybe (ZipperLenses a c b')
        mvUpSavingL 0 z = gcast z
        mvUpSavingL n' (ZL (Cons (H l _) stck) ls) = 
                          mvUpSavingL (n'-1) (ZL stck $ Cons (TL l) ls)
        mvUpSavingL _ _ = Nothing



-- | Close the zipper, returning the saved path back down to the zipper\'s
-- focus. See 'close'
closeSaving :: Zipper a b -> (To a b, Maybe a)
closeSaving (Z stck b) = (S ls, ma)
    where ls = getReverseLensStack stck
          kCont = compStack $ mapThrist hCont stck
          ma = runKleisli kCont b


-- | Return a path 'To' the current location in the 'Zipper'.
-- This lets you return to a location in your data type with 'restore'.
--
-- > save = fst . closeSaving
save :: Zipper a b -> To a b
save = fst . closeSaving

-- | Extract a composed lens that points to the location we saved. This lets 
-- us modify, set or get a location that we visited with our 'Zipper' after 
-- closing the Zipper.
flatten :: (Typeable a, Typeable b)=> To a b -> (a M.:~> b)
flatten = compStack . mapThrist tLens . savedLenses


-- | Enter a zipper using the specified 'Motion'.
--
-- Saving and restoring lets us for example: find some location within our 
-- structure using a 'Zipper', save the location, 'fmap' over the entire structure,
-- and then return to where we were safely, even if the \"shape\" of our
-- structure has changed.
--
-- > restore s = move s . zipper
restore :: (Typeable a, Typeable b)=> To a b -> a -> Maybe (Zipper a b)
restore s = move s . zipper


-- | returns 'True' if 'Zipper' is at the top level of the data structure:
atTop :: Zipper a b -> Bool
atTop = nullThrist . stack

-- | Return our depth in the 'Zipper'. if 'atTop' z then @'level' z == 0@
level :: Zipper a b -> Int
level = lengthThrist . stack
----------------------------------------------------------------------------


    ----------------
    -- CONVENIENCE
    ----------------

-- | a view function for a 'Zipper'\'s focus.
--
-- > viewf = get focus
viewf :: Zipper a b -> b
viewf = get focus

-- | a simple type synonym for a 'Zipper' where the type at the focus is the
-- same as the type of the outer (unzippered) type. Cleans up type signatures
-- for simple recursive types:
type Zipper1 a = Zipper a a



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


-- this would be useful in thrist
newtype IntB a b = IntB { getInt :: Int }
plusB :: IntB a b -> IntB b c -> IntB a c
plusB a b = IntB (getInt a + getInt b)

lengthThrist :: Thrist (+>) a b -> Int
lengthThrist = getInt . foldrThrist plusB (IntB 0) . mapThrist (const $ IntB 1)
