{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell,
GADTs, DeriveDataTypeable, TupleSections,
MultiParamTypeClasses, 
TypeFamilies, FlexibleContexts,
ExistentialQuantification #-}

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
     lens using functions from "fclabels":

     > data Tree a = Node { 
     >     _leftNode :: Tree a
     >   , _val      :: a 
     >   , _rightNode :: Tree a }
     >   | Nil  
     >   deriving (Typeable,Show)
     >
     > $(mkLabels [''Tree])

     Now we can go crazy using Tree in a 'Zipper':

     > treeBCD = Node (Node Nil 'b' Nil) 'c' (Node Nil 'd' Nil)
     > 
     > descendLeft :: (Typeable a)=> Zipper1 (Tree a) -> Zipper1 (Tree a)
     > descendLeft = moveFloor (to leftNode) -- stops at Nil constructor
     >
     > insertLeftmost :: (Typeable a)=> a -> Tree a -> Maybe (Tree a)
     > insertLeftmost a = close . setf newNode . descendLeft . zipper
     >     where newNode = Node Nil a Nil
     >
     > treeABCD = insertLeftmost 'a' treeBCD
      
     Because of the flexibility of "fclabels", this zipper library can be used to
     express moving about in reversible computations simply by defining such a lens,
     for instance:
      
     > stringRep :: (Show b, Read b) => b :-> String
     > stringRep = lens show (const . read)

     Another exciting possibility are zippers that can perform validation,
     refusing to 'close' if a field is rejected.
    -}

    -- * Zipper functionality
    Zipper() 
    {- |
       /A note on failure in zipper operations:/

       Most operations on a 'Zipper' return a result in a 'Failure' class
       monad, throwing various types of failures. Here is a list of failure
       scenarios:

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

       See the "failure" package for details.
    -}
    -- ** Creating and closing Zippers
    , zipper , close
    -- ** Moving around
    , Motion(..) 
    , Up(..) , UpCasting(..) , To() , to 
    --, Flatten(..)
    -- *** Error types
    {- |
       Every defined 'Motion' has an associated error type, thrown in a
       'Failure' class monad (see "failure"). These types are also have a small
       'Exception' hierarchy.
    -}
    , ZipperException() , UpErrors(..) , ToErrors(..)
    -- *** Repeating movements
    , moveWhile
    , moveUntil
    , moveFloor
    -- ** The zipper focus
    -- | a "fclabels" lens for setting, getting, and modifying the zipper's
    -- focus. Note: a zipper may fail to 'close' if the lens used to reach the
    -- current focus performed some validation.
    , focus 
    , viewf , setf , modf
    -- ** Querying Zippers and Motions
    , atTop , level
    , LevelDelta(..)
    -- ** Saving and recalling positions in a Zipper
    , save , closeSaving
    , restore , flatten   

    -- * Convenience operators, types, and exports
    , Zipper1

    -- ** Re-exports
    , Data.Typeable.Typeable(..)
    , Data.Label.mkLabels
    , (M.:~>)
    , Control.Failure.Failure(..)
    , Control.Exception.Exception(..)
) where



{- 
 -   IMPLEMENTATION NOTES:
 -
 -   we use a Thrist to create a type-threaded stack of continuations
 -   allowing us to have a polymorphic history of where we've been.
 -   By using Typeable, we are able to "move up" by type casting in
 -   the Maybe monad. This means that the programmer has to know
 -   what type a move up will produce, or deal with unknowns.
 -
 -
 -   TODO NOTES
 -   - clean up documentation, code samples
 -   - release 0.1.0
 -
 -   - NEXT TODO
 -   ------------
 -   - complete code coverage
 -   - make error types return more useful info: height above where constructor
 -     failed, typeRep of the failure,
 -   - implement focusValid, or a better solution.
 -   - can we define appropriate instances to allow, e.g. `move -2` ?
 -   - pure move functionality (either separate module/namespace or new
 -      function)
 -      - pureMove :: (PureMotion m)=>
 -   - re. above: also see note under CONVENIENCE: can we use a mechanism
 -      similar to what fclabels uses on generated zippers to force the use of
 -      e.g. focusSafe on a zipper where we have used 'To' with a failable lens,
 -      forcing a close function that would return Maybe, etc.
 -
 -      We should provide a function validate :: FallibleZipper -> ClosableZipper, which allows validation at any one time
 -      Then, moveFallible :: z -> FallibleZipper, move :: z -> z
 -
 -      But there is a real question with fclabels that has come up:
 -          1) basic lenses that can fail only ever fail (because of multiple
    -          constructors) on the getter, yet underlying type can fail in setter
 -             too. This adds needless fallability to our close function
 -          2) we might like (as we want in focusValid below) to have a lens
    -          that ONLY fails on a setter (does validation), but which always
 -             succeeds in a getter (has a single constructor for instance)
 -      
 -
 -   - conversion from motions to fclabels (:~>)
 -   - add Flatten motion down that collapses history?
 -      - doesn't make sense for motion from top level. return Nothing?
 -   - other motion ideas:
 -      - Up to the nth level of specified type
 -      - up to the level of a specified type with focus matching predicate
 -      - Up to topmost level matching type:
 -      - repeat descend a :~> a (ToLast?)
 -      - motion down a :~> a, until matching pred.
 -   - look at Arrow instance for thrist (in module yet)
 -   - make To an instance if Iso (if possible)
 -   - Kleisli-wrapped arrow interface that works nicely with proc notation
 -
 -   PERFORMANCE TODO
 -   -----------------
 -   - consider instead of using section, use head form of parent with
 -     the child node set to undefined. Any performance difference?
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
import Control.Monad
import Control.Failure
import Control.Exception


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

--TODO NOTE: this is the class we would like, however this causes a cycle
--because of superclass declaration of Motion. see this thread: 
--    http://www.haskell.org/pipermail/glasgow-haskell-users/2011-July/020585.html
--class (Exception (ThrownBy mot), Motion (Returning mot))=> Motion mot where

-- | Types of the Motion class describe \"paths\" up or down (so to speak)
-- through a datatype. The exceptions thrown by each motion are enumerated in
-- the associated type @ThrownBy mot@. The @Motion@ type that will return the
-- focus to the last location after doing a 'moveSaving is given by @Returning mot@.
class (Exception (ThrownBy mot))=> Motion mot where
    type ThrownBy mot :: *
    type Returning mot :: * -> * -> *

    -- | Move to a new location in the zipper, either returning the new zipper,
    -- or throwing @err@ in some @Failure@ class type (from the "failure" pkg.)
    --
    -- The return type can be treated as @Maybe@ for simple exception handling
    -- or one can even use something like "control-monad-exception" to get 
    -- powerful typed, checked exceptions.
    move :: (Typeable b, Typeable c, Failure (ThrownBy mot) m) => 
                mot b c -> Zipper a b -> m (Zipper a c)
    move mot z = moveSaving mot z >>= return . snd

    -- | like 'move' but saves the @Motion@ that will return us back to the 
    -- location we started from in the passed zipper.
    moveSaving :: (Typeable b, Typeable c, Failure (ThrownBy mot) m) => 
                    mot b c -> Zipper a b -> m ((Returning mot) c b, Zipper a c)



-- MOTIONS
-------------

-- | a 'Motion' upwards in the data type. e.g. @move (Up 2)@ would move up to
-- the grandparent level, as long as the type of the focus after moving is 
-- @b@. This 'Motion' type throws 'UpErrors'
newtype Up c b = Up { upLevel :: Int }
    deriving (Show,Num,Integral,Eq,Ord,Bounded,Enum,Real)

data UpErrors = CastFailed
              | LensSetterFailed
              | MovePastTop
              deriving (Show,Typeable,Eq)


{-
--TODO: THIS IS PROBABLY NOT A GGOD IDEA UNLESS WE CAN DO IT RIGHT. AT THE
--MOMENT I DON'T UNDERSTAND HOW GHC DOES SOMETHING LIKE:
--      [-1,-2..-3] :: [ Up Int Int]
-- BUT THE FOLLOWING CODE ISN'T ENOUGH. FOR NOW DERIVE NUMERIC CLASSES ABOVE AND
-- DO NOT DOCUMENT USING `move 3`.
-- | 'fromInteger' gets defined as @Up . abs@, so @move (Up 2)@ is equivalent to
-- @move (-2)@.
instance Num (Up a b) where
    (Up a) + (Up b) = Up $ a+b
    (Up a) - (Up b) = Up $ a-b
    (Up a) * (Up b) = Up $ a*b
    abs (Up n)      = Up $ abs n
    signum (Up n)   = Up $ signum n
    fromInteger n   = Up $ fromInteger $ abs n

instance Integral (Up a b) where
    toInteger (Up n) = toInteger $ negate $ abs n
    quotRem (Up a) (Up b) = (Up $ quot a b, Up $ rem a b)

-- also need fromEnum and fromIntegral?
-}

instance Category Up where
    (Up m) . (Up n) = Up (m+n)
    id              = 0

instance Motion Up where
    type ThrownBy Up = UpErrors
    type Returning Up = To

    move (Up 0)  z = 
        maybeThrow CastFailed $ gcast z
    move (Up n) (Z (Cons (H _ k) stck) c) = 
        maybeThrow LensSetterFailed (runKleisli k c) >>= 
        move (Up (n-1)) . Z stck
    move _ _ = 
        failure MovePastTop

    -- TODO: it makes more sense to define 'move' and 'saveFromAbove' in terms
    -- of moveSaving below, but we ran into some type weirdness, so...
    moveSaving p z = liftM2 (,) (saveFromAbove p z) (move p z)


-- | indicates a 'Motion' upwards in the zipper until we arrive at a type which
-- we can cast to @b@, otherwise throwing 'UpErrors'
data UpCasting c b = UpCasting
    deriving(Show,Typeable,Eq)


instance Motion UpCasting where
    type ThrownBy UpCasting = UpErrors
    type Returning UpCasting = To

    moveSaving p z = do 
        when (atTop z) $ failure MovePastTop
        firstSuccess $ map (flip ms z) [Up 1 ..]
        where ms = moveSaving :: (Typeable b, Typeable c)=>Up c b -> Zipper a c -> Either UpErrors (To b c, Zipper a b)
              firstSuccess []                            = failure CastFailed
               -- this would be raised on each of it's ancestors: 
              firstSuccess ((Left LensSetterFailed):_) = failure LensSetterFailed
               -- if cast failed, skip:
              firstSuccess ((Left CastFailed):zms)     = firstSuccess zms
              firstSuccess ((Right (m,z')):_)          = return (m,z')
              firstSuccess _ = error "bug in move UpCasting"


-- | A 'Motion' type describing an incremental path \"down\" through a data
-- structure. Use 'to' to move to a location specified by a "fclabels" lens.
--
-- Use 'restore' to return to a previously-visited location in a zipper, with
-- previous history intact, so:
--
-- > (\(l,ma)-> move l <$> ma) (closeSaving z)  ==  Just z
--
-- Use 'flatten' to turn this into a standard fclabels lens, flattening the
-- incremental move steps.
--
-- Throws errors of type 'ToErrors':
newtype To a b = S { savedLenses :: Thrist TypeableLens a b } 
    deriving (Typeable, Category)

-- We need another GADT here to enforce the Typeable constraint within the
-- hidden types in our thrist of lenses above:
data TypeableLens a b where
    TL :: (Typeable a,Typeable b)=> { tLens :: (a M.:~> b)
                                    } -> TypeableLens a b

-- TODO: we might store some info here re. at what level the error occured:
data ToErrors = LensGetterFailed
    deriving(Show,Typeable,Eq)

instance Motion To where
    type ThrownBy To = ToErrors
    type Returning To = Up

    move mot z = maybeThrow LensGetterFailed $ 
        foldMThrist pivot z $ savedLenses mot

    moveSaving p z = do z' <- move p z
                        let motS = Up $ lengthThrist $ savedLenses p
                        return (motS,z')

-- | use a "fclabels" label to define a Motion \"down\" into a data type.
to :: (Typeable a, Typeable b)=> (a M.:~> b) -> To a b
to = S . flip Cons Nil . TL



{-  TODO for next version
-- | a 'Motion' \"down\" that squashes the saved history of the motion, so for
-- instance:
--
-- > level $ move (Flatten l) z  ==  level z
--
-- and:
--
-- > move (Up 1) z  ==  move (Up 1) $ move (Flatten l) z
newtype Flatten a b = Flatten (To a b) 
    deriving (Typeable, Category)

instance Motion Flatten where
    move m z = undefined --flip (foldMThrist pivot) . savedLenses  
-}

--------------- REPEATED MOTIONS -----------------

-- | Apply the given Motion to a zipper until the Motion fails, returning the
-- last location visited. For instance @moveFloor (to left) z@ might return
-- the left-most node of a 'zipper'ed tree @z@.
-- 
-- > moveFloor m z = maybe z (moveFloor m) $ move m z
moveFloor :: (Motion m,Typeable a, Typeable b)=> 
                 m b b -> Zipper a b -> Zipper a b
moveFloor m z = maybe z (moveFloor m) (move m z)

-- | Apply a motion each time the focus matches the predicate, raising an error
-- in @m@ otherwise
moveWhile :: (Failure (ThrownBy mot) m, Motion mot, Typeable c) =>
              (c -> Bool) -> mot c c -> Zipper a c -> m (Zipper a c)
moveWhile p m z | p $ viewf z = move m z >>= moveWhile p m
                | otherwise   = return z

{-
-- THIS SEEMS NOT TERRIBLY USEFUL, AND WAS CONFUSING EVEN ME
--
-- | Apply a motion one or more times until the predicate applied to the focus
-- returns @True@, otherwise raising an error in @m@ if a 'move' fails before
-- we reach a focus that matches.
moveUntil :: (Failure (ThrownBy mot) m, Motion mot, Typeable c) =>
              (c -> Bool) -> mot c c -> Zipper a c -> m (Zipper a c)
moveUntil p m z = move m z >>= maybeLoop
    where maybeLoop z' | p $ viewf z' = return z'
                       | otherwise    = moveUntil p m z'
-}


-- | Apply a motion zero or more times until the focus matches the predicate
--
-- > moveUntil p = moveWhile (not . p)
moveUntil :: (Failure (ThrownBy mot) m, Motion mot, Typeable c) =>
              (c -> Bool) -> mot c c -> Zipper a c -> m (Zipper a c)
moveUntil p = moveWhile (not . p)


-- TODO: consider:
--     moveWhen
--     moveUnless

--------------- 

-- | create a zipper with the focus on the top level.
zipper :: a -> Zipper a a
zipper = Z Nil



    ------------------------------
    -- ADVANCED ZIPPER FUNCTIONS:
    ------------------------------


data ZipperLenses a c b = ZL { zlStack :: ZipperStack b a,
                               zLenses :: Thrist TypeableLens b c }

-- INTERNAL FOR NOW:
saveFromAbove :: (Typeable c, Typeable b, Failure UpErrors m) => 
                    Up c b -> Zipper a c -> m (To b c)
saveFromAbove n = liftM (S . zLenses) . mvUpSavingL (upLevel n) . flip ZL Nil . stack
    where mvUpSavingL :: (Typeable b', Typeable b, Failure UpErrors m)=> 
                          Int -> ZipperLenses a c b -> m (ZipperLenses a c b')
          mvUpSavingL 0 z = 
              maybeThrow CastFailed $ gcast z
          mvUpSavingL n' (ZL (Cons (H l _) stck) ls) = 
              mvUpSavingL (n'-1) (ZL stck $ Cons (TL l) ls)
          mvUpSavingL _ _ = failure MovePastTop
        

-- | Close the zipper, returning the saved path back down to the zipper\'s
-- focus. See 'close'
closeSaving :: Zipper a b -> (To a b, Maybe a)
closeSaving (Z stck b) = (S ls, ma)
    where ls = getReverseLensStack stck
          kCont = compStack $ mapThrist hCont stck
          ma = runKleisli kCont b


-- TODO: consider that if we stick with fclabels-generated lenses here, there
-- isn't any conceptual reason why such lenses whould have to fail on their
-- setters, and why 'close' should have to fail here:
-- I guess this would require an implementation of M.lens like:
--
--     lens :: (f -> Maybe a) -> (f -> Maybe (a -> f)) -> f :~> a
-- e.g. lLeft = lens lGet lSet where
--           lGet (Left a) = Just a
--           lGet _ = Nothing
--           lSet (Left a) = Just (\a'-> Left a') -- if the type had multiple params they would be preserved of course
--           lSet _ = Nothing
--
--        ...so is (Just $\a-> Left a) an arrow at this point? 

-- | re-assembles the data structure from the top level, returning @Nothing@ if
-- the structure cannot be re-assembled.
--
-- /Note/: For standard lenses produced with 'mkLabels' this will never fail.
-- However setters defined by hand with 'lens' can be used to enforce arbitrary
-- constraints on a data structure, e.g. that a type @Odd Int@ can only hold an
-- odd integer.  This function returns @Nothing@ in such cases, which
-- corresponds to the @LensSetterFailed@ constructor of 'UpErrors'
close :: Zipper a b -> Maybe a
close = snd . closeSaving



-- | Return a path 'To' the current location in the 'Zipper'.
-- This lets you return to a location in your data type with 'restore'.
--
-- > save = fst . closeSaving
save :: Zipper a b -> To a b
save = fst . closeSaving


-- TODO: consider making flatten polymorphic over: To, Zipper, etc. and change name to toLens

-- | Extract a composed lens that points to the location we saved. This lets 
-- us modify, set or get a location that we visited with our 'Zipper', after 
-- closing the Zipper, using "fclabels" @get@ and @set@.
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
restore :: (Typeable a, Typeable b, Failure ToErrors m)=> To a b -> a -> m (Zipper a b)
restore s = move s . zipper


-- | returns 'True' if 'Zipper' is at the top level of the data structure:
atTop :: Zipper a b -> Bool
atTop = nullThrist . stack


-- | Return our zero-indexed depth in the 'Zipper'. 
-- if 'atTop' zipper then @'level' zipper == 0@
level :: Zipper a b -> Int
level = lengthThrist . stack

-- | Motion types which alter a Zipper by a knowable integer quantity.
-- Concretly, the following should hold:
--
-- > level (move m z) == level z + delta m
--
-- For motions upwards this returns a negative value.
class (Motion m)=> LevelDelta m where
    delta :: (Typeable a, Typeable b)=>m a b -> Int

instance LevelDelta Up where
    delta = negate . upLevel

instance LevelDelta To where
    delta = lengthThrist . savedLenses

{- TODO maybe in next version
instance LevelDelta Flatten where
    delta = const 0
-}

----------------------------------------------------------------------------


    ----------------
    -- CONVENIENCE
    ----------------

-- TODO: we should at least export a lens 'focusM' or 'focusSafe'that fails
-- when the zipper fails validation (i.e. can't be closed) . There are probably
-- some clever polymorphic solutions similar to what fclabels itself does to
-- force use of focusSafe when we've moved with a failable lens, vs. a zipper
-- untainted by failable lenses in history (in which case 'close' will never
-- fail).


-- | a view function for a Zipper\'s 'focus'.
--
-- > viewf = get focus
viewf :: Zipper a b -> b
viewf = get focus

-- | modify the Zipper\'s 'focus'.
--
-- > modf = modify focus
modf :: (b -> b) -> Zipper a b -> Zipper a b
modf = modify focus

-- | set the Zipper\'s 'focus'.
-- 
-- > setf = set focus
setf :: b -> Zipper a b -> Zipper a b
setf = set focus

-- | a simple type synonym for a 'Zipper' where the type at the focus is the
-- same as the type of the outer (unzippered) type. Cleans up type signatures
-- for simple recursive types:
type Zipper1 a = Zipper a a



    ------------
    -- HELPERS
    ------------

 -- The core of move To
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


maybeThrow :: (Failure e m)=> e -> Maybe a -> m a
maybeThrow e = maybe (failure e) return


    ----------------------
    -- EXCEPTION HIERARCHY
    ----------------------

-- NOTE: a 'Throws' hierarchy must be defined manually for c-m-e. Perhaps we
-- should create a separate package with those instances defined

-- | The root of the exception hierarchy for Zipper 'move' operations:
data ZipperException = forall e . Exception e => ZipperException e
     deriving (Typeable)

instance Show ZipperException where
    show (ZipperException e) = show e

instance Exception ZipperException

instance Exception UpErrors where
    toException = toException . ZipperException
    fromException x = do
        ZipperException a <- fromException x
        cast a

instance Exception ToErrors where
    toException = toException . ZipperException
    fromException x = do
        ZipperException a <- fromException x
        cast a
