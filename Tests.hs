{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ViewPatterns #-}
module Main
    where

import Test.QuickCheck 
import qualified Test.QuickCheck.Property as P
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Label.Zipper
import PreludeLenses
import Data.Label.Maybe(get)
import Data.Label(mkLabels) -- redundant, checking that no ambiguity error

import qualified Control.Category as C

{-
 - These tests are vital, since with all the dynamic magic we're using, a
 - function that compiles could very well not actually work.
 -
 - They run through a lot of tests at once and aren't very well-written,
 - certainly don't really test distinct "properties" of the code, but oh well
 -}

-- a linear, mutually recursive type:
data Tick = Tick { _tock :: Tock }
          | Gong 
          deriving (Typeable, Eq, Show)

data Tock = LoudTock { _tick :: Tick }
          | SoftTock { _tick :: Tick }
          deriving (Typeable, Eq, Show)

newtype TickTock = TT { _tickTocks :: Tick }
                   deriving (Typeable, Eq, Show)

$(mkLabels [''TickTock, ''Tock, ''Tick])

instance Arbitrary TickTock where
    arbitrary = fmap TT arbTick where
        arbTick = do
            n <- choose (1,2) :: Gen Int
            case n of
                 1 -> fmap Tick arbTock
                 2 -> return Gong

        arbTock = do
            to <- elements [LoudTock, SoftTock]
            ti <- arbTick
            return $ to ti


{-
-- a simple binary tree:
data Tree a = Branch (Tree a) (Tree a) 
            | Leaf a
            deriving (Typeable, Eq)
-}

-- we also test on simple lists 

 -- Don't know the appropriate way to run batch job:
main = defaultMain tests

tests = 
   [ testProperty "simple zipper creation" prop_simple_creation
   , testProperty "recursive descent/ascent" prop_simple_recursive_movement
   , testProperty "repeated move helpers" prop_builtin_recursive_movement
   , testProperty "various save restore functionality, with a more complex type" prop_mutual_saving
   , testProperty "test successfully-performed move up" prop_moveUpSaving
   , testProperty "moving up past top throws error" prop_simple_moveUp_past_top
   , testProperty "moving up 0 to correct type succeeds and is id" prop_move_Up_0_is_id
   ]
        

{-
-- for when we have a pure zipper type:
prop_simple_creation :: [Char] -> Bool
prop_simple_creation a = 
    let z = zipper a
        f = viewf z                           
        a' = close z                          
     in a == f && a == a'
-}

prop_simple_creation :: [Char] -> Property
prop_simple_creation a = 
    let z = zipper a
        f = viewf z                           
        ma' = close z                          
     in maybe (property False) (\a'-> a == f .&&. a == a') ma'

prop_simple_recursive_movement i =
    let i' = abs i `mod` 50 :: Int
        l = replicate i' () 
         -- when we descended the length of our list, we should be at [] ...
        descend 0 z | null $ viewf z = maybe False atTop $ ascend i' z
                    | otherwise = False
        descend n z = maybe False (descend (n-1)) (move (to lTail) z)

         -- ...then test ascending to Top
        ascend 0 z = return z
        ascend 1 z = move (Up 1) z
        ascend n z = move (Up 2) z >>= ascend (n-2)

     in descend i' $ zipper l

-- similar to above, but testing repeated move functions:
prop_builtin_recursive_movement i =
    let i' = abs i `mod` 50 :: Int
        l = replicate i' () 
        z = zipper l
        
        mz'1 = moveUntil null (to lTail) z
        z'2 = moveFloor (to lTail) z
        -- test that both have level equal to length of list:
        testLength z1 z2 | level z1 /= i' = P.failed{P.reason = "moveFloor broken" }
                         | level z2 /= i' = P.failed{P.reason = "moveUntil broken" }
                         | otherwise = P.succeeded

     in maybe (P.failed{P.reason = "moveTo lTail failed"}) 
        (testLength z'2) mz'1


--prop_mutual_saving :: TickTock -> Bool
prop_mutual_saving tt = maybe False id $
                         (move (to tickTocks) $ zipper tt) >>= descend >>= checkSaving
          -- we can compose the tick and tock motions here, and only check the
          -- predicate after moving twice:
    where descend = moveUntil (== Gong) (to tick C.. to tock)
          checkSaving z = do
              let (p,ma) = closeSaving z
              a <- ma
              z' <- restore p a
              let lns = flatten p
              -- fetch last location of zipper via flattened motion to lens:
              lensAccessedFocus <- get lns tt
                       -- closed zipper is equal to original, 
              return$ a == tt && 
                       -- restoring brings us back to the end
                      viewf z' == Gong && 
                       -- lens rebuilt from SavedPath is equivalent
                      lensAccessedFocus == Gong &&
                       -- moving to rebuilt lens and moving up gets us back to top:
                      (maybe False ((==tt) . viewf) $ 
                          move (to lns) (zipper tt) >>= move (Up 1))

-- check moveSaving Up
prop_moveUpSaving :: ((),((),(Int,Int))) -> Bool
prop_moveUpSaving tup = maybe False id $
   return (zipper tup) >>=
   move (to lSnd) >>=
   move (to lSnd) >>=
   move (to lSnd) >>=
   check
       where check :: Zipper ((),((),(Int,Int))) Int -- type sig for documentation only
                   -> Maybe Bool
             check z = do
                 let n = viewf z 
                 (p', z') <- moveSaving (Up 2 :: Up Int ((),(Int,Int)) {- correct type -}) z
                 n' <- fmap viewf $ move p' z'
                 -- we successfully moved up and back down again?:
                 return $ n == n'


------ TESTING FAILURES --------


-- TODO: check the actual exception constructors returned here, with Either:
-- test mo
prop_simple_moveUp_past_top :: [Int] -> Bool
prop_simple_moveUp_past_top l = check $ 
    move (to lTail {- possibly LensGetterFailed -}) (zipper l) >>=
    move (Up 2 :: Up [Int] [Int] {- MovePastTop -})
   
    where check = maybe True (const False)

prop_move_Up_0_is_id :: [Char] -> Bool
prop_move_Up_0_is_id s = maybe False (== s) $ move (Up 0 :: Up String String) (zipper s) >>= close
