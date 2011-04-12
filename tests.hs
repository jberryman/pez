{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ViewPatterns #-}
module Main
    where

import Test.QuickCheck
import Data.Typeable.Zipper
import Data.Record.Label.Prelude

{-
 - These tests are vital, since with all the dynamic magic we're using, a
 - function that compiles could very well not actually work
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

$(mkLabelsNoTypes [''TickTock, ''Tock, ''Tick])

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
main = sequence_
        [ quickCheck prop_simple_creation
        , quickCheck prop_simple_recursive_movement
        , quickCheck prop_mutual_saving
        ]

prop_simple_creation :: [Char] -> Bool
prop_simple_creation a = 
    let z = zipper a
        f = viewf z                           
        a' = close z                          
     in a == f && a == a'

prop_simple_recursive_movement i =
    let i' = abs i `mod` 50 :: Int
        l = replicate i' () 
         -- test simple descending
        descend 0 z | null $ viewf z = maybe False atTop $  ascend i' z
                    | otherwise = False
        descend n z = descend (n-1) (moveTo lTail z)

         -- test ascending by two and one:
        ascend 0 z = return z
        ascend 1 z = moveUp 1 z
        ascend n z = moveUp 2 z >>= ascend (n-2)
     in descend i' $ zipper l


prop_mutual_saving :: TickTock -> Bool
prop_mutual_saving tt = checkSaving $ descend $ moveTo tickTocks $ zipper tt
    where descend z@(viewf -> Gong) = z
          descend z = descendTock $ moveTo tock z
          descendTock = descend . moveTo tick
          checkSaving z = 
              let (p,a) = closeSaving z
                  z' = restore p a
                  lns = savedLens p
               -- closed zipper is equal to original, 
               in a == tt && 
               -- restoring brings us back to the end
                  viewf z' == Gong && 
               -- lens rebuilt from SavedPath is equivalent
                  getL lns tt == Gong &&
               -- moving to rebuilt lens and moving up gets us back to top:
                  (maybe False ((==tt) . viewf) $ 
                      moveUp 1 $ moveTo lns $ zipper tt)
