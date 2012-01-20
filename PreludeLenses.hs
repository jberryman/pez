{-# LANGUAGE TypeOperators #-}
module PreludeLenses
    where

import Data.Label.Maybe
import qualified Data.Label.Abstract as A
import Control.Arrow


-- First class labels pre-defined for the standard types from haskell's prelude

-- | [a]
lHead :: [a] :~> a
lHead = lens getHead (\h-> Just . (h:))
    where getHead (h:_) = Just h
          getHead []    = Nothing

lTail :: [a] :~> [a]
lTail = lens getTail setTail
    where setTail t (h:_) = Just $ h:t
          setTail t []    = Nothing

          getTail (_:t) = Just t
          getTail []    = Nothing

-- | (a,b)
--lFst :: (a,b) :~> a
--lFst = lens fst (\a (_,b)-> (a,b))
lFst :: Arrow (~>) => A.Lens (~>) (a,b) a
lFst = A.lens (arr fst) (arr $ \(a, (_,b))-> (a,b))

--lSnd :: (a,b) :-> b
lSnd :: Arrow (~>) => A.Lens (~>) (a,b) b
lSnd = A.lens (arr snd) (arr $ \(b, (a,_))-> (a,b))
