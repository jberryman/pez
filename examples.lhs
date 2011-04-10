> {-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeOperators, ViewPatterns #-}

The first three extensions above are almost always required when using 'pez':
    - TemplateHaskell for generating lenses via Data.Record.Label
    - TypeOperators for infix (:->) from 'fclabels' package
    - DeriveDataTypeable for deriving Typeable on user-defined types

We also use ViewPatterns which are useful for pattern matching on our zipper's
focus.

> module Main
>    where

    Import the 'pez' library (which also brings in Data.Record.Label and
Data.Typeable:

> import Data.Typeable.Zipper
> import Control.Applicative


    ------------------------------------
       EXAMPLE 1: 
           A binary tree
    ------------------------------------


    We define a simple binary search tree, deriving its Typeable instance.
Typeable "reify"s the type of some data, basically bringing some of the 
type system into the world of data.
    Further, we create accessor functions starting with an underdash. This
will let the 'fclabels' package generate lenses for our tree. See below.

> data Tree a = Node { _leftNode :: Tree a, 
>                      _val      :: a, 
>                      _rightNode :: Tree a }
>             | Nil  
>             deriving (Typeable,Show)
            

    Now we use some templete haskell provided by 'fclabels' to generate our
lenses. We use these lenses to refer to children nodes we would like to move
to.
    The code below will automatically create lenses named "leftNode", 
"rightNode", and "val" at compile time. You can see their types in ghci.

> $(mkLabelsNoTypes [''Tree])


At this point we have everything we need to work with `Tree` in a Zipper! Let's 
try it out on an example `Tree` that looks like...

                b
               / \
              a   c

> tree = Node (Node Nil 'a' Nil) 'b' (Node Nil 'c' Nil)

Let's use our zipper to apply a clockwise rotation (a rebalancing procedure) 
on the leftmost node, which in the case of the tree above would produce...

              a
               \
                b
                 \
                  c


> rotateLeftmost :: Tree Char -> Maybe (Tree Char)
> rotateLeftmost = fmap close . (doRotation =<<) . moveUp 1 . descend . zipper
>         -- travel down the left side of the tree, until reaching a Nil branch:
>     where descend z@(viewf-> Nil) = z
>           descend z               = descend $ moveTo leftNode z
>
>            -- use the Zipper1 type synonym for brevity when outer constructor
>            -- is the same as the focus:
>           doRotation :: Zipper1 (Tree Char) -> Maybe (Zipper1 (Tree Char))
>           doRotation z1@(viewf->Node l1 a1 r1) = do
>                -- navigate up one level in the zipper:
>               z0 <- moveUp 1 $ setL focus Nil z1
>                -- perform clockwise rotation:
>               let (Node _  a0 r0) = viewf z0
>                   z0' = setL focus (Node l1 a1 $ Node r1 a0 r0) z0
>               return z0'


    ------------------------------------
       EXAMPLE 1b: 
           Monadic interface
    ------------------------------------

  The code above would be a little less clunky if we used a State monad.
Specifically, we will use the State / Maybe monad transformer, and see how
the code above looks:

... > type ZipperState a = StateT (Zipper1 (Tree Char)) Maybe a
...todo when we finish the monadic interface


    ------------------------------------
       EXAMPLE 2
           Mutually-recursive types
    ------------------------------------

Typeable allows us to define 'moveUp' on mutually-recursive data types, when we
wouldn't otherwise be able to make such a function type-check. It falls on the
module user to make sure that a 'moveUp' will land us at the type we were
expecting. Here is an example:

> newtype Timer = Timer { tickTocks :: Tick } deriving Show
>
> data Tick = Tick { _tock :: Tock }
>           | Claaaannnnggg deriving (Show, Typeable)
>
> data Tock = Tock { _tick :: Tick } deriving (Show, Typeable)
>
> timer = Timer $ Tick $ Tock $ Tick $ Tock $ Claaaannnnggg

Once again we will generate the labels for the types we will pass through with
our zipper:

> $(mkLabelsNoTypes [''Tick, ''Tock])


Let's make a function that shortens the timer by one tick-tock pair. We'll also
demonstrate some of the convenience operators for moving and setting the focus,
these may change or disappear if I decide they are a bad idea:

> shortenTimer :: Timer -> Maybe Timer
> shortenTimer = fmap (Timer . close) . shortenTick . zipper . tickTocks
>     where shortenTick z@(viewf-> Claaaannnnggg) = 
>               z .- 2 ?> Claaaannnnggg
>           shortenTick z = shortenTick (z .+ tock .+ tick)

The function above would have returned Nothing from 'moveUp' had the timer not 
had at least one Tick-Tock pair, OR should we have arrived by moving up at a
type we were not expecting.
