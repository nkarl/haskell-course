{-# LANGUAGE InstanceSigs #-}

import Data.Functor hiding (unzip)
import Data.Semigroup (Sum (..))
import Prelude

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 1 ------------------------------------------------
{-
Question 1: Implement the Functor instance for a RoseTree type.
A RoseTree is a tree where each node can have any number of children.
It's a bit more complicated than the binary tree we saw in the lecture,
but here's the trick to solve it: Follow the types!
-}

{- |
Application of a rose tree: https://cs.stackexchange.com/a/150397

In short, Property-based Testing is often messy and massive. That is why
when we found a failing case, we can strategically recursively shrink it until
some smaller case that still fails.

The shrinking process can be _linear_ or _logarithmic_.
-}
data RoseTree a = RoseNode a [RoseTree a] deriving (Eq, Show)

exampleRoseTree :: RoseTree Int
exampleRoseTree = RoseNode 1 [RoseNode 2 [], RoseNode 3 [RoseNode 4 []]]

-- DONE: Implement the Functor instance for RoseTree
instance Functor RoseTree where
  fmap f (RoseNode a []) = RoseNode (f a) []
  -- fmap f (RoseNode a xs) = RoseNode (f a) (fmap' f xs)
  --  where
  --    fmap' _ [] = []
  --    fmap' f (x : xs) = (f <$> x) : (fmap' f xs)
  fmap f (RoseNode a xs) = RoseNode (f a) (map (fmap f) xs)

-- Test it out:

--- >>> fmap (+1) exampleRoseTree
-- RoseNode 2 [RoseNode 3 [],RoseNode 4 [RoseNode 5 []]]

--- >>> (*2) <$> exampleRoseTree
-- RoseNode 2 [RoseNode 4 [],RoseNode 6 [RoseNode 8 []]]

--- >>> exampleRoseTree $> "Yes!"
-- RoseNode "Yes!" [RoseNode "Yes!" [],RoseNode "Yes!" [RoseNode "Yes!" []]]

--- >>> (id <$> exampleRoseTree) == id exampleRoseTree
-- True

----------------------------------------------------------------------------------------------------
---------------------------------- QUESTION 2 - Introduction ---------------------------------------
{-
You're writing an app that queries the database for a list of items (products) and then performs
several transformation on these items, such as calculating the taxes, adding them to get the
total value, etc. The database query that returns the list of items might fail, so the result of
the query is wrapped in a Maybe.
-}

-- Type representing an item (product)
newtype Item a = Item {getItem :: (String, Sum a)} deriving (Eq, Show)

exampleItem :: Item Double
exampleItem = Item ("Phone", Sum 50.0)

-- Type of the result of the database query
type DBResponse = Maybe [Item Double]

-- Example of a database query result
dbResult :: DBResponse
dbResult = Just [Item ("Phone", Sum 50.0), Item ("Glasses", Sum 30.0)]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 A ----------------------------------------------
{-
Write the Functor instance for Item.
-}

-- DONE
instance Functor Item where
  fmap :: (a -> b) -> Item a -> Item b
  fmap f = Item . (fmap . fmap) f . getItem

-- >>> fmap (*2) exampleItem
-- Item {getItem = ("Phone",Sum {getSum = 100.0})}

-- >>> fmap id exampleItem == id exampleItem
-- True

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 B ----------------------------------------------
{-
Write a function that gives you all the items of the list for free (price = 0.0).
-}

giveForFree :: DBResponse -> DBResponse
giveForFree = fmap . map $ (\x -> x $> 0.0)

-- >>> giveForFree dbResult
-- Just [Item {getItem = ("Phone",Sum {getSum = 0.0})},Item {getItem = ("Glasses",Sum {getSum = 0.0})}]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 C ----------------------------------------------
{-
Write a function that changes the products prices by applying a tax of 20%.
-}

applyTaxes :: DBResponse -> DBResponse
--           Maybe        Item Double
applyTaxes = fmap . map . fmap $ (* 1.2)

-- >>> applyTaxes dbResult
-- Just [Item {getItem = ("Phone",Sum {getSum = 60.0})},Item {getItem = ("Glasses",Sum {getSum = 36.0})}]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 D ----------------------------------------------
{-
Write a function that marks the products as discounted (in the name) and applies the discount (that
goes from 0.0 to 1.0) to the price.
-}

markOnSale :: Double -> DBResponse -> DBResponse
markOnSale perc = fmap . map $ update
 where
  tag = (<> " (" <> show (perc * 100) <> "% Discount)")
  reduce = (* (1 - perc))
  update (Item (name, price)) = Item (tag name, reduce <$> price)

-- >>> markOnSale 0.3 dbResult
-- Just [Item {getItem = ("Phone (30.0% Discount)",Sum {getSum = 35.0})},Item {getItem = ("Glasses (30.0% Discount)",Sum {getSum = 21.0})}]

----------------------------------------------------------------------------------------------------
---------------------------------------- QUESTION 2 E ----------------------------------------------
{-
Write a function that returns a pair with the list of items as first argument and the total price
as second argument.
-}

-- type DBResponse = Maybe [Item Double]
listItemsWithFinalPrice :: DBResponse -> Maybe ([String], Double)
listItemsWithFinalPrice = fmap $ ((fmap $ getSum . mconcat) . unzip) . map getItem

--                                              ([b],a) <== ([b],[a]) <== [(b, a)] <== [Item a]
-- NOTE: fmap only applies a function to the last elelement of any tuple

-- >>> listItemsWithFinalPrice dbResult
-- Just (["Phone","Glasses"],80.0)

-- >>> listItemsWithFinalPrice . applyTaxes . markOnSale 0.3 $ dbResult
-- Just (["Phone (30.0% Discount)","Glasses (30.0% Discount)"],67.2)
