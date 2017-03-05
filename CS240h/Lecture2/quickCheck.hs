import EncodeChar
import Test.QuickCheck
import Control.Arrow (first)
import System.Random
import Data.Char

prop_encodeOne :: Char -> Bool
prop_encodeOne c = length (encodeChar c) == 1

newtype BigChar = Big Char
    deriving (Eq, Show)

instance Random BigChar where
    random = first Big `fmap` random
    randomR (Big a,Big b) = first Big `fmap` randomR (a,b)

-- random  :: RandomGen g => g -> (a, g)
-- randomR :: RandomGen g => (a,a) -> g -> (a,g)

-- or add {-# LANGUAGE GeneralizedNewtypeDeriving #-} to the front and just deriving (Eq, Show, Random)

instance Arbitrary BigChar where
    arbitrary = choose (Big '0', Big '\x10FFFF')
    shrink (Big c) = map Big (shrinkChar c)

prop_encodeOne3 :: BigChar -> Bool
prop_encodeOne3 (Big c) = length (encodeChar c) == 1


shrinkChar :: Char -> [Char]
shrinkChar c = map chr $ shrink $ ord c

prop_encodeOne2 = do 
  c <- choose('\0', '\xFFFF')
  return $ length (encodeChar c) == 1

prop_encodeOne4 (Big c) =
  (c < '\x10000') ==> length (encodeChar c) == 1

-- prop_encodeOne5 = do
--   Big c <- arbitrary `suchThat` (< Big '\x10000')
--   return $ length (encodeChar c) == 1


-- data Tree a = Node (Tree a) (Tree a)
--             | Leaf a
--               deriving (Show)


-- instance (Arbitrary a) => Arbitrary (Tree a) where
--     arbitrary = oneof [
--                   liftM  Leaf arbitrary
--                 , liftM2 Node arbitrary arbitrary
--                 ]


-- tree :: (Arbitrary a) => Int -> Gen (Tree a)
-- tree 0 = liftM Leaf arbitrary
-- tree n = oneof [
--            liftM  Leaf arbitrary
--          , liftM2 Node subtree subtree
--          ]
--   where subtree = tree (n `div` 2)








-- prop_encodeOne2 = do
--   c <- choose ('\0', '\xFFFF')
--   return $ length (encodeChar c) == 1


-- prop_encodeOne4 (Big c) =
--   (c < '\x10000') ==> length (encodeChar c) == 1

-- prop_encodeOne5 = do
--   Big c <- arbitrary `suchThat` (< Big '\x10000')
--   return $ length (encodeChar c) == 1

-- class Functor f  where
--     fmap :: (a -> b) -> f a -> f b


-- data Point a = Point a a

-- instance (Arbitrary a) => Arbitrary (Point a) where
--     arbitrary = do
--       x <- arbitrary
--       y <- arbitrary
--       return (Point x y)
-- After:

-- import Control.Monad (liftM2)

-- instance (Arbitrary a) => Arbitrary (Point a) where
--     arbitrary = liftM2 Point arbitrary arbitrary


-- data Point a = Point a a
--                deriving (Eq, Show)

-- instance (Arbitrary a) => Arbitrary (Point a) where
--     arbitrary = liftM2 Point arbitrary arbitrary
--     -- TODO: provide a body for shrink
--     shrink = undefined

-- instance (Arbitrary a) => Arbitrary (Tree a) where
--     arbitrary = sized tree

-- tree :: (Arbitrary a) => Int -> Gen (Tree a)
-- tree 0 = liftM Leaf arbitrary
-- tree n = oneof [
--            liftM  Leaf arbitrary
--          , liftM2 Node subtree subtree
--          ]
--   where subtree = tree (n `div` 2)
