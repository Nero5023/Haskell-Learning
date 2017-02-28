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
  c <- choose ('\0', '\xFFFF')
  return $ length (encodeChar c) == 1


prop_encodeOne4 (Big c) =
  (c < '\x10000') ==> length (encodeChar c) == 1

prop_encodeOne5 = do
  Big c <- arbitrary `suchThat` (< Big '\x10000')
  return $ length (encodeChar c) == 1

class Functor f  where
    fmap :: (a -> b) -> f a -> f b