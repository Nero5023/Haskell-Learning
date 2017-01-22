module JoinList where

import Data.Monoid

import Sized
import Buffer
import Editor

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Single _ a) 
    | index == 0 = Just a
    | otherwise = Nothing
indexJ index (Append m l r) 
    | index < 0 || index >= totalSize = Nothing
    | index < lSize       = indexJ index l
    | otherwise           = indexJ (index - lSize) r
        where totalSize = getSize $ size m
              lSize     = getSize $ size $ tag l
indexJ _ _ = Nothing


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ n (Append m l r)
    | n >= totalSize = Empty
    | n >= lSize     = dropJ (n - lSize) r
    | n >= 0         = dropJ n l +++ r
        where totalSize = getSize $ size m
              lSize     = getSize $ size $ tag l
dropJ n _  
    | n>0 = Empty


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n jl@(Append m l r)
    | n >= totalSize = jl
    | n >= lSize     = l +++ takeJ (n - lSize) r
    | n >= 0         = takeJ n l
        where totalSize = getSize . size $ m
              lSize     = getSize . size . tag $ l
takeJ n jl 
    | n > 0 = jl