{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module List where

infixr 5 :!:
data FList a = Empty | a :!: FList a deriving (Show)

fromList :: [a] -> FList a
fromList [] = Empty
fromList (x:xs) = x :!: fromList xs

asList :: FList a -> [a]
asList Empty = []
asList (x :!: xs) = x : asList xs

(.++) :: FList a -> FList a -> FList a
(.++) Empty l = l
(.++) (x:!:xs) l = x :!: (xs .++ l)

at :: FList a -> Int -> a
at (x:!:xs) 0 = x
at (x:!:xs) n = at xs (n-1)

map' :: (a -> a) -> FList a -> FList a
map' f Empty = Empty
map' f (x :!: xs) = f x :!: map' f xs

filter' :: (a -> Bool) -> FList a -> FList a
filter' f Empty = Empty
filter' f (x :!: xs)
    | f x = x :!: filter' f xs
    | otherwise = filter' f xs