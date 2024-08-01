module CppExp.Utils(module CppExp.Utils) where

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p (x : xs)
    | p x       = xs
    | otherwise = x : removeFirst p xs
removeFirst _ [] = []

-- Copied from GHC.Internal.List.(!?)
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

uncurry3r :: (a -> b -> c -> d) -> (a, (b, c)) -> d
uncurry3r f (a, (b, c)) = f a b c

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
