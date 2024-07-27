module CppExp.Utils(module CppExp.Utils) where
uncurry3r :: (a -> b -> c -> d) -> (a, (b, c)) -> d
uncurry3r f (a, (b, c)) = f a b c

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
