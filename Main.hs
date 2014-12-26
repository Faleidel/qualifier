{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}

import Data.Maybe

{-|
This class is for qualifying data. See Not0 for an example.
-}
class Qualifier r a where
    unwrap :: r a -> a
    wrap :: a -> Maybe (r a)

data Not0 x = Not0 x

instance (Num x, Ord x) => Qualifier Not0 x where
    unwrap (Not0 x) = x
    wrap n | n /= 0    = Just $ Not0 n
           | otherwise = Nothing

safeDiv :: (Num x, Ord x, Fractional x) => x -> Not0 x -> x
safeDiv x (unwrap -> y) = x / y

wr :: Float -> Maybe (Not0 Float)
wr = wrap

{-|
A data for lists that are not empty.
-}
data NotEmpty x = NotEmpty x

instance Qualifier NotEmpty [x] where
    unwrap (NotEmpty x) = x
    wrap x = case x of
        (e:l) -> Just $ NotEmpty x
        _     -> Nothing

safeHead :: NotEmpty [x] -> x
safeHead (unwrap -> x) = head x

main = do
    print $ fmap (\x -> safeDiv 500 x) (wr 123)
    print $ fmap (\x -> safeHead x)    (wrap [1,2,3])