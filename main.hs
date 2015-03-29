module ChurchNumeral where

import Data.List

type ChurchNumeral a = (a -> a) -> a -> a
type ChurchBool = Bool -> Bool -> Bool

zero, one, two :: ChurchNumeral a
zero = \f x -> x
one = \f x -> f x
two = \f x -> f (f x)

-- [f, f, f]
-- (f $ (f $ (f $ x)))
-- f (f (f x))
churchNum :: Integral a => a -> ChurchNumeral a
churchNum n = \f x -> foldr ($) x (genericReplicate n f)

unchurchNum :: Integral a => ChurchNumeral a -> a
unchurchNum n = n (+1) 0

churchSucc :: Integral a => ChurchNumeral a -> ChurchNumeral a
churchSucc n = \f x -> f (n f x)

churchAdd :: Integral a => ChurchNumeral a -> ChurchNumeral a -> ChurchNumeral a
churchAdd m n = \f x -> m f (n f x)

-- Logic

true, false :: ChurchBool
true = \x y -> x
false = \x y -> y

churchBool :: Bool -> ChurchBool
churchBool b = if b then true else false

unchurchBool :: ChurchBool -> Bool
unchurchBool b = b True False

main :: IO ()
main = do
  let three = churchNum 3          -- f (f (f x))
      four = churchNum 4           -- f (f (f (f x)))
      sum = three `churchAdd` four -- f (f (f (f (f (f (f x))))))

  putStrLn $ (show $ unchurchNum three) ++ " + " ++ (show $ unchurchNum four) ++ " = " ++ (show $ unchurchNum sum)