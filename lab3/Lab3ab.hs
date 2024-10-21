module Lab3ab where

-- Part A

-- 1
data Nat1 = Zero1 | Succ1 Nat1

nat1Eq :: Nat1 -> Nat1 -> Bool
nat1Eq Zero1 Zero1 = True
nat1Eq (Succ1 x) (Succ1 y) = nat1Eq x y
nat1Eq _ _ = False

instance Eq Nat1 where
    (==) = nat1Eq
    x /= y = not (x == y)

instance Show Nat1 where
    show :: Nat1 -> String
    show Zero1 = "Zero1"
    show (Succ1 Zero1) = "Succ1 Zero1"
    show (Succ1 x) = "Succ1 (" ++ show x ++ ")"

-- 2
data Nat2 = Zero2 | Succ2 Nat2
    deriving (Eq, Show)

-- 3
instance Ord Nat2 where
    Zero2 <= _ = True
    (Succ2 x) <= (Succ2 y) = x <= y
    _ <= _ = False
{-
Having Haskell derive the Ord instance for us will work in this case.
This is because Haskell will first order the first element Zero2 <= Succ2 Nat2, which is True.
Then Haskell will recursively compare Succ2 Nat2 <= Succ2(Succ2 Nat2), which is also True.
-}

-- 4
data Nat = Zero | Succ Nat
    deriving (Eq, Show, Ord)

data SignedNat =
    Neg Nat | Pos Nat
    deriving (Show)

instance Eq SignedNat where
    Neg Zero == Pos Zero = True
    Pos Zero == Neg Zero = True
    (Neg x) == (Neg y) = x == y
    (Pos x) == (Pos y) = x == y
    _ == _ = False

instance Ord SignedNat where
    Neg _ <= Pos _ = True
    Pos Zero <= Neg Zero = True
    (Neg x) <= (Neg y) = x >= y
    (Pos x) <= (Pos y) = x <= y
    _ <= _ = False

{-
Having Haskell derive the Eq and Ord instance for us will NOT work in this case.
This is because Haskell cannot tell Neg Zero == Pos Zero.
In addition, Haskell cannot tell Neg Nat > Neg (Succ Nat),
since it will assume Neg (Succ Nat) needs more recursive cycles
and is therefore larger than less negative Neg Nat, which is not true.
-}

-- 5
-- (+) & (-)
addNat :: Nat -> Nat -> Nat -- Lec 06 P31
addNat Zero x = x
addNat x Zero = x
addNat (Succ x) y = Succ (addNat x y)

subNat :: Nat -> Nat -> Nat
subNat x Zero = x
subNat (Succ x) (Succ y) | x >= y = subNat x y
subNat _ _ = error "invalid input" -- this happens when the result <= 0

{- Initially used this, but seemed too redundant from question 7, use add and sub instead.
addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos Zero) x = x -- These defs are needed for (Succ x) and (Succ y), too ugly lol
addSignedNat x (Pos Zero) = x
addSignedNat (Neg Zero) x = x
addSignedNat x (Neg Zero) = x
addSignedNat (Pos x) (Pos y) = Pos (addNat x y)
addSignedNat (Neg x) (Neg y) = Neg (addNat x y)
addSignedNat (Pos (Succ x)) (Neg (Succ y)) = addSignedNat (Pos x) (Neg y)
addSignedNat (Neg (Succ x)) (Pos (Succ y)) = addSignedNat (Neg x) (Pos y)

negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Pos x) = Neg x
negateSignedNat (Neg x) = Pos x
-}

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos x) (Pos y) = Pos (addNat x y)
addSignedNat (Neg x) (Neg y) = Neg (addNat x y)
addSignedNat (Pos x) (Neg y) | x <= y = Neg (subNat y x)
                             | otherwise = Pos (subNat x y)
addSignedNat (Neg x) (Pos y) | x <= y = Pos (subNat y x)
                             | otherwise = Neg (subNat x y)

subSignedNat :: SignedNat -> SignedNat -> SignedNat
subSignedNat (Pos x) (Pos y) | x <= y = Neg (subNat y x)
                             | otherwise = Pos (subNat x y)
subSignedNat (Neg x) (Neg y) | x <= y = Pos (subNat y x)
                             | otherwise = Neg (subNat x y)
subSignedNat (Pos x) (Neg y) = Pos (addNat x y)
subSignedNat (Neg x) (Pos y) = Neg (addNat x y)

-- (*)
mulNat :: Nat -> Nat -> Nat -- Lec 06 P31
mulNat Zero _ = Zero
mulNat (Succ x) y = addNat y (mulNat x y)

mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Pos x) (Pos y) = Pos (mulNat x y)
mulSignedNat (Neg x) (Neg y) = Pos (mulNat x y)
mulSignedNat (Pos x) (Neg y) = Neg (mulNat x y)
mulSignedNat (Neg x) (Pos y) = Neg (mulNat x y)

-- abs
absSignedNat :: SignedNat -> SignedNat
absSignedNat (Pos x) = Pos x
absSignedNat (Neg x) = Pos x

-- signum
signumSignedNat :: SignedNat -> SignedNat
signumSignedNat (Pos Zero) = Pos Zero
signumSignedNat (Neg Zero) = Pos Zero
signumSignedNat (Pos _) = Pos (Succ Zero)
signumSignedNat (Neg _) = Neg (Succ Zero)

-- fromInteger
fromIntegerNat :: Integer -> Nat
fromIntegerNat 0 = Zero
fromIntegerNat x | x > 0 = Succ (fromIntegerNat (x - 1))
                 | otherwise = error "invalid input" -- Nat must be >= 0

fromIntegerSignedNat :: Integer -> SignedNat
fromIntegerSignedNat 0 = Pos Zero
fromIntegerSignedNat x | x > 0 = Pos (fromIntegerNat x)
                       | otherwise = Neg (fromIntegerNat (-x))

instance Num SignedNat where
    (+) = addSignedNat
    (-) = subSignedNat
    (*) = mulSignedNat
    abs = absSignedNat
    signum = signumSignedNat
    fromInteger = fromIntegerSignedNat
    negate x = (Pos Zero) - x -- this is optional

-- 6
signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger (Pos (Succ x)) = 1 + signedNatToInteger (Pos x)
signedNatToInteger (Neg (Succ x)) = -1 + signedNatToInteger (Neg x)

-- 7
{-
One thing that is a bit ugly and redundant is the existence of both (Pos Zero) and (Neg Zero),
which is equivalent to each other though I have to define extra pattern match cases.
For the new datatype UnaryInteger, we can define:

data UnaryInteger = Pred UnaryInteger | Zero | Succ UnaryInteger.

Here, Pred is the opposite to Succ.
This definition will have the problem that we don't know the absolute value of the UnaryInteger any more,
as previously defined (Neg x) has absolute value of x as an Nat that can be directly used for deriving Num instance.

To retrieve the absolute value of UnaryInteger, we can further define a absconv function:

absconvUnaryInteger :: UnaryInteger -> UnaryInteger
absconvUnaryInteger Zero = Zero
absconvUnaryInteger (Succ x) | x <= 0 = - Pred (-x)  -- Succ (-x) = -x+1 = -(x-1) = - Pred x
                             | otherwise = Succ x
absconvUnaryInteger (Pred x) | x <= 0 = - Succ (-x) -- Pred (-x) = -x-1 = -(x+1) = - Succ x
                             | otherwise = Pred x

The increased complexity of such absolute conversion is O(n)

-}

-- 8
factorial :: (Num a, Ord a) => a -> a
factorial x | x < 0 = error "invalid input"
factorial 0 = 1
factorial x = x * factorial (x-1)
-- Result: Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- Part B

-- 1
{-
>#< should be infix. This is because the result of x >#< y is string,
and we cannot further apply >#< operator to string.

(>#<) :: Integer -> Integer -> String
(>#<) x y | x == y = "Tie"
        | x > y = "First Player"
        | others = "Second Player"
infix

+| can be declared either infixr or infixl. This is because (+) operator can be either right- or left-associative,
and taking the sum of the last digit will work either way. For example,
7 +| 6 +| 5 = (7 +| 6) +| 5 = 3 +| 5 = 8
7 +| 6 +| 5 = 7 +| (6 +| 5) = 7 +| 1 = 8

(+|) :: Integer -> Integer -> Integer
(+|) x y = (x + y) 'mod' 10
infixl -- it could be infixr as well

&< should be infixl. This is because the operator appends the right argument of an integer to the end of the list,
which is left argument. infixr will append the list to an integer and will result in type error.
For example,
[1, 2] &< 3 &< 4 = ([1, 2] &< 3) &< 4 = [1, 2, 3] &< 4 = [1, 2, 3, 4]

(&<) :: [Integer] -> Integer -> [Integer]
(&<) x y = x ++ [y]
infixl

>&& should be infixr. This is because the operator “cons”es the left argument of an integer twice,
then appends it to the right argument of the beginning of a list.
infixl will append the list to the twice “cons”ed integer and will result in type error.
For example,
1 >&& 2 >&& [3, 4] = 1 >&& (2 >&& [3, 4]) = 1 >&& [2, 2, 3, 4] = [1, 1, 2, 2, 3, 4]

(>&&) :: Integer -> [Integer] -> [Integer]
(>&&) x y = [x, x] ++ y
infixr
-}

-- 2
{-
+# could be infixl, infixr, and infix.
For example,
5 +# 7 +# 9 = 2         (infixl)
            = 1         (infixr)
            = error     (infix)
9 +# 7 +# 5 = 1         (infixl)
            = 2         (infixr)
            = error     (infix)
infixl, infixr, and infix show different results, and changing input sequence will show different results as well.
Therefore, +# should be defined as infix, to prevent possible errors.
-}