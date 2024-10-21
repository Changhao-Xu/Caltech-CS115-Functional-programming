module State where

import Control.Monad
import Control.Monad.State
import Data.IORef
import GHC.StgToJS.Types (ExpFun(result))

--
-- Part A.
--

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (do block
                whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do body
              whileState test body)

-- A.1
factIO :: Integer -> IO Integer -- computes factorials in IO
factIO n 
    | n < 0 = error "invalid negative input" 
    | otherwise = 
        do x <- newIORef n
           result <- newIORef 1
           whileIO
                (do count <- readIORef x
                    return (count > 0))
                (do count <- readIORef x
                    curr <- readIORef result
                    writeIORef result (curr * count)
                    writeIORef x (count - 1))
           readIORef result


-- A.2
factState :: Integer -> Integer -- computes factorials in state monad
factState n
    | n < 0 = error "invalid negative input" 
    | otherwise = evalState helper (n, 1)
        where
          helper :: State (Integer, Integer) Integer
          helper =
            do whileState (\(x, _) -> (x > 0))
                          (do (x, result) <- get
                              put (x - 1, result * x))
               (_, result) <- get
               return result

-- A.3
fibIO :: Integer -> IO Integer -- computes fibonacci numbers in IO
fibIO n
    | n < 0 = error "invalid negative input"  
    | otherwise = 
        do x <- newIORef n -- represent the count
           n1 <- newIORef 0
           n2 <- newIORef 1
           whileIO
                (do count <- readIORef x
                    return (count > 0))
                (do n1' <- readIORef n1
                    n2' <- readIORef n2
                    count <- readIORef x
                    writeIORef n1 n2'
                    writeIORef n2 (n1' + n2')
                    writeIORef x (count - 1)) -- decrement by 1 at each iteration
           readIORef n1 -- smaller one

-- A.4
fibState :: Integer -> Integer
fibState n
    | n < 0 = error "invalid negative input"  
    | otherwise = evalState helper (0, 1, n)
        where
          helper :: State (Integer, Integer, Integer) Integer
          helper = 
            do whileState (\(_, _, x) -> (x > 0))
                          (do (n1, n2, x) <- get
                              put (n2, n1 + n2, x - 1))
               (result, _, _) <- get
               return result
--
-- Part B.
--
{-
Given:
data Reader r b = Reader (r -> b)

runReader :: Reader r a -> r -> a
runReader (Reader f) = f

instance Monad (Reader r) where
  return x = Reader (\r -> x)

  mx >>= f = Reader (\r ->
               let x = runReader mx r in
                 runReader (f x) r)
-}
{-
Derive the definitions of return and >>=  -- Note: similar with Lec19, page 22

---------------------------------------------------------->>=--------------------------------------------------------
Assume two functions in the Reader r monad
 f :: a -> Reader r b
 g :: b -> Reader r c
, and we would like to compose them to give a function with the type signature
h :: a -> Reader r c

We first define the non-monadic forms of f, g, h:
f' :: (a, r) -> b
g' :: (b, r) -> c
h' :: (a, r) -> c
we can then define h' in terms of f' and g':
h' :: (a, r) -> c
h' (x, r) =
  let y = f' (x, r)
      z = g' (y, r)
  in (z, r)

Then we can write curried versions of them with these type signatures:
f'' :: a -> r -> b
g'' :: b -> r -> c
, and we have
f'' x rt = f' (x, rt)
g'' y rt = g' (y, rt)
Or, written slightly differently:
f'' x = \rt -> f' (x, rt)
g'' y = \rt -> g' (y, rt)

If we wrap the right-hand sides of f'' and g'' in a Reader constructor,
we have the definitions of f and g in terms of f' and g':
f :: a -> Reader r b
f x = Reader (\rt -> f' (x, rt))
g :: b -> Reader r c
g y = Reader (\rt -> g' (y, rt))
h :: a -> Reader r c
h x = Reader (\rt -> h' (x, rt))

Since
h = f >=> g  -- Lec19, page 31
h x = f x >>= g
f x >>= g = h x
f x >>= g = Reader (\rt -> h' (x, rt))
          = Reader (\rt -> let y = f' (x, rt) -- expand using definition of h'
                               z = g' (y, rt)
                           in (z, r))
          = Reader (\rt -> let y = f' (x, rt) in -- z is redundant
                            g' (y, rt))
          = Reader (\rt -> let (Reader ff) = f x -- unpack (f x)
                               y = ff rt         -- ff = \rt -> f' (x, rt)
                           in g' (y, rt))
          = Reader (\rt -> let (Reader ff) = f x
                               y = ff rt
                               (Reader gg) = g y -- unpack (g y)
                           in gg rt)             -- gg = \rt -> g' (y, rt)

Substitute mx for f x to get:
mx >>= g = Reader (\rt -> let (Reader ff) = mx
                              y = ff rt
                              (Reader gg) = g y
                          in gg rt))
We can also write it like this (using runReader):
mx >>= g = Reader (\rt -> let y = runReader mx rt
                          in runReader (g y) rt)

If we change namings, we can then get the given definitions:
mx >>= f = Reader (\r ->
             let (Reader g) = mx
                 x = g r
                 (Reader h) = f x
             in h r)
and
mx >>= f = Reader (\r ->
               let x = runReader mx r in
                 runReader (f x) r)

----------------------------------------------------------return--------------------------------------------------------
return is the monadic version of the identity function,
and the identity function would be:
id_reader :: (a, r) -> a
id_reader (x, rt) = x

id_reader' :: a -> r -> a -- curried
id_reader' x = \rt -> x

Written as a function in the (Reader r) monad, this becomes:
id_reader_monad :: a -> Reader r a
id_reader_monad x = Reader (\rt -> x)

The above identity function in the (Reader r) monad is also the return method:
return :: a -> Reader r a
return x = Reader (\rt -> x)

If we change namings, we can then get the given definitions:
return x = Reader (\r -> x)

-}