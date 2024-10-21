module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns, as a pair of Integers
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values, map key a
  deriving (Eq, Show)

-- 1
sparseMatrix :: (Eq a, Num a) =>
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- <list of index/element pairs> -> <bounds> -> sparse matrix
-- [(row index, column index), corresponding values] (this is lst, need convert to map k a), (row, col) as bounds, and create matrix
sparseMatrix _ (row, col) | row < 1 || col < 1 = error "invalid input"
sparseMatrix pair_lst (row, col)
        | all (\((x, y), _) -> (x <= row && y <= col && x >= 1 && y >= 1)) pair_lst == False = error "invalid input"
            -- check upper and lower bounds first
        | otherwise =
            let non_zero_map = M.filter (\x -> x /= 0) (M.fromList pair_lst) in
              -- input index/value pair list, i.e. pair_lst could contain zero, filter and convert to map
              SM (row, col) (S.fromList (map (\(x, _) -> x) (M.keys non_zero_map))) (S.fromList (map (\(_, y) -> y) (M.keys non_zero_map))) non_zero_map

{- Note for convenience:
If use sparseMatrix function, input is      sparseMatrix(    pair_lst, (row, col)    );
If use SM datatype, input is                SM bound(row, col) rid cid val, or SM { bounds = , ...}
-}

-- 2
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM (row1, col1) _ _ val1) (SM (row2, col2) _ _ val2) 
        | row1 /= row2 || col1 /= col2 = error "invalid input" -- sparse matrices are not compatible, i.e. they don’t have the same number of rows or columns
        | otherwise = sparseMatrix(M.toList(M.unionWith (+) val1 val2)) (row1, col1)

-- 3
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM bound1 rid1 cid1 val1) = SM bound1 rid1 cid1 (M.map (\x -> 0 - x) val1)

-- 4
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM sm1 sm2 = addSM sm1 (negateSM sm2)

-- 5

-- helper function 1 to multiply a row of one matrix (a row vector) by a column of the other (a column vector)
mulVector :: (Eq a, Num a) => Integer -> [((Integer, Integer), a)] -> (M.Map (Integer, Integer) a) -> a
-- input: column index of SM_1, list that belongs to row1, val_SM_2
mulVector c (((_, c1), val1):rest) val2 = (mulVector c rest val2) + val1 * (M.findWithDefault 0 (c1, c) val2)
mulVector _ [] _ = 0

-- helper function 2 to map the value of mulVector to key, and convert to map key a
mulConvert :: (Eq a, Num a) => [(Integer, Integer)] -> (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a)
-- input: Cartesian product of new SM, val_SM_1, val_SM_2, and output result of values in form of map k a
mulConvert ((row1, col1):rest) val1 val2 = 
    let lst_row1 = M.toList (M.filterWithKey (\(x, _) _ -> (x == row1)) val1) in -- find the list that belongs to row1
        M.union (M.fromList [((row1, col1), (mulVector col1 lst_row1 val2))]) (mulConvert rest val1 val2)
mulConvert [] _ _ = M.empty

mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM (row1, col1) rid1 _ val1) (SM (row2, col2) _ cid2 val2)
      | col1 /= row2 = error "invalid input" -- sparse matrices are not compatible
      | otherwise = 
          let non_zero_map = M.filter (\x -> x /= 0) (mulConvert (S.toList (S.cartesianProduct rid1 cid2)) val1 val2) in -- make sure non-zero
              SM (row1, col2) (S.fromList (map (\(x, _) -> x) (M.keys non_zero_map))) (S.fromList (map (\(_, y) -> y) (M.keys non_zero_map))) non_zero_map

-- 6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (row1, col1) _ _ _) (rid, cid) | rid > row1 || cid > col1 || rid < 1 || cid < 1 = error "invalid input" -- check upper and lower bounds first
getSM (SM _ _ _ val1) key = M.findWithDefault 0 key val1

rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM (row, _) _ _ _) = row

colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM (_, col) _ _ _) = col

-- 7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM

-- 8
{-
SparseMatrix datatype is a collection of numbers, and cannot be converted from nor to Num class.
In addition, basic Num class requires functions including signum and fromInteger as shown in Part A, which cannot apply to matrices.
Therefore, it does NOT make sense to define the SparseMatrix datatype as an instance of the Num type class.
-}