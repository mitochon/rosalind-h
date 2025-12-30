--import Data.Vector.Unboxed as U
--import Data.Sequence (Seq, ViewR(..), ViewL(..), zip, dropWhileR, dropWhileL)
import Data.Sequence as S
import Data.Sequence (ViewR(..), ViewL(..)) -- zip, dropWhileR, dropWhileL)

--
--
-- >> 1 2 3 4 5 6 7 8 9 10
-- >> 3 1 5 2 7 4 9 6 10 8
-- 
-- >> 3 10 8 2 5 4 7 1 6 9
-- >> 5 2 3 1 7 4 10 8 6 9
--
-- >> 8 6 7 9 4 1 3 10 2 5
-- >> 8 2 7 6 9 1 5 3 10 4
--
-- >> 3 9 10 4 1 8 6 7 5 2
-- >> 2 9 8 5 1 7 3 4 6 10
--
-- >> 1 2 3 4 5 6 7 8 9 10
-- >> 1 2 3 4 5 6 7 8 9 10
-- 
-- >>> 9 4 5 7 0


data Action = Done | Flip | Pivot Int
            deriving (Eq, Show)

  
rear :: Eq a => Seq a -> Seq a -> Int -> Int
rear xs ys tally =
  let zs      = S.zip xs ys
      zs'     = trimLeftRight zs
      xs'     = fst <$> zs'
      ys'     = snd <$> zs'
  in case findAction zs' of
    Done    -> tally
    Flip    -> rear xs' (S.reverse ys') (tally + 1)
    Pivot p -> rear xs' (rotate p ys')  (tally + 1)


findAction :: Eq a => Seq (a, a) -> Action
findAction xs =
  let l :< ls = viewl xs
      rs :> r = viewr xs
      canFlip = (fst l == snd r) || (snd l == fst r)
      pvtIdx  = findIndexL isEq xs
  in if (S.null xs) then Done
     else if canFlip then Flip
          else maybe Done Pivot pvtIdx

{--
rotate :: Int -> Seq a -> Seq a
rotate p xs =
  let (qs, rs) = S.splitAt p xs
      y :< ys = viewl rs
  in ((S.reverse ys) |> y) >< (S.reverse qs)

rotate' :: Int -> Seq a -> Seq a
rotate' p xs =
  let (left, right) = S.splitAt p xs
      y :< ys = viewl right
      q = p - (S.length xs - p)
      (r1, r2) = S.splitAt p ys
      (l1, l2) = S.splitAt q left
  in undefined
         -- in case (((S.reverse ys) |> y) >< (S.reverse qs)     
--}

rotate :: Int -> Seq a -> Seq a
rotate p xs =
  let len = S.length xs
      (qs, rs) = S.splitAt (2 * p + 1) xs
      (qs', rs') = S.splitAt (p - (len - p) + 1) xs
  in case (2 * p + 1) > len of
    False -> (S.reverse qs) >< rs
    True -> qs' >< (S.reverse rs')
    

trimLeftRight :: Eq a => Seq (a, a) -> Seq (a, a)
trimLeftRight = (dropWhileR isEq) . (dropWhileL isEq)


isEq :: Eq a => (a, a) -> Bool
isEq x = fst x == snd x


foo xs ys = S.zip (fromList xs) (fromList ys)

main :: IO ()
main = print "hi"


{--
zip
trim left
trim right
check if fst xs == last xs
untangle
check for pivots
untangle
-- repeat, add 1
--}
