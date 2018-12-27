-- my own solution of the pole problem
-- description in the book "Learn You a Haskell", chapter 12 - walk the line

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft birdsN (left,right) = if abs (newLeft - right) <= 3
                               then Just (newLeft, right)
                               else Nothing
                               where newLeft = left + birdsN

landRight :: Birds -> Pole -> Maybe Pole
landRight birdsN (left,right) = if abs (newRight - left) <= 3
                              then Just (left, newRight)
                              else Nothing
                              where newRight = right + birdsN

simulate :: [(Pole -> Maybe Pole)] -> Pole -> Maybe Pole
simulate [] p = return p
simulate (f:fs) p = (return p >>= f) >>= (simulate fs)
