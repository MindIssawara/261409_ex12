data Tree a =
      Empty
    | Node (Tree a) a (Tree a)
  deriving (Show)

-- how should we define equality for binary trees? when are two binary trees equal?
--      They have the same structure. Corresponding nodes have equal values.
equalTree :: (Eq a) => Tree a -> Tree a -> Bool
equalTree Empty Empty = True
equalTree Empty _ = False
equalTree _ Empty = False
equalTree (Node fl fv fr) (Node sl sv sr) = fv == sv && (equalTree fl sl) && (equalTree fr sr)

-- how should we define equality for lists? when are two lists equal?
--      They have the same length. Elements in both lists are equal (in order).
equalList :: (Eq a) => [a] -> [a] -> Bool
equalList [] [] = True                       
equalList [] _  = False                      
equalList _  [] = False                      
equalList (x:xs) (y:ys) = x == y && equalList xs ys 

--how should we define equality for pairs? when are two pairs equal?
--  Both elements are equal.
equalPair :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
equalPair (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

--how should we define equality for Maybe? when are two Maybe's equal?
--      Both are Nothing. Both are Just and their contained values are equal.
equalMaybe :: (Eq a) => Maybe a -> Maybe a -> Bool
equalMaybe Nothing Nothing = True
equalMaybe Nothing _ = False
equalMaybe _ Nothing = False
equalMaybe (Just x) (Just y) = x == y
