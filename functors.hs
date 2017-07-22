module FunctorsExercices
    (
    ) where
      -- either e
      instance Functor (Either e) where
        fmap :: (a -> b) ->  Either e a -> Either e b
        fmap g Left x = Left x
        fmap g Right x = Right g x

      -- ((->) e).
      instance Functor ((->)e) where
        fmap ::  (a -> b) -> ((->)e a) -> ((->)e b)
        fmap g f = g . f

      -- ((,) e)
      instance Functor ((,) e) where
        fmap :: (a ->b) -> ((,)e a) -> ((,) e b)
        fmap g ((,) e x) = ((,) e (g x))

      data Pair a = Pair a a
      instance Functor Pair where
        fmap :: (a -> b) -> Pair a -> Pair b
        fmap g (Pair x y) = Pair (g x) (g y)

      data ITree a = Leaf (Int -> a) | Node [ITree a]
      instance Functor ITree where
        fmap :: (a -> b) -> ITree -> ITree
        fmap g (Leaf h) = Leaf g . h
        fmap g (Node []) = Node []
        fmap g (Node x:xs) = fmap g x : xs
