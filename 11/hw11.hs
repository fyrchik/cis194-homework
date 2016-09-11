-- forget first
(*>) :: Applicative f => f a -> f b -> f b
g *> h = flip const <$> g <*> h

-- list a -> list f b -> f list b
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA g = sequenceA . map g

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA i = sequenceA . replicate i . return
