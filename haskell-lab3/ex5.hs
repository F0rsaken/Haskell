import Data.List

sortDesc :: Ord a => [a] -> [a]
-- sortDesc xs = (reverse . sort) xs
sortDesc = reverse . sort

-- are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
-- are2FunsEqAt f g xs = 