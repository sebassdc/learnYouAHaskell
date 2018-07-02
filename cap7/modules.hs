import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

histogram :: (Ord a) => [a] -> [(a, Int)]
histogram = map (\l@(x:xs) -> (x, length l)) . group . sort

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- findKey :: (Eq k) => k -> [(k, v)] -> v
-- findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

-- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findkey key [] = Nothing
-- findKey key ((k, v):xs) = if key == k then Just v else findKey key xs

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBook =
    [
        ("betty","555-2938"),
        ("betty","342-2492"),
        ("bonnie","452-2928"),
        ("patsy","493-2928"),
        ("patsy","943-2929"),
        ("patsy","827-9162"),
        ("lucille","205-2928"),
        ("wendy","939-8282"),
        ("penny","853-2492"),
        ("penny","555-2111")
    ]

-- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
-- phoneBookToMap = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2)

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
