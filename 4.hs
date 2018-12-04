import Data.List
import qualified Data.Map as M

data Entry = Switch Int | WakeUp | Sleep deriving (Eq, Ord, Show)

data Time = Time Int Int Int Int deriving (Eq, Ord, Show)

parse xs = case c of
    'G' -> (Time month (if hour == -1 then day + 1 else day) hour minute, Switch idd)
    'w' -> (time, WakeUp)
    _ -> (time, Sleep)
    where
        a1 = drop 6 xs
        month = read (take 2 a1) :: Int
        a2 = drop 3 a1
        day = read (take 2 a2) :: Int
        a3 = drop 2 a2
        hour_ = read (take 3 a3) :: Int
        hour = if hour_ == 23 then -1 else 0
        a4 = drop 4 a3
        minute = read (take 2 a4) :: Int
        a5 = drop 4 a4
        c = head a5
        a6 = drop 7 a5
        idd = read $ takeWhile (/=' ') a6 :: Int
        time = Time month day hour minute

sol guard m [] = (highestMinute1*guardId1, highestMinute2*guardId2) where
    pairs = sort $ M.toList m
    flippedPairs = (\(a, b) -> (b, a)) <$> pairs
    (guardId2, highestMinute2) = snd . maximum $ flippedPairs
    guardsAndDurations = (\((g, m), v) -> (v, g)) <$> pairs
    guardId1 = snd . maximum $ go guardsAndDurations
    go [] = []
    go [x] = [x]
    go ((v1, g1):(v2, g2):xs)
        | g1 == g2 = go ((v1 + v2, g1) : xs)
        | otherwise = (v1, g1) : go ((v2, g2) : xs)
    gmins = filter (\((g, _), _) -> g == guardId1) pairs
    highestMinute1 = snd . maximum $ (\((_, m), n) -> (n, m)) <$> gmins

sol guard m ((_, Switch x):xs) = sol x m xs
sol guard m ((Time _ _ _ m1, Sleep):(Time _ _ _ m2, WakeUp):xs) = sol guard m' xs
    where
        m' = foldr go m [m1..m2-1]
        go minute mm = case M.lookup (guard, minute) mm of
            Nothing -> M.insert (guard, minute) 1 mm
            Just mins -> M.insert (guard, minute) (mins + 1) mm

main = do
    inp <- readFile "4.in"
    let parsed = sort $ parse <$> lines inp
    print $ sol 0 M.empty parsed
