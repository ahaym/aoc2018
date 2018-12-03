import qualified Data.Map as M
import Data.List.Split

covered ((x,y), (w,h)) = (,) <$> [x..x+w-1] <*> [y..y+h-1]

parse xs = (idd, ((d1, d2), (height, width)))
    where
        (ids, rest) = span (/='@') (tail xs)
        idd = read ids :: Int
        (dims, rest') = span (/=':') (tail rest)
        (dim1s, dr) = span (/=',') dims
        d1 = read dim1s :: Int
        d2 = read (tail dr) :: Int

        (hs, wr) = span (/='x') (tail rest')
        height = read hs :: Int
        width = read (tail wr) :: Int

part1 inp = (length ans, fst . head $ filter (\(a, b) -> works endcounts b) tupled)
    where
        tupled = (\(a, b) -> (a, covered b)) <$> inp
        endcounts = foldl tally M.empty $ snd <$> tupled
        ans = filter (\(_, a) -> a > 1) (M.toList endcounts)

works :: M.Map (Int, Int) Int -> [(Int, Int)] -> Bool
works m xs = 0 == length (filter (/=1) counts)
    where counts = map (m M.!) xs

tally :: M.Map (Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int) Int
tally = foldr go
    where
        go x mm = case M.lookup x mm of
            Nothing -> M.insert x 1 mm
            Just n -> M.insert x (n+1) mm

main = do
    inp <- readFile "3.in"
    let xs = parse <$> lines inp
    print $ part1 xs
