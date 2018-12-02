import qualified Data.Map as M
import qualified Data.Set as S

main = do
    inp <- readFile "2.in"
    let parsed = lines inp
    print $ addUp parsed 0 0
    case head $ sol parsed of
        Just x -> putStrLn x
        Nothing -> return ()

addUp [] two three = two*three
addUp (x:xs) two three = addUp xs (two + c1) (three + c2)
    where
        pairs = (map snd . M.toList) $ counts M.empty x
        set = S.fromList pairs
        c1 = if S.member 2 set then 1 else 0
        c2 = if S.member 3 set then 1 else 0

counts m [] = m
counts m (x:xs) = counts m' xs
     where
        m' = case M.lookup x m of
            Nothing -> M.insert x 1 m
            Just n -> M.insert x (n+1) m

numDifferent True [] [] = Just []
numDifferent False [] [] = Nothing
numDifferent False (x:xs) (y:ys)
    | x == y = (x:) <$> numDifferent False xs ys
    | otherwise = numDifferent True xs ys
numDifferent True (x:xs) (y:ys)
    | x == y = (x:) <$> numDifferent True xs ys
    | otherwise = Nothing

sol xs = filter (/=Nothing) $ numDifferent False <$> xs <*> xs
