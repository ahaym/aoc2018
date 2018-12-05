import qualified Data.Map as M
import qualified Data.Set as S

main = do
    parsed <- lines <$> readFile "2.in"
    print $ addUp parsed 0 0
    case sol parsed of
        Just x : _ -> putStrLn x
        _ -> return ()

addUp [] two three = two*three
addUp (x:xs) two three = addUp xs (two + c1) (three + c2)
    where
        set = S.fromList $ counts M.empty x
        c1 = if S.member 2 set then 1 else 0
        c2 = if S.member 3 set then 1 else 0

counts m [] = map snd . M.toList $ m
counts m (x:xs) = counts m' xs
     where
        m' = case M.lookup x m of
            Nothing -> M.insert x 1 m
            Just n -> M.insert x (n+1) m

numDifferent True [] [] = Just []
numDifferent False [] [] = Nothing
numDifferent seen (x:xs) (y:ys)
    | x == y = (x:) <$> numDifferent seen xs ys
    | otherwise = if seen 
        then Nothing 
        else numDifferent True xs ys

sol xs = filter (/=Nothing) $ numDifferent False <$> xs <*> xs
