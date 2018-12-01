import qualified Data.Set as S

main = do
    inp <- readFile "1.in"
    let parsed = parse inp
    print $ sum parsed
    print $ sol2 S.empty 0 [] parsed

sol2 m n [] xs0 = sol2 m n xs0 xs0
sol2 m n (x:xs) xs0 = let cur = n + x in 
    if S.member (n + x) m 
        then n + x
        else sol2 (S.insert (n + x) m) (n + x) xs xs0

parse xs = (read . removePlus) <$> lines xs

removePlus ('+':xs) = xs
removePlus xs = xs
