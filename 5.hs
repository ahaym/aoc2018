import Data.Char

main = do
    inp <- init <$> readFile "5.in"
    print $ solve inp
    print $ part2 inp

react c [] = (c, [])
react c [x] = (c, [x])
react c (x0:x1:xs)
    | (isLower x0 /= isLower x1)  && (toUpper x0 == toUpper x1) = react True xs
    | otherwise = let (c', xs') = react c (x1:xs) in (c', x0 : xs')

solve xs = let (reacted, xs') = react False xs in
    if reacted then solve xs' else length xs

part2 xs = minimum $ go <$> ['a'..'x']
    where
        go c = solve $ filter (\c0 -> toUpper c /= toUpper c0) xs
