import Data.List (nub)

runSeq, runSeq' :: Int -> [Int] 
runSeq  x = x : takeWhile (\z -> (z/=1) && (z/=89)) (runSeq' x)
runSeq' x = nextX : runSeq' nextX  
        where nextX = sum $ map ((\x->x*x).read.(:[])) (show x)

is89 x  = (x/=0) && ((==89) . head . runSeq' . last . runSeq) x
combs   = [read $ foldr (:) [] [x1, x2, x3, x4, x5, x6, x7] | 
           x1 <- ['0'..'9'], x2 <- ['0'..'9'], x3 <- ['0'..'9'], 
           x4 <- ['0'..'9'], x5 <- ['0'..'9'], x6 <- ['0'..'9'], 
           x7 <- ['0'..'9'], x1<=x2, x2<=x3, x3<=x4, x4<=x5, 
           x5<=x6, x6<=x7]    :: [Int]
fact x  = product [1..x]
f7      = fact 7 
freq xs = [(c, length $ filter (== c) xs) | c <- nub xs]
perms x = let s     = filter (/='0') (show x)
              d     = length s 
              denom = map (fact.snd) (freq s)
          in  f7 `div` (fact (7-d) * product denom)

main    = putStrLn . show . sum $
    map (\x-> if is89 x then perms x else 0) combs