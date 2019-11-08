-- workshop4 Q2
-- convert 2d list to 1d
flat::[[a]]->[a]
flat [] = []
flat ([]:xs) = flat xs
flat ([a]:xs) = a: flat xs
flat ((a:as):xs) = a: (flat (as:xs))

make::[a]-> Int -> [[a]]
make [] _ = []
make list n = e: (make remain n)
    where
        e = take n list
        remain = drop n list

transpose::[[a]]->[[a]]
transpose lst = make (flat lst) (length lst)


-- workshop4 Q3
stats1 l = (length l, sum1 l, sumsq1 l)
sum1 l = foldr (+) 0 l
sumsq1 l = foldr ((+).sqrt) 0 l


stats2 [] = (0,0,0)
stats2 (n:ns) =
    let (l,s,sq) = stats2 ns
    in (l+1, s+n, sq+n*n)
    
stats3 [] = (0,0,0)
stats3 (n:ns) = (l+1, s+n, sq+n*n)
    where
        (l,s,sq) = stats3 ns

-- ws5 q2
zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 _ [] [] = []
zipWith1 _ [] (_:_) = error "zipWith: list length mismatch"
zipWith1 _ (_:_) [] = error "zipWith: list length mismatch"
zipWith1 f (x:xs) (y:ys) = (f x y):(zipWith1 f xs ys)

zWith f [] _ = []
zWith f _ [] = []
zWith f (x:xs) (y:ys) = f x y : zWith f xs ys

-- ws5 q3
linearEqn1 :: Num a => a -> a -> [a] -> [a]
linearEqn1 m n = map (\x -> m*x + n)

linearEqn2 :: Num a => a -> a -> [a] -> [a]
linearEqn2 m n lst = [m*x+n|x<-lst]

-- ws5 q4
sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
    | x  > 0    = let y = sqrt x in [y, -y] 
    | x == 0    = [0]
    | otherwise = []

-- allSqrts lst = foldr1 ((++).sqrtPM) lst
allSqrts xs = foldl (++) [] (map sqrtPM xs)