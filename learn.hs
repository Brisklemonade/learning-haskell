
inRange :: Int -> Int -> Int -> Bool
inRange min max x =
    let in_lower_bound = min <= x
        in_upper_bound = max >= x
    in
        in_lower_bound && in_upper_bound

boomBangs list = [if x > 10 then "BANG!" else "BOOM" | x <- list, odd x]
length' xs = sum [1 | _ <- xs]
multiArr xxs = [[ x | x <- xs, even x ] | xs <- xxs] 

triangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]