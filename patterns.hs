seven :: (Show a, Integral a) => a -> String
seven 7 = "Yay lucky number 7"
seven x = "Sorry "++ show x ++" is not lucky number seven"

head' :: [a] -> a
-- head' [] = error "Can't call head on an empty list"
-- head' (x:_) = x
head' xs = case xs of [] -> error "No empty lists" 
                      (x:_) -> x

tell :: (Show a) => [a] -> String
tell [] = "This list is empty"
tell [x] = "This list has one element" ++ show x
tell [x, y] = "This list has two"
tell (x:y:_) = "This list is long but"

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

caps :: String -> String
caps str@(x:xs) = "The whole string is: "++str++ " and the first letter is: "++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
    | a == b = a
    | a < b = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b = GT
    | a == b = EQ
    | a < b = LT
--
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
--
t :: Num a => a -> a
t x = 4 * (let a = 4 in x * a)