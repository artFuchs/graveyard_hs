import Shape

data MyEnum = Um | Dois | Tres | Quatro | Cinco | Seis | Sete deriving (Enum, Ord, Eq, Bounded, Show, Read)

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]

tellNumber :: Int -> String
tellNumber 1 = "It's a One"
tellNumber 2 = "It's a Two"
tellNumber 3 = "It's a Three"
tellNumber 4 = "It's a Four"
tellNumber x = "It's a number"

sumTuples :: Num a => [(a, a)] -> [a]
sumTuples l = [ a+b | (a,b) <- l]

foo :: (Ord a, Num a, Num p) => a -> a -> p
foo x y
    | z <= 10 = 10
    | z <= 20 = 20
    | z <= 30 = 30
    | z <= 40 = 40
    | otherwise = 50
    where z = x + y

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
        | even n = n:collatz(n `div` 2)
        | odd n = n:collatz(n*3+1)

sum1 :: Num a => [a] -> [a]
sum1 [] = []
sum1 (x:xs) = (x+1):(sum1 xs)

tryUp :: Shape -> Shape -> Bool
tryUp s1 s2 = collides (translate s1 0 1) s2
