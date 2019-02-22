module Tema1 (
        solveSimple,
        solveCosts
        ) where
                
import Data.Array
import Debug.Trace

data Number a = Infinite | Only a

instance Num a => Num (Number a) where
    (Only a) + (Only b) = Only (a + b)
    (Only a) + Infinite = Infinite
    Infinite + (Only a) = Infinite
    Infinite + Infinite = Infinite

    (Only a) * (Only b) = Only (a * b)
    (Only a) * Infinite = Infinite
    Infinite * (Only a) = Infinite
    Infinite * Infinite = Infinite

    abs (Only a) = Only (abs a)
    abs Infinite = Infinite

    signum (Only a) = Only (signum a)
    signum Infinite = Infinite

    fromInteger a = Only (fromInteger a)

    negate (Only a) = Only (negate a)
    negate Infinite = Infinite
-- end instance

instance Eq a => Eq (Number a) where
    (Only a) == (Only b) = a == b
    (Only a) == Infinite = False
    Infinite == (Only a) = False
    Infinite == Infinite = True
-- end instance       

instance Ord a => Ord (Number a) where
    (Only a) <= (Only b) = a <= b
    (Only a) <= Infinite = True
    Infinite <= (Only a) = False
    Infinite <= Infinite = True
-- end instance

instance Show a => Show (Number a) where
    show Infinite = "Infinite"
    show (Only a) = show a
-- end instance

solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple (n, l) = 
    let vect = listArray bounds1 [dp i | i <- range bounds1 ]

        -- define bounds
        bounds1 = (1, n)
        bounds2 = ((0, 0), (n, n))

        -- create arraylist of costs
        cost = listArray bounds2 [getCost l i j | (i, j) <- range bounds2 ]

        -- get cost for (i,j) of (j,i) edge
        getCost :: [(Int, Int, Int)] -> Int -> Int -> Number Int
        getCost [] i j = Infinite
        getCost ((first, second, cost):t) i j =   
            if first == i && second == j && i /= j then Only cost
            else if first == j && second == i && i /= j then Only cost 
            else if i == j then 0
            else getCost t i j

        -- compute all costs
        allVal :: Int -> Int -> [([Int], Number Int)]
        allVal i j
            | j == 0 = []
            | otherwise = (j:(fst (vect ! j)), ((snd (vect ! j)) + (cost ! (j, i)))):(allVal i (j - 1))

        -- miminum function
        minimul [x] = x
        minimul ((f1, s1):(f2, s2):t) = if s1 < s2 then minimul((f1, s1):t) else minimul((f2, s2):t)

        -- our recurrence
        dp :: Int -> ([Int], Number Int)
        dp i
            | i == 0 = ([], Infinite)
            | i == 1 = ([], 0)
            | otherwise = minimul (allVal i (i - 1))

        -- convert Number to Int
        toInt :: Number Int -> Int
        toInt (Only a) = a
        toInt Infinite = -1

    -- return the final result
    in case snd (vect ! n) of
        Only a -> Just (reverse (n:(fst (vect ! n))), toInt (Only a))
        Infinite -> Nothing


solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts (n, m, t, l) =
    let mat = listArray bounds1 [dp i j | (i,j) <- range bounds1]

        -- define bounds
        bounds1 = ((0,0),(n, m))
        bounds2 = (1,n)
        bounds3 = ((0,0),(n, n))   

        -- create listarrays for tax and costs
        tax = listArray bounds2 t
        cost = listArray bounds3 [getCost l i j | (i,j) <- range bounds3 ]

        -- get cost for (i,j) of (j,i) edge
        getCost :: [(Int, Int, Int)] -> Int -> Int -> Number Int
        getCost [] i j = Infinite
        getCost ((first,second,c):t) i j =
            if first == i && second == j && i /= j then Only c
            else if first == j && second == i && i /= j then Only c
            else if i == j then 0
            else getCost t i j

        -- compute all paths 
        allVal :: Int -> Int -> Int -> Maybe [([Int], Number Int)]
        allVal i j s
            | (j == 0) || ((s + (tax ! i)) > m) = Just []
            | (i == j) || (cost ! (j, i) == Infinite) = allVal i (j - 1) s
            | otherwise = 
                let pair = mat ! (j, s + (tax ! i))
                    first = case pair of 
                        Just a -> Just (fst a)
                        Nothing -> Nothing
                    second = case pair of
                        Just a -> snd a
                        Nothing -> Infinite
                    newPair = case first of
                        Just a -> j:a
                        Nothing -> []
                    head = (newPair, second + (cost ! (j, i)))
                    result = case allVal i (j - 1) s of 
                        Just a -> Just (head:a)
                        Nothing -> Nothing
                in result

        -- minimum function
        minimul [x] = x
        minimul ((f1, s1):(f2, s2):t) = if s1 < s2 then minimul((f1, s1):t) else minimul((f2, s2):t)

        -- our recurrence
        dp :: Int -> Int -> Maybe ([Int], Number Int)
        dp i s
            | i == 0 = Just ([], Infinite)
            | i == 1 = Just ([], 0)
            | otherwise = 
                let min = case allVal i n s of 
                        Just a -> if length a /= 0 then Just (minimul a) else Nothing
                        Nothing -> Nothing
                in min

        -- convert Number to Int
        toInt :: Number Int -> Int
        toInt (Only a) = a
        toInt Infinite = -1

        -- process the result
        computeResult :: Maybe ([Int], Number Int) -> Maybe ([(Int, Int)], Int)
        computeResult res = 
            let list = case res of
                    Just a -> reverse (n:(fst a))
                    Nothing -> []
                crtCost = case res of
                    Just a -> Only (snd a)
                    Nothing -> Infinite
                money [] = []
                money l = (tax ! (head l)):(money (tail l))
                start = if (length (money list) /= 0) then m : (tail (money list)) 
                        else []
                remained [] _ = []
                remained l ant = (ant - (head l)):(remained (tail l) (ant - (head l)))
                finalList = zipWith (\a b -> (a,b)) list (remained start (2 * m))
            in case crtCost of 
                Only a -> case a of 
                    Only b -> Just (finalList, b)
                    Infinite -> Nothing
                Infinite -> Nothing

        -- get the final result
        getFinalResult computeResult mat s = case computeResult (mat ! (n,s)) of
            Just a -> a:(getFinalResult computeResult mat (s+1))
            Nothing -> []

    -- return the final result
    in 
        let result = getFinalResult computeResult mat 0
        in if (length result /= 0) 
           then Just (minimul result) else Nothing
