{-
    Name of program contained in the file: Assignment7.hs
    Brief description of the program: This program creates an assortment of Haskell functions. This includes
                                      functions for distributing objects into boxes
    Inputs: None
    Output: Information to the user answering their query
    All collaborators: N/A
    Other Sources for the Code: Stack Overflow, W3Schools, ZVON.org, "Haskell List Comprehension" Canvas Slides,
                                "Higher-Order Haskell Functions" Canvas Slides
    Author's full name: Nabeel Ahmad
    Creation date: 04/10/2024
-}

--Takes in an integer and an element to generate a list with that element replicated a number of times specified by the integer
replicate' :: Int -> a -> [a]
--Uses list comprehension to generate a list of "y" elements repeated "x" times
replicate' x y = [y | x <- [1..x]]
--Takes in an integer to generate a list
perfects :: Int -> [Int]
--Uses list comprehension to filter numbers from 1 to "x" by checking if the sum of their proper divisors equals the number itself
perfects x = [xs | xs <- [1..x], sum [d | d <- [1..xs - 1], xs `mod` d == 0] == xs]
--Takes in an element and a list of pairs and then finds the specified element in the list of pairs
find :: Eq a => a -> [(a,b)] -> [b]
--Filters pairs whose first element matches "x", then extracts the second elements of those pairs
find x xs = map snd (filter(\(a,b) -> a == x) xs)
--Takes in an element and a list of elements and returns their positions (indices) in the list if they match the specified element
positions :: Eq a => a -> [a] -> [Int]
--Uses "find" with "zip" to pair elements of "xs" with their indices, then extracts indices where "x" occurs
positions x xs = find x (zip xs [0..])
--The scalar product of two lists of integers xs and ys is given by the sum of the products of the corresponding integers
scalarproduct :: [Int] -> [Int] -> Int
--Multiplies corresponding elements of "xs" and "ys", then sums the results
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]
--Takes in an integer and returns its factorial
factorial :: Integer -> Integer
--Declaration of the factorial function
factorial x
    --If "x" is 0, return 1
    | x == 0 = 1
    --Otherwise, multiply "x" by the factorial of "x - 1"
    | otherwise = x * factorial (x - 1)
--Calculates the number of combinations
combination :: Integer -> Integer -> Integer
--Calculates the combination using the factorial formula: n! / ((n - r)! * r!)
combination n r = factorial n `div` (factorial r * factorial (n - r))
{-
    This is a Stirling number of the second kind that tells us the number of ways that 
    a set of "n" items can be partitioned into "j" non-empty subsets
-}
stirling :: Integer -> Integer -> Integer
--Returns the Stirling number of the second kind for "n" and "j"
stirling n j
    --If both "n" and "j" are 0, return 1
    | n == 0 && j == 0 = 1
    --If either "n" or "j" is 0, return 0
    | n == 0 || j == 0 = 0
    --If "n" equals "j", return 1
    | n == j = 1
    --If "n" is less than "j", return 0
    | n < j = 0
    --Otherwise, calculate the Stirling number using recursion
    | otherwise = j * (stirling (n - 1) j) + (stirling (n - 1) (j - 1))
--Takes in two integers
question1 :: Integer -> Integer -> Integer
--Uses combination to return the answer. Counts the number of ways to distribute distinguishable objects into distinguishable boxes
question1 x y = combination x y
--Outputs the answer for the specified question
question1a = question1 52 5
--Outputs the answer for the specified question
question1b = question1 40 10
--Takes in two integers
question2 :: Integer -> Integer -> Integer
--Uses combination to return the answer. Counts the number of ways to distribute indistinguishable objects into distinguishable boxes
question2 x y = combination (x + y - 1) x
--Outputs the answer for the specified question
question2a = question2 10 8
--Outputs the answer for the specified question
question2b = question2 12 6
{-
    Takes in two integers. Uses stirling and recursion to return the answer. 
    Counts the number of ways to distribute distinguishable objects into indistinguishable boxes
-}
question3 :: Integer -> Integer -> Integer
--Declaration of the question3 function
question3 x y
    --If "y" is greater than or equal to 1, recursively calculate the sum of Stirling numbers for "x" and decreasing values of "y"
    | y >= 1 = (stirling x y) + (question3 x (y - 1))
    --Otherwise, if "y" is less than 1, return 0 because there are no more Stirling numbers to add
    | otherwise = 0
--Outputs the answer for the specified question
question3a = question3 4 3
--Outputs the answer for the specified question
question3b = question3 5 4
{-
    Takes in two integers. Uses recursion to return the answer. 
    Counts the number of ways to distribute indistinguishable objects into indistinguishable boxes
-}
question4 :: Integer -> Integer -> Integer
--Declaration of the question4 function
question4 x y
    --If "x" is 0, return 1
    | x == 0 = 1
    --If "y" is 0 or "x" is less than 1, return 0
    | y == 0 || x < 1 = 0
    --Otherwise, calculate the sum recursively
    | otherwise = (question4 (x - y) y) + (question4 x (y - 1))
--Outputs the answer for the specified question
question4a = question4 6 4
--Outputs the answer for the specified question
question4b = question4 5 3