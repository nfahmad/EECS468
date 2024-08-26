{-
    Name of program contained in the file: Assignment8.hs
    Brief description of the program: The aim of this project is to create a Haskell program that can parse and evaluate 
                                      arithmetic expressions in infix form containing operators +, -, *, /, %, and **, as
                                      well as numeric constants. The program should be able to handle infix
                                      expressions with parentheses to define precedence and grouping
    Inputs: None
    Output: Information to the user answering their query
    All collaborators: N/A
    Other Sources for the Code: Stack Overflow, W3Schools, ZVON.org, ChatGPT, 'Recursive Haskell Functions' Canvas Slides,
                                'Higher-Order Haskell Functions' Canvas Slides, 'Declaring Haskell Types' Canvas Slides, 
                                'Haskell Countdown Example' Canvas Slides
    Author's full name: Nabeel Ahmad
    Creation date: 04/15/2024
-}

--Definition of data type "Expr" for use in representing mathematical expressions
data Expr = Double
        --Constructor for addition expression
        | Addition Expr Expr
        --Constructor for subtraction expression
        | Subtraction Expr Expr
        --Constructor for multiplication expression
        | Multiplication Expr Expr
        --Constructor for division expression
        | Division Expr Expr
        --Constructor for modulo expression
        | Modulo Expr Expr
        --Constructor for exponentiation expression
        | Exponentiation Expr Expr
        --Constructor for negation expression
        | Negation Expr
        --Constructor for literal double value
        | Literal Double
        --The "Show" instance for "Expr" produces a string with the encoded value and it's type
        deriving(Show, Eq)
--This will determine if there is a space by taking in a char
determineSpace :: Char -> Bool
--Return "True" if there is a space
determineSpace ' ' = True
--Return "False" if there is no space
determineSpace _ = False
--This will determine if the char input is a usable value (0-9 or decimal)
determineUsableValue :: Char -> Bool
--Return True if the char falls under the contraints
determineUsableValue '0' = True
--Return True if the char falls under the contraints
determineUsableValue '1' = True
--Return True if the char falls under the contraints
determineUsableValue '2' = True
--Return True if the char falls under the contraints
determineUsableValue '3' = True
--Return True if the char falls under the contraints
determineUsableValue '4' = True
--Return True if the char falls under the contraints
determineUsableValue '5' = True
--Return True if the char falls under the contraints
determineUsableValue '6' = True
--Return True if the char falls under the contraints
determineUsableValue '7' = True
--Return True if the char falls under the contraints
determineUsableValue '8' = True
--Return True if the char falls under the contraints
determineUsableValue '9' = True
--Return True if the char falls under the contraints
determineUsableValue '.' = True
--Return False if the char does not fall under the contraints
determineUsableValue _ = False
--This will determine if the char input is a usable operator
determineUsableOperator :: Char -> Bool
--Return True if the char falls under the contraints
determineUsableOperator '+' = True
--Return True if the char falls under the contraints
determineUsableOperator '-' = True
--Return True if the char falls under the contraints
determineUsableOperator '*' = True
--Return True if the char falls under the contraints
determineUsableOperator '/' = True
--Return True if the char falls under the contraints
determineUsableOperator '%' = True
--Return True if the char falls under the contraints
determineUsableOperator '(' = True
--Return True if the char falls under the contraints
determineUsableOperator ')' = True
--Return False if the char does not fall under the contraints
determineUsableOperator _ = False
--This will determine if the expression is a valid one
determineValid :: String -> Bool
--Return True if the string is empty
determineValid [] = True
--Otherwise, use recursion to check if each character is a usable value, usable operator, or a space
determineValid (x : xs) = (determineUsableValue x || determineSpace x || determineUsableOperator x) && determineValid xs
--This will replace "**" with the proper operator "^"
exponentSign :: String -> String
--Return an empty string if the input string is empty
exponentSign [] = []
--If the input string starts with "**"", it replaces them with "^" and recursively checks the rest of the string
exponentSign ('*' : '*' : xs) = '^' : exponentSign xs
--Otherwise, keep the current character and continue recursively checking the string
exponentSign (x : xs) = x : exponentSign xs
--This will throw an error in the case of division by 0
zeroError :: Double -> Double -> Double
--If the divisor is 0, the error will be thrown
zeroError _ 0 = error "Error: divide by 0"
--Otherwise, it divides the dividend by the divisor
zeroError x y = x / y
--This will perform the modulo operation on two numbers
moduloOperation :: Double -> Double -> Double
--Declaration of the moduloOperation function
moduloOperation x y
                --If the divisor is 0, the error will be thrown
                | y == 0 = error "Error: modulo by 0"
                --If the dividend is 0, 0 will be returned
                | x == 0 = 0
                --If both numbers are negative, it recursively calculates the modulo with positive numbers
                | x < 0 && y < 0 = - moduloOperation (-x) (-y)
                --If the dividend is negative, it recursively calculates the modulo with a positive dividend
                | x < 0 = moduloOperation (x + y) y
                --If the divisor is negative, it recursively calculates the modulo with a positive divisor
                | y < 0 = moduloOperation (x + y) (y)
                --If the dividend is greater than the divisor, it recursively calculates the modulo with a reduced dividend
                | x > y = moduloOperation (x - y) y
                --Otherwise, the dividend will be returned
                | otherwise = x
--This will evaluate the expression that is being passed into it
evaluate :: Expr -> Double
--Add both operands
evaluate (Addition x y) = evaluate x + evaluate y
--Subtract both operands
evaluate (Subtraction x y) = evaluate x - evaluate y
--Multiply both operands
evaluate (Multiplication x y) = evaluate x * evaluate y
--Divide both operands
evaluate (Division x y) = zeroError (evaluate x) (evaluate y)
--Perform modulo on both operands
evaluate (Modulo x y) =  moduloOperation (evaluate x) (evaluate y)
--Perform exponentiation for both operands
evaluate (Exponentiation x y) = evaluate x ** evaluate y
--Negate the operand
evaluate (Negation y) = - (evaluate y)
--Return the value
evaluate (Literal a) = a
--This will parse binary operations in an expression
binaryOperation :: (String -> (Expr, String)) -> Char -> (Expr -> Expr -> Expr) -> String -> (Expr, String)
--Declaration of the binaryOperation function
binaryOperation next op constructor input =
    --Parse the first operand
    let (first, rest) = next input
    --Start a pattern matching on the remaining input string
    in case rest of
        {-
            If the next character in the input string is the binary operator,
            recursively parse the second operand and construct the binary operation
        -}
        (c : cs) | c == op -> let (second, finalRest) = binaryOperation next op constructor cs in (constructor first second, finalRest)
        --Otherwise, return the parsed first operand and the remaining input string
        otherwise -> (first, rest)
--This will parse an addition operation
parseAddition :: String -> (Expr, String)
--Use the binaryOperation function to parse addition, specify parseSubtraction as the next parsing function
parseAddition input = binaryOperation parseSubtraction '+' Addition input
--This will parse a subtraction operation
parseSubtraction :: String -> (Expr, String)
--Use the binaryOperation function to parse addition, specify parseModulo as the next parsing function
parseSubtraction input = binaryOperation parseModulo '-' Subtraction input
--This will parse a multiplication operation
parseMultiplication :: String -> (Expr, String)
--Use the binaryOperation function to parse addition, specify parseDivision as the next parsing function
parseMultiplication input = binaryOperation parseDivision '*' Multiplication input
----This will parse a division operation
parseDivision :: String -> (Expr, String)
--Use the binaryOperation function to parse addition, specify parseExponentiation as the next parsing function
parseDivision input = binaryOperation parseExponentiation '/' Division input
--This will parse a modulo operation
parseModulo :: String -> (Expr, String)
--Use the binaryOperation function to parse addition, specify parseMultiplication as the next parsing function
parseModulo input = binaryOperation parseMultiplication '%' Modulo input
--This will parse an exponentiation operation
parseExponentiation :: String -> (Expr, String)
--Use the binaryOperation function to parse addition, specify parseNegation as the next parsing function
parseExponentiation input = binaryOperation parseNegation '^' Exponentiation input
--This will parse a negation operation
parseNegation :: String -> (Expr, String)
--If the input string starts with a "-", parse the expression inside parentheses and construct a Negation expression
parseNegation ('-' : input) = let (expr, rest) = parseParanthesis input in (Negation expr, rest)
--Otherwise, parse the expression inside parentheses directly
parseNegation input = parseParanthesis input
--This will parse through paranthesis
parseParanthesis :: String -> (Expr, String)
--If the input string starts with "(", parse the expression inside parentheses
parseParanthesis ('(' : input) =
    --Parse the expression inside parentheses using the sendToParse function
    let (expr, rest) = sendToParse input
    --Start a pattern matching on the remaining input string
    in case rest of
        --If the expression is followed by a closing parenthesis, return the expression and the remaining string
        ')' : more -> (expr, more)
        --Otherwise, the error will be thrown
        _ -> error "Error: unmatched parentheses"
--If the input string does not start with "(", try to parse a number
parseParanthesis input =
    --Split the input string
    let (num, rest) = span determineUsableValue input
    --Determine if the number is not nothing
    in if not (null num)
        --If the number is not nothing, return it as a Literal expression along with the remaining string
        then (Literal (read num :: Double), rest)
        --If the number is nothing, the error will be thrown
        else error "Error: missing operator/operand"
--This will filter out spaces and pass the resulting string to the parseAddition function
sendToParse :: String -> (Expr, String)
--Declaration of the sendToParse function
sendToParse input =
    --Filter out spaces from the input string and parse the resulting expression using parseAddition
    let (result, rest) = parseAddition (filter (not . determineSpace) input)
    --Return the parsed expression and the remaining string after parsing
    in (result, rest)
--This will parse a mathematical expression from user input
parse :: String -> Double
--Declaration of the parse function
parse input
        --If the input string is empty, the error will be thrown
        | null input = error "Error: empty"
        --Determine if the input string is valid and then parse the expression
        | determineValid input = case sendToParse (exponentSign input) of
            --If parsing is successful and there's no remaining unparsed input, evaluate the expression
            (expr, "") -> evaluate expr
            --If there's remaining unparsed input after parsing, the error will be thrown
            (_, rest) -> error "Error: missing operator"
        --If the input string is invalid, raise an error
        | otherwise = error "Error: invalid characters"