-- Harel Zahari
-- 305494452

-- Naor Zaharia
-- 312423841

module HW4 where
 
--Ex1A

data TokenType = TokLeftPar | TokRightPar | TokConstant | TokVar | TokOp deriving(Show, Eq)
type Token = (TokenType, String)

lexer:: String -> [Token]
lexer [] = []
lexer ('+':xs) = (TokOp, "+") : lexer xs
lexer ('-':xs) = (TokOp, "-") : lexer xs
lexer ('/':xs) = (TokOp, "/") : lexer xs
lexer ('*':xs) = (TokOp, "*") : lexer xs
lexer (')':xs) = (TokRightPar, ")") : lexer xs
lexer ('(':xs) = (TokLeftPar, "(") : lexer xs
lexer (' ':xs) = lexer xs --Igonre space
lexer (ch:xs) = if elem ch ['0'..'9']
                    then 
                         let currentStr = (ch : getConstantString xs) in
                         (TokConstant, currentStr) : lexer (drop (length currentStr) xs)
                    else 
                         let currentStr = (ch : getVarString xs) in
                         (TokVar, currentStr) : lexer (drop (length currentStr) xs)

getConstantString :: String -> String
getConstantString [] = []
getConstantString (x:xs) = if elem x ['0'..'9']
                              then x:getConstantString xs
                              else []

getVarString :: String -> String
getVarString [] = []
getVarString (x:xs) = if elem x ['a'..'z'] || elem x ['A'..'Z'] || elem x ['0'..'9']
                              then x:getVarString xs
                              else []
--Ex1B

data Op = Addition | Subtraction | Multiplication | Division deriving(Show, Eq)
data Expr = Const Int | Variable String | ArithmeticExp Expr Op Expr | BrackExpr Expr deriving(Show, Eq) 

--Ex2B

exampleA = Const 5
exampleB = ArithmeticExp (Variable "x") Addition (Const 5)
exampleC = ArithmeticExp (ArithmeticExp (Variable "y") Addition (ArithmeticExp (Const 2) Multiplication (Variable "x"))) Subtraction (Const 3)
exampleD = ArithmeticExp (Variable "y") Addition (ArithmeticExp (Const 2) Multiplication (BrackExpr (ArithmeticExp (Variable "x") Subtraction (Const 3))))

--Ex3B

type Dictionary = [(String, Int)]

calc_expr :: Expr -> Dictionary -> Maybe Int
calc_expr (Const c) _ = Just c
calc_expr (Variable var) dict = getVarFromDictionary var dict
calc_expr (ArithmeticExp exp1 operation exp2) dict = getMabyeIntFromExpr (calc_expr exp1 dict) (calc_expr exp2 dict) operation
calc_expr (BrackExpr exp) dict = calc_expr exp dict

getVarFromDictionary :: String -> Dictionary -> Maybe Int
getVarFromDictionary var dict = let filterDict = filter (\(a,_) -> a == var) dict in  
                               if(length filterDict == 1)
                                    then Just (snd (head filterDict))
                                    else Nothing

getMabyeIntFromExpr :: Maybe Int -> Maybe Int -> Op -> Maybe Int
getMabyeIntFromExpr Nothing _ _ = Nothing
getMabyeIntFromExpr _ Nothing _ = Nothing
getMabyeIntFromExpr (Just n1) (Just n2) Addition = Just (n1 + n2)
getMabyeIntFromExpr (Just n1) (Just n2) Subtraction = Just (n1 - n2)
getMabyeIntFromExpr (Just n1) (Just n2) Multiplication = Just (n1 * n2)
getMabyeIntFromExpr (Just n1) (Just n2) Division = if n2==0
                                                       then Nothing 
                                                       else Just (n1 `div` n2)