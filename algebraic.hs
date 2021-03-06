data Expr = Val Int
          | Expr :+: Expr
          | Expr :-: Expr
          deriving Show

expr1 = Val 3 :+: Val 4
expr2 = expr1 :-: Val 2
expr3 = Val 3 :+: (Val 2 :-: Val 4)

interpret :: Expr -> Int
interpret (Val x)   = x
interpret (x :+: y) = interpret x + interpret y
interpret (x :-: y) = interpret x - interpret y

data Instr = PUSH Int
           | ADD
           | SUB
           deriving Show

compile :: Expr -> [Instr]
compile (Val v)   = [PUSH v]
compile (x :+: y) = compile x ++ compile y ++ [ADD]
compile (x :-: y) = compile x ++ compile y ++ [SUB]

data ExprAlgebra e = ExprAlgebra
                   { val :: Int -> e
                   , add :: e -> e -> e
                   , sub :: e -> e -> e
                   }

foldExpr :: ExprAlgebra a -> Expr -> a
foldExpr alg (Val i)     = (val alg) i
foldExpr alg (e1 :+: e2) = (add alg) (foldExpr alg e1) (foldExpr alg e2)
foldExpr alg (e1 :-: e2) = (sub alg) (foldExpr alg e1) (foldExpr alg e2)

interpreter :: ExprAlgebra Int
interpreter = ExprAlgebra
            { val = id
            , add = (+)
            , sub = (-)
            }

compiler :: ExprAlgebra [Instr]
compiler = ExprAlgebra
         { val = \x -> [PUSH x]
         , add = \x y -> x ++ y ++ [ADD]
         , sub = \x y -> x ++ y ++ [SUB]
         }

interpret' :: Expr -> Int
interpret' = foldExpr interpreter

compile' :: Expr -> [Instr]
compile' = foldExpr compiler