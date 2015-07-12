data Expr = Val Int
	      | Expr :+: Expr
	      | Expr :-: Expr


expr1 = Val 3 :+: Val 4
expr2 = expr1 :-: Val 2
expr3 = Val 3 :+: (Val 2 :-: Val 4)


interpret :: Expr -> Int
interpret (Val x)   = x
interpret (x :+: y)	= interpret x + interpret y
interpret (x :-: y) = interpret x - interpret y


data Instr = VAL Int
           | ADD
           | SUB


compile :: Expr -> [Instr]
compile (Val v)   = [VAL v]
compile (x :+: y) = compile x ++ compile y ++ [ADD]
compile (x :-: y) = compile x ++ compile y ++ [SUB]


data ExprAlgebra e = ExprAlgebra
                   { val :: Int -> e
                   , add :: e -> e -> e
                   , sub :: e -> e -> e
                   }

interpreter :: ExprAlgebra Int
interpreter = ExprAlgebra
            { val = id
            , add = (+)
            , sub = (-)
            }

--foldExpr :: ExprAlgebra a -> Expr -> a
--foldExpr alg (Val v)   = (val alg) v
--foldExpr alg (x :+: y) = (add alg) (foldExpr x) (foldExpr y)
--foldExpr alg (x :-: y) = (sub alg) (foldExpr x) (foldExpr y)

