data Expr = LitE Int
          | AddE Expr Expr
          | SubE Expr Expr
          | MulE Expr Expr

eval :: Expr -> Int
eval (LitE v) = v
eval (AddE l r) = eval l + eval r
eval (SubE l r) = eval l - eval r
eval (MulE l r) = eval l * eval r
