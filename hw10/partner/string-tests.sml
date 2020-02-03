structure S = SealedSolution
val (lam, var, app) = (S.lam, S.var, S.app)

val () = Unit.checkExpectWith (fn s => s) "test 1"
    (fn () => toString (app (lam "y" (app (lam "x" (var "x")) (var "z"))) 
                       (app (var "y") (var "y")))) 
              "((lambda (y) ((lambda (x) x) z)) (y y))"

val () = Unit.checkExpectWith (fn s => s) "too many parentheses"
    (fn () => toString ((((lam "x" (app (var "even?") ((var ("x"))) ))))))
            "(lambda (x) (even? x))"

val () = Unit.checkExpectWith (fn s => s) "t"
    (fn () => toString (lam "x" (app (var "x") (app (var "y") (var "z")))))
            "(lambda (x) (x (y z)))"