structure S = SealedSolution
val (lam, var, app, subst) = (S.lam, S.var, S.app, S.subst)
val N : S.term = app (app (var "fst") (var "x")) (var "y")
val checkExpectTerm = Unit.checkExpectWith toString

val () = checkExpectTerm "subst, case (a)" (fn () => subst ("x", N) (var "x")) N


val () = checkExpectTerm "subst, case (b)" (fn () => subst ("x", N) (var "y")) 
                                             (var "y")

val () = checkExpectTerm "subst, case (c)" 
           (fn () => subst ("x", N) (app (var "x") (var "y"))) (app N (var "y"))


val () = checkExpectTerm "subst, case (d)" 
           (fn () => subst ("x", N) (lam "x" (var "y"))) (lam "x" (var "y"))


val () = checkExpectTerm "subst, case (e)" 
           (fn () => subst ("x", N) (lam "y" (var "z"))) (lam "y" (var "z"))


val () = checkExpectTerm "subst, renaming"
           (fn () => subst ("x", var "y") (lam "y" (var "x")))
           (lam "0" (var "y"))