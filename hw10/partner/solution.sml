(* Lambda calculus solution template, COMP 105 *)
structure OpenSolution = struct

  (******************* EVALUATION BASICS ********************)

  datatype void = VOID of void  (* a type with no values *)

  datatype term = VAR of string
                | LAM of (string * term)
                | APP of (term * term)

  exception LeftAsExercise of string

  val var : string -> term         = fn x => VAR x
  val app : term   -> term -> term = fn x => fn y => APP(x, y)
  val lam : string -> term -> term = fn s => fn x => LAM(s, x)

  val cpsLambda :
      term -> (string -> term -> 'a) -> (term -> term -> 'a) -> (string -> 'a) 
      -> 'a
    = fn term => fn f => fn g => fn h => case term of LAM(x, e)  => f x e
                                                    | APP(e, e') => g e e'
                                                    | VAR x      => h x

  (********************** SUBSTITUTION **********************)

  fun member z = List.exists (fn w => w = z)

  fun freeVars (LAM(x, e))  = List.filter (fn (y) => not (y = x)) (freeVars e) 
    | freeVars (APP(e, e')) = (freeVars e) @ List.filter (fn (x) => 
                                not (member x (freeVars e))) (freeVars e')
    | freeVars (VAR x)      = x :: []

  fun freshVar (vs, n) = if (member (Int.toString n) vs) then freshVar (vs, n+1)
                         else Int.toString n

  fun subst (x, n) = 
    fn(term) => 
      case term 
        of VAR(y)      => if y = x then n else var y
         | APP(m1, m2) => app (subst(x, n) m1) (subst (x, n) m2)
         | LAM(y, m)   => if y = x then lam y m
                          else 
                            let val freeM = freeVars m
                                val freeN = freeVars n
                            in
                              if (member x freeM) andalso (member y freeN)
                              then subst (x, n)   
                                         (lam (freshVar (freeN @ freeM, 0)) m)
                              else lam y (subst(x, n) m)
                            end

          
  (****************** REDUCTION STRATEGIES ******************)
  val ONE_STEPS_TO = Reduction.ONE_STEPS_TO
  val DOESN'T_STEP = Reduction.DOESN'T_STEP
  val rmap = Reduction.rmap
  val >=> = Reduction.>=>
  infix 1 >=>
  fun flip f = fn x => fn y => f y x
  fun curry f x y = f (x, y)

  fun beta term = 
    case term of APP(LAM(x, m), n) => ONE_STEPS_TO (subst (x, n) m)
               | _                 => DOESN'T_STEP

  fun nu_maker reducer term =
    case term of APP(e1, e2) => rmap (flip app e2) (reducer e1)
               | _           => DOESN'T_STEP

  fun mu_maker reducer term =
    case term of APP(e1, e2) => rmap (app e1) (reducer e2)
               | _           => DOESN'T_STEP

  fun xi_maker reducer term = 
    case term of LAM(x, m) => rmap (lam x) (reducer m)
               | _         => DOESN'T_STEP


  fun eta term =
    case term of LAM(x, APP(m, VAR(x'))) => if (x = x') andalso 
                                                  (not (member x' (freeVars m)))
                                            then ONE_STEPS_TO m
                                            else DOESN'T_STEP
               | _                       => DOESN'T_STEP

  fun reduceN m =
    let
      val this = reduceN
      val nu = nu_maker this
      val mu = mu_maker this
      val xi = xi_maker this
      val reduce = beta >=> nu >=> mu >=> eta >=> xi
    in
      reduce m
    end

  fun reduceA m =
    let
      val this = reduceA
      val nu = nu_maker this
      val mu = mu_maker this
      val xi = xi_maker this
      val reduce = mu >=> nu >=> beta >=> eta >=> xi
    in
      reduce m
    end

end


structure SealedSolution :> SOLUTION = OpenSolution
