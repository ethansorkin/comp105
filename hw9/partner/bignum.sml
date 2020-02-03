exception LeftAsExercise of string

functor BignumFn(structure N : NATURAL) :> BIGNUM
  =
struct

    datatype bigint = POSITIVE of N.nat
                    | NEGATIVE of N.nat
                    | ZERO

    (* Abstraction function: 
     * 
     * CONVERTING FROM INT TO BIGINT:
     * BigNum.ofInt(x) = POSITIVE(natural.ofInt(x)), where x > 0
     * BigNum.ofInt(x) = NEGATIVE(natural.ofInt(x)), where x < 0
     * BigNum.ofInt(0) = ZERO
     *)

    fun invariant (POSITIVE(n)) = N.invariant(n)
      | invariant (NEGATIVE(n)) = N.invariant(n)
      | invariant ZERO          = true
      

    exception BadDivision
   
    fun ofInt (x) = if x = 0 then ZERO
                            else if x > 0 then POSITIVE(N.ofInt x)
                                 else NEGATIVE(N.ofInt (0 - x))
 
    fun negated (b) = case b
                        of POSITIVE(n) => NEGATIVE(n)
                         | NEGATIVE(n) => POSITIVE(n)
                         | ZERO        => ZERO

    fun <+> (x, y) = case (x, y)
                       of (POSITIVE(n), POSITIVE(m)) => POSITIVE(N./+/ (n, m))
                        | (POSITIVE(n), NEGATIVE(m)) => (POSITIVE(N./-/ (n, m))
                                          handle Negative =>
                                                       NEGATIVE(N./-/ (m, n)))
                        | (NEGATIVE(n), POSITIVE(m)) 
                                             => <+> (POSITIVE(m), NEGATIVE(n))
                        | (NEGATIVE(n), NEGATIVE(m)) => NEGATIVE(N./+/ (n, m))
                        | (b, ZERO) => b
                        | (ZERO, b) => b

    fun <-> (x, y) = case (x, y)
                of (POSITIVE(n), POSITIVE(m)) => (POSITIVE(N./-/ (n, m))
                                   handle Negative => NEGATIVE (N./-/ (m, n)))
                 | (POSITIVE(n), NEGATIVE(m)) => POSITIVE(N./+/ (n, m))
                 | (NEGATIVE(n), POSITIVE(m)) => NEGATIVE(N./+/ (n, m))
                 | (NEGATIVE(n), NEGATIVE(m)) => <+> (NEGATIVE(n), POSITIVE(m))
                 | (b, ZERO) => b
                 | (ZERO, POSITIVE(m)) => NEGATIVE(m)
                 | (ZERO, NEGATIVE(m)) => POSITIVE(m)

    fun <*> (x, y) = case (x, y)
                of (POSITIVE(n), POSITIVE(m)) => POSITIVE(N./*/ (n, m))
                 | (POSITIVE(n), NEGATIVE(m)) => NEGATIVE(N./*/ (n, m))
                 | (NEGATIVE(n), POSITIVE(m)) => NEGATIVE(N./*/ (n, m))
                 | (NEGATIVE(n), NEGATIVE(m)) => POSITIVE(N./*/ (n, m))
                 | (ZERO, _) => ZERO
                 | (_, ZERO) => ZERO

    fun sdiv (POSITIVE(n), d) = 
            let val {quotient = q, remainder = r} = N.sdiv(n, d)
            in {quotient = POSITIVE(q), remainder = r} end
      | sdiv (NEGATIVE(n), d) =
            let val {quotient = q, remainder = r} = N.sdiv(n, d)
            in {quotient = NEGATIVE(q), remainder = r} end
      | sdiv (ZERO, d) =
            let val {quotient = q, remainder = r} = N.sdiv(N.ofInt(0), d)
            in {quotient = ZERO, remainder = r} end
    
    fun compare (POSITIVE(x), POSITIVE(y)) = N.compare(x, y)
      | compare (NEGATIVE(x), NEGATIVE(y)) = N.compare(y, x)
      | compare (b, POSITIVE(x))           = LESS
      | compare (b, NEGATIVE(x))           = GREATER
      | compare (POSITIVE(x), ZERO)        = GREATER
      | compare (NEGATIVE(x), ZERO)        = LESS
      | compare (ZERO, ZERO)               = EQUAL
    
    fun toString (POSITIVE(x)) = 
          foldr (fn(x, y) => Int.toString(x) ^ y) "" (N.decimal(x))
      | toString(NEGATIVE(x)) =
          "-" ^ foldr (fn(x, y) => Int.toString(x) ^ y) "" (N.decimal(x))
      | toString (ZERO) = "0"

end
