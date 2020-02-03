(* Ethan Sorkin and Josh DeSieno *)

structure ExposedNatural = struct
    val base = 10
    type digit = int
    datatype nat = LITTLE_ENDIAN of digit list  
            (* where 0 <= digit < base and digit list is non-empty *)

    (* abstraction function:
     *  
     *  CONVERTING FROM INT TO NAT:
     *  ofInt(x) = LITTLE_ENDIAN(x mod base::ofInt(x/10)), where x >= base
     *  ofInt(x) = LITTLE_ENDIAN([x]), where 0 <= x < base.
     *  
     *  CONVERTING FROM NAT TO INT:
     *  ofNat(LITTLE_ENDIAN(x::y::xs)) = foldr (fn(d, m) => d + (base * m)) 0 
                                                                      (x::y::xs)
     *  ofNat(LITTLE_ENDIAN(x::nil)) = x
     *)


    fun invariant (LITTLE_ENDIAN(x::xs)) = 
        let fun checkList (y::ys) = if (y < 0 orelse y >= base) 
                                    then false else checkList ys
              | checkList nil     = true 
        in
            checkList(x::xs)
        end 
      | invariant _ = false

    exception Negative
    exception BadDivisor
    exception Impossible

    fun ofInt (x) = let fun diglist (n) = 
                            if n < 0 then raise Negative
                            else if n < base then [n]
                                 else (n mod base) :: (diglist(n div base))
                    in
                        LITTLE_ENDIAN (diglist (x))
                    end

    fun trimZeroes (xs) = 
        let fun trim (y::nil) = [y]
              | trim (y::ys)  = if y = 0 then trim ys
                                 else rev(y::ys)
              | trim _        = raise Impossible
        in trim (rev xs)
        end 
     
    fun addlists (nil, ys) = ys
              | addlists (xs, nil) = xs
              | addlists (x::xs, y::ys) = 
                    let val d = (x + y) mod base
                        val c = (x + y) div base
                    in
                        if (c = 0) then d :: addlists(xs, ys)
                        else d :: addlists(addlists(xs, [c]), ys)
                    end

    fun /+/ (LITTLE_ENDIAN(x), LITTLE_ENDIAN(y)) = LITTLE_ENDIAN(addlists(x, y))

    fun /-/ (LITTLE_ENDIAN(x), LITTLE_ENDIAN(y))  =
        let fun sublists (ys, nil) = ys
              | sublists (nil, ys) = raise Negative
              | sublists (x::xs, y::ys) = 
                    let val d = (x - y) mod base
                        val b = if (x - y) < 0 then 1 else 0
                    in
                        if (b = 0) then d :: sublists(xs, ys)
                        else d :: sublists(sublists(xs, [b]), ys) 
                    end
        in
            LITTLE_ENDIAN(trimZeroes(sublists(x, y)))
        end 

    fun /*/ (LITTLE_ENDIAN(x), LITTLE_ENDIAN(y)) = 
        let fun multdig (x::xs, n) = 
                let val d = (n * x) mod base
                    val c = (n * x) div base
                in 
                    d :: (addlists (multdig (xs, n), [c]))
                end
              | multdig (nil, d) = nil

            fun multlists (xs, y::ys) = addlists (multdig((xs), y), 
                                                         0::multlists((xs), ys))
              | multlists (_, nil) = [0]
        in
            LITTLE_ENDIAN(trimZeroes(multlists(x, y)))
        end

    fun sdiv (LITTLE_ENDIAN(n), d) = 
        let val rn = rev n
            fun divdig(x::xs, d) = 
                let val q = x div d
                    val c = x mod d
                in
                    case xs of y::ys => q :: divdig(((base * c) + y)::ys, d)
                             | nil => q :: [c]
                end
              | divdig(_, d) = raise Impossible
        in
            if d <= 0 orelse d > base then raise BadDivisor
            else case (rev (divdig(rn, d))) 
                 of (r::quot) => {quotient = (LITTLE_ENDIAN(trimZeroes quot)), 
                                                                  remainder = r}
                  | (_) => raise Impossible
        end

    fun compare (LITTLE_ENDIAN(x), LITTLE_ENDIAN(y)) = 
        let fun compdig (x::xs, y::ys, order) =
                if x > y then compdig(xs, ys, GREATER) 
                else if x < y then compdig(xs, ys, LESS) 
                     else compdig(xs, ys, order)
              | compdig(nil, y::ys, order) = LESS
              | compdig(x::xs, nil, order) = GREATER
              | compdig(nil, nil, order)   = order
        in
            compdig(x, y, EQUAL)
        end

    fun decimal (LITTLE_ENDIAN(x)) = rev x 

end

structure Natural :> NATURAL = ExposedNatural
