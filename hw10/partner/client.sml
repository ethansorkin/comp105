
fun toString term = 
    let val f = fn x => fn e => "(lambda (" ^ x ^ ") " ^ toString e ^ ")"
        val g = fn e => fn e' => "(" ^ toString e ^ " " ^ toString e' ^ ")"
        val h = fn x => x
    in SealedSolution.cpsLambda term f g h
    end