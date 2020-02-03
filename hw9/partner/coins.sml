(* Josh Desieno, Ethan Sorkin
 * Comp 105 : Modules and Abstract Types
 * Exercise C 
 *)

exception LeftAsExercise of string 

structure Coins :> GAME = struct

    datatype state = PQDN of Player.player * int * int * int

    (* State abstraction function: 
     * Given that q, d, and n represent the number of remaining quarters, dimes,
     * and nickels respectively, along with which player p's turn it is,
     * our corresponding state is PQDN(p, x, y, z).
     *
     * Given that there are no coins left on the table, and it is player p's 
     * turn, player p has lost because the other player picked the last coin
     * which made the state FINAL(P).
     *)

    fun invariant (PQDN(p, q, d, n)) =  q <= 4  andalso q >= 0 andalso 
                                        d <= 10 andalso d >= 0 andalso
                                        n <= 7  andalso n >= 0 
 

    fun initial (Player.O) = PQDN(Player.O, 4, 10, 7)
      | initial (Player.X) = PQDN(Player.X, 4, 10, 7)

    fun whoseturn (PQDN(p, q, d, n)) = p

        structure Move = struct
        exception Move
                type move = (string * int)

        (* State abstraction function:
         * Our representation of a move is a pair containing a string and int.
         * The string must be "quarter", "dime", or "nickel", as it represents 
         * the denomination; and the integer must be in the appropriate range
         * of quantities for each denomination.
         *)
        fun invariant ("quarter", n) = n <= 4 andalso n > 0 
          | invariant ("dime", n)    = n <= 10 andalso n > 0
          | invariant ("nickel", n)  = n <= 7 andalso n > 0
          | invariant _              = false

        fun visualize p (denom, n) = "Player " ^ Player.unparse(p) ^ " takes " 
                                                ^ Int.toString(n) ^ " " ^ denom

        fun prompt p = "What coins does player " ^ Player.unparse(p) ^ " take?"

        (*val findName : string -> string option *)
        fun findName s =
          List.find (fn name => String.isSubstring name s) 
                    ["quarter", "dime", "nickel"]

        (*val findNumber : string -> int option *)
        val findNumber =
          Int.fromString o implode o List.filter Char.isDigit o explode

        fun parse s = case (findName s, findNumber s) of 
                        (SOME d, SOME x) => if invariant (d, x) 
                                            then (d, x) else raise Move
                      | _ => raise Move
    end


    fun legalmoves (PQDN(p, q, d, n)) = 
        let fun coinmoves (denom, x) = 
                if x <= 0 then [] 
                else (denom, x)::coinmoves(denom, x - 1)
        in
            coinmoves("quarter", q) @ 
            coinmoves("dime", d)    @ 
            coinmoves("nickel", n)
        end

    fun member x = List.exists (fn y => y = x)

    fun makemove (PQDN(p, 0, 0, 0)) _ = raise Move.Move
      | makemove (PQDN(p, q, d, n)) ("quarter", x) = 
            if member ("quarter", x) (legalmoves(PQDN(p, q, d, n)))
            then PQDN(Player.otherplayer(p), q - x, d, n)
            else raise Move.Move

      | makemove (PQDN(p, q, d, n)) ("dime", x) = 
            if member ("dime", x) (legalmoves(PQDN(p, q, d, n)))
            then PQDN(Player.otherplayer(p), q, d - x, n)
            else raise Move.Move

      | makemove (PQDN(p, q, d, n)) ("nickel", x) = 
            if member ("nickel", x) (legalmoves(PQDN(p, q, d, n)))
            then PQDN(Player.otherplayer(p), q, d, n - x)
            else raise Move.Move
      | makemove _ _ = raise Move.Move

    fun outcome (PQDN(p, 0, 0, 0)) = SOME (Player.WINS(Player.otherplayer(p)))
      | outcome (PQDN(p, q, d, n)) = NONE

    fun isOver (PQDN(p, 0, 0, 0)) = true 
      | isOver (PQDN(p, q, d, n)) = false

    fun visualize (PQDN(p, 0, 0, 0)) = "Player " ^ Player.unparse(p) ^ 
                                                      "faces an empty table \n"
      | visualize (PQDN(p, q, d, n)) = 
            "Player " ^ Player.unparse(p) ^ " sees " ^ Int.toString(q) ^ 
                        " quarters, " ^ Int.toString(d) ^ " dimes, and " 
                                              ^ Int.toString(n) ^ " nickels \n"
end
