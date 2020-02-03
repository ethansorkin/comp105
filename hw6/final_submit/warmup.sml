(******************** CORE ML ASSIGNIMENT ********************)


(***** Problem A *****)
(* val mynull : 'a list -> bool *)
fun mynull [] = true
  | mynull (x::xs) = false

        val () =
            Unit.checkAssert "empty list"
            (fn () => mynull [])

        val () =
            Unit.checkAssert "nonempty list"
            (fn () => not (mynull [1, 2, 3]))

(**************************************************************************)

(***** Problem B *****)
(* firstVowel : char list -> bool *)
fun firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _ = false


        val () =
            Unit.checkAssert "empty list"
            (fn () => not (firstVowel []))

        val () =
            Unit.checkAssert "nonempty list true"
            (fn () => firstVowel [#"a", #"b", #"c"])

        val () =
            Unit.checkAssert "nonempty list false"
            (fn () => not (firstVowel [#"b", #"c", #"a"]))

(**************************************************************************)

(***** Problem C *****)
(* 1 *)
(* val reverse : 'a list -> 'a list *)
fun reverse xs = foldl op :: [] xs

        val () =
            Unit.checkExpectWith (Unit.listString Int.toString) "empty list"
            (fn () => reverse [])
            []

        val () =
            Unit.checkExpectWith (Unit.listString Int.toString) "nonempty list"
            (fn () => reverse [1, 2, 3])
            [3, 2, 1]

(* 2 *)
(* val minlist : int list -> int *)
fun minlist (x::xs) = foldl Int.min x xs
  | minlist [] = raise Match

    val () =
        Unit.checkExpectWith Int.toString "nonempty list"
        (fn () => minlist [3, ~9, 0, 2, 100])
        ~9

    val () =
        Unit.checkExnWith Int.toString "empty list --> error"
        (fn () => minlist [])

(**************************************************************************)

(***** Problem D *****)
exception Mismatch   (* defining an exception *)

(* zip: 'a list * 'b list -> ('a * 'b) list *)
fun zip ([], []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip _ = raise Mismatch

        val () =
            Unit.checkExpectWith (Unit.listString Unit.showNothing) "empty list"
            (fn () => zip ([], []))
            []

        val () =
            Unit.checkExpectWith 
                  (Unit.listString (Unit.pairString Int.toString Bool.toString))
            "nonempty lists" (fn () => zip ([1, 2, 3], [true, false, false]))
            [(1, true), (2, false), (3, false)]

        val () =
            Unit.checkExnWith (Unit.listString Unit.showNothing) "mismatched"
            (fn () => zip ([1, 2, 3], ["one", "two"]))

(**************************************************************************)

(***** Problem E *****)
(* val pairfoldrEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c *)
fun pairfoldrEq f x0 ([], []) = x0
  | pairfoldrEq f x0 (x::xs, y::ys) = f (x, y, (pairfoldrEq f x0 (xs, ys)))
  | pairfoldrEq f x0 _ = raise Mismatch

        (* function for testing, same behavior as 'bind' in uscheme *)
        fun sum (x, y, z) = x + y + z 

        val () =
            Unit.checkExpectWith Int.toString "empty lists" 
            (fn () => pairfoldrEq sum 0 ([], []))
            0

        val () =
            Unit.checkExpectWith Int.toString "nonempty lists" 
            (fn () => pairfoldrEq sum 0 ([1, 2, 3], [4, 5, 6]))
            21

        val () =
            Unit.checkExnWith Int.toString "mismatched lists" 
            (fn () => pairfoldrEq sum 0 ([1, 2, 3], [4, 5]))

        

(* val ziptoo : 'a list * 'b list -> ('a * 'b) list *)
fun ziptoo ([], []) = []
  | ziptoo (x::xs, y::ys) = pairfoldrEq (fn (h1, h2, list) => 
                                            (h1, h2) :: list) [] (x::xs, y::ys)
  | ziptoo _ = raise Mismatch

        val () =
            Unit.checkExpectWith (Unit.listString Unit.showNothing) "empty list"
            (fn () => ziptoo ([], []))
            []

        val () =
            Unit.checkExpectWith 
                  (Unit.listString (Unit.pairString Int.toString Bool.toString))
            "nonempty lists" (fn () => ziptoo ([1, 2, 3], [true, false, false]))
            [(1, true), (2, false), (3, false)]

        val () =
            Unit.checkExnWith (Unit.listString Unit.showNothing) "mismatched"
            (fn () => ziptoo ([1, 2, 3], ["one", "two"]))

(**************************************************************************)

(***** Problem F *****)
(* val concat : 'a list list -> 'a list *)
fun concat xs = foldr (op @) [] xs

        val () =
            Unit.checkExpectWith (Unit.listString Int.toString) "list of lists"
            (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
            [1, 2, 3, 4, 5, 6]

(**************************************************************************)

(***** Problem G *****)


type 'a env = string -> 'a
exception NotFound of string

(* val emptyEnv : 'a env *)
fun emptyEnv (x) = raise NotFound x

        val () =
            Unit.checkExnWith (Unit.listString Int.toString) "in empty env?"
            (fn () => emptyEnv "x")

(* val bindVar : string * 'a * 'a env -> 'a env *)
fun bindVar (s, value, env) = (fn (x) => if (s = x) then value else env x)


(* val lookup  : string * 'a env -> 'a *)

fun lookup (s, env) = env s

        (* defining 3 letter words env  string -> bool *)
        val tlw = (fn (x) => if size(x) = 3 then true else raise NotFound x) 

        (* defining 3 length words env, string -> int *) 
        val len = (fn (x) => size(x))
        val () =
            Unit.checkExpectWith (fn (x) => x) "extend env and lookup"
            (fn () => lookup ("comp105", bindVar ("comp105", "NR", emptyEnv)))
            "NR"

        val () =
            Unit.checkExnWith Bool.toString "lookup not found"
            (fn () => lookup ("comp105", tlw))



(* val isBound : string * 'a env -> bool *)
fun isBound (s, env) = (lookup (s, env) = lookup (s, env)) 
                                                     handle NotFound s => false

        val () =
            Unit.checkAssert "bound in env"
            (fn () => isBound ("cat", len))

        val () =
            Unit.checkAssert "not bound in env"
            (fn () => not (isBound ("cat", emptyEnv)))


(**************************************************************************)


datatype nat = ZERO
             | TIMES10PLUS of nat * int

fun times10plus (ZERO, 0) = ZERO
  | times10plus (m, d)    = TIMES10PLUS (m, d)


(* times10 : nat -> nat *)
fun times10 n = times10plus (n, 0)

(* natOfDigit : int -> nat *)
fun natOfDigit d = times10plus (ZERO, d)

fun flip f (x, y) = f (y, x)

(* natOfDigits : int list -> nat *)
fun natOfDigits ds = foldl (flip times10plus) ZERO ds

(***** Problem H *****)

(* val intOfNat : nat -> int *)
fun intOfNat ZERO = 0
  | intOfNat (TIMES10PLUS (nat, int)) = 10 * intOfNat (nat) + int


        val () =
            Unit.checkExpectWith Int.toString "converting nat to int" 
            (fn () => intOfNat (natOfDigits [1, 2, 3]))
            123



(* val natOfInt : int -> nat *)
fun natOfInt 0 = ZERO
  | natOfInt n = times10plus (natOfInt (n div 10), n mod 10)



        val () =
            Unit.checkExpectWith Int.toString "converting int to nat"
            (fn () => intOfNat (natOfInt 2018))
            2018

(* val natString : nat -> string *)
fun natString ZERO = "0"
  | natString (TIMES10PLUS (ZERO, int)) = "" ^ Int.toString int
  | natString (TIMES10PLUS (nat, int)) = natString nat ^ Int.toString int

        val () =
            Unit.checkExpectWith (fn (x) => x) "converting ZERO to string"
            (fn () => natString (natOfInt 0))
            "0"

        val () =
            Unit.checkExpectWith (fn (x) => x) "converting nat to string"
            (fn () => natString (natOfInt 2018))
            "2018"


(**************************************************************************)

(***** Problem I *****)

(* carryIntoNat : nat * int -> nat *)
fun carryIntoNat (n, 0) = n
  | carryIntoNat (ZERO, c) = times10plus (ZERO, c)
  | carryIntoNat (TIMES10PLUS (m, d), c) = times10plus (carryIntoNat 
                                         (m, (d + 1) div 10), (d + 1) mod 10)
  
        val () =
            Unit.checkExpectWith Int.toString "carry dig is 0"
            (fn () => intOfNat (carryIntoNat (natOfInt 55, 0)))
            55

        val () =
            Unit.checkExpectWith Int.toString "natural is ZERO"
            (fn () => intOfNat (carryIntoNat (ZERO, 1)))
            1

        val () =
            Unit.checkExpectWith Int.toString "third case"
            (fn () => intOfNat (carryIntoNat (natOfInt 99, 1)))
            100


(* addWithCarry : nat * nat * int -> nat *)
fun addWithCarry (n1, ZERO, c) = carryIntoNat (n1, c)
  | addWithCarry (ZERO, n2, c) = carryIntoNat (n2, c)
  | addWithCarry (TIMES10PLUS (m1, d1), TIMES10PLUS (m2, d2), c) = 
        let val d = (d1 + d2 + c) mod 10
            val c' = (d1 + d2 + c) div 10
        in times10plus (addWithCarry (m1, m2, c'), d)
        end

        val () =
            Unit.checkExpectWith Int.toString "addWithCary"
            (fn () => intOfNat (addWithCarry (natOfInt 55, natOfInt 44, 1)))
            100

(* addNats : nat * nat -> nat *)
fun addNats (n1, n2) = addWithCarry (n1, n2, 0)

(* borrowFromNat : nat * int -> nat *)
exception Negative

fun borrowFromNat (n, 0) = n
  | borrowFromNat (ZERO, b) = raise Negative
  | borrowFromNat (TIMES10PLUS (m, 0), b) = times10plus (borrowFromNat 
                                                                     (m, 1), 9)
  | borrowFromNat (TIMES10PLUS (m, d), b) = times10plus (m, d - 1)

        val () =
            Unit.checkExpectWith Int.toString "borrow is 0"
            (fn () => intOfNat (borrowFromNat (natOfInt 55, 0)))
            55

        val () =
            Unit.checkExnWith Int.toString "natural is ZERO"
            (fn () => intOfNat (borrowFromNat (ZERO, 1)))

        val () =
            Unit.checkExpectWith Int.toString "90 - 1"
            (fn () => intOfNat (borrowFromNat (natOfInt 90, 1)))
            89

        val () =
            Unit.checkExpectWith Int.toString "99 - 1"
            (fn () => intOfNat (borrowFromNat (natOfInt 99, 1)))
            98

(* subWithBorrow : nat * nat * int -> nat *)
fun subWithBorrow (n1, ZERO, b) = borrowFromNat (n1, b)
  | subWithBorrow (ZERO, n2, b) = raise Negative
  | subWithBorrow (TIMES10PLUS(m1, d1), TIMES10PLUS(m2, d2), b) =
        let val d  = (d1 - d2 - b) mod 10
            val b' = if d1 - d2 - b < 0 then 1 else 0 (* the "borrow out" *)
        in  times10plus (subWithBorrow (m1, m2, b'), d)
        end
     

        val () =
            Unit.checkExpectWith Int.toString "natural is ZERO"
            (fn () => intOfNat (subWithBorrow (natOfInt 55, natOfInt 45, 1)))
            9


(* subNats : nat * nat -> nat *)
fun subNats (n1, n2) = subWithBorrow (n1, n2, 0)

val () =
  Unit.checkExnSatisfiesWith natString "1 - 5"
  (fn () => subNats (natOfDigits [1], natOfDigits [5]))
  ("Negative", fn Negative => true | _ => false)

(* val opsAgree : string -> (int * int -> int) -> (nat * nat -> nat) -> 
                                                          int -> int -> unit *)
fun opsAgree name intop natop n1 n2 =
  Unit.checkExpectWith Int.toString name
  (fn () => intOfNat (natop (natOfInt n1, natOfInt n2)))
  (intop (n1, n2))

val () = opsAgree "123 + 2018" (op +)  addNats 123 2018
val () = opsAgree "2018 - 123" (op -)  subNats 2018 123
val () = opsAgree "100 - 1   " (op -)  subNats 100 1

(**************************************************************************)

(***** Problem J *****)

(* an 'a flist is represented by a triple containing the following respectively:
            1. A list of elements moving left of the finger
            2. The element pointed to by the finger
            3. A list of elements moving right of the finger 

            It roughly looks like this: (<-------[finger]------->)
*)
datatype 'a flist = FLIST of 'a list * 'a * 'a list

(* smart constructor *)
fun flist(left, finger, right) = FLIST (left, finger, right)

(* val singletonOf : 'a -> 'a flist *)
fun singletonOf a = flist([], a, [])

(* val atFinger : 'a flist -> 'a *)
fun atFinger (FLIST(left, finger, right)) = finger

(* val fingerLeft  : 'a flist -> 'a flist *)
fun fingerLeft (FLIST([], finger, right)) = raise Subscript
  | fingerLeft (FLIST(l::ls, finger, right)) = flist(ls, l, finger::right)

(* val fingerRight  : 'a flist -> 'a flist *)
fun fingerRight (FLIST(left, finger, [])) = raise Subscript
  | fingerRight (FLIST(left, finger, r::rs)) = flist(finger::left, r, rs)

(* val deleteLeft  : 'a flist -> 'a flist *)
fun deleteLeft (FLIST([], finger, right)) = raise Subscript
  | deleteLeft (FLIST(l::ls, finger, right)) = flist(ls, finger, right)

(* val deleteRight : 'a flist -> 'a flist *)
fun deleteRight (FLIST(left, finger, [])) = raise Subscript
  | deleteRight (FLIST(left, finger, r::rs)) = flist(left, finger, rs)

(* val insertLeft  : 'a * 'a flist -> 'a flist *)
fun insertLeft (a, (FLIST(left, finger, right))) = flist(a::left, finger, right)

(* val insertRight : 'a * 'a flist -> 'a flist *)
fun insertRight (a, (FLIST(left, fin, right))) = flist(left, fin, a::right)

(* val ffoldl : ('a * 'b -> 'b) -> 'b -> 'a flist -> 'b *)
fun ffoldl f zero (FLIST(left, finger, right)) = foldl f zero ((List.rev(left)) 
                                                                        @ right)

(* val ffoldr : ('a * 'b -> 'b) -> 'b -> 'a flist -> 'b *)
fun ffoldr f zero (FLIST(left, finger, right)) = foldr f zero ((List.rev(left)) 
                                                                        @ right)

        val test = singletonOf 3
        val test = insertLeft  (1, test)
        val test = insertLeft  (2, test)
        val test = insertRight (4, test)
        val test = fingerRight test
        val test = insertRight (5, test)

        val () =
            Unit.checkAssert "testing 1-5"
            (fn () => (test = FLIST([3, 2, 1], 4, [5])))

        val () =
            Unit.checkAssert "delete insert"
            (fn () => (test = deleteLeft(insertLeft (0, test))))

        val () =
            Unit.checkAssert "finger insert"
            (fn () => (fingerLeft(insertLeft (0, test))) = 
                                   fingerRight(insertRight(0, fingerLeft test)))



        val () = Unit.reportWhenFailures ()
        