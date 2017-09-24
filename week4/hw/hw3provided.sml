(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0)))

(* 2 *)
val longest_string1 =
  List.foldl (fn (next, acc) => if (String.size next > String.size acc) then next else acc) ""

(* 3 *)
val longest_string2 =
  List.foldl (fn (next, acc) => if (String.size next >= String.size acc) then next else acc) ""

(* 4 *)
fun longest_string_helper f = List.foldl (fn (next, acc) => if f(String.size next, String.size acc) then next else acc) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f lst = 
  case lst of
    [] => raise NoAnswer
  | x :: xs => case f(x) of
                 SOME y => y
               | NONE => first_answer f xs

(* 8 *)
fun all_answers f lst =
  let fun looper (lst, acc) =
      case lst of 
        [] => acc
      | x :: xs => case f(x) of 
                    SOME y => looper(xs, acc @ y)
                   | NONE => []
  in
    case (lst, looper(lst, [])) of
      ([], _) => SOME []
    | (_, []) => NONE
    | (_, y) => SOME y          
  end

(* 9a *)
val count_wildcards = g (fn x => 1) (fn x => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn x => 1) String.size

(* 9c *)
fun count_some_var (str, pat) = 
  g (fn x => 0) (fn x => if (str = x) then 1 else 0) pat

(* 10 *)
fun check_pat pat = 
  let fun getAllVariableStrs pat =
        case pat of
          Variable x => [x]
        | ConstructorP (_, p) => getAllVariableStrs p
        | TupleP (x::xs) => getAllVariableStrs(x) @ getAllVariableStrs(TupleP xs)
        | _ => []

      fun ifStrRepeats strList =
        case strList of 
          [] => false
        | x::xs => (List.exists (fn y => x = y) xs) orelse ifStrRepeats(xs)
  in
    (not o ifStrRepeats o getAllVariableStrs) pat
  end
