(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 1 *)
fun all_except_option (str, strList) =
  let
    fun isInList strList = 
      case strList of
        [] => false
      | x::xs => same_string(x, str) orelse (isInList xs)

    fun removeItem strList =
      case strList of 
        [] => []
      | x::xs => if same_string(x, str)
                 then removeItem xs
                 else x :: removeItem(xs)
  in
    if isInList(strList)
    then SOME (removeItem strList)
    else NONE
  end
