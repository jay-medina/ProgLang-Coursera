datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
         | Name of string * (string option) * string


val id = StudentNum(123)
val id2 = Name("jose", SOME("manuel"), "Medina")
val id3 = Name("samuel", NONE, "Medina")

datatype exp = Constant of int
    | Negate of exp
    | Add of exp * exp
    | Multiply of exp * exp

val temp = Add (Constant(19), Negate(Constant 4))

fun eval item = 
  case item of 
      Constant c => c
    | Negate e1 => ~1 * eval(e1)
    | Add(e1, e2) => eval(e1) + eval(e2)
    | Multiply(e1, e2) => eval(e1) * eval(e2)

fun numberOfAdds e =
  case e of
    Constant c => 0
  | Negate e1 => numberOfAdds(e1)
  | Add(e1, e2) => 1 + numberOfAdds(e1) + numberOfAdds(e2)
  | Multiply(e1, e2) => numberOfAdds(e1) + numberOfAdds(e2)

fun max_constant e =
    case e of
      Constant c => c
    | Negate e1 => max_constant e1
    | Add (e1, e2) =>  Int.max(
                          max_constant e1, 
                          max_constant e2
                        )
    | Multiply(e1,e2) =>  Int.max(
                            max_constant e1, 
                            max_constant e2
                          )
  
