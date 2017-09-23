fun double_or_triple f =
  if f 7
  then fn x => 2 * x
  else fn x => 3 * x


val double = double_or_triple (fn x => x mod 2 = 0)

val nine = double 3

datatype exp = Constant of int
  | Negate of exp
  | Add of exp * exp
  | Multiply of exp * exp

fun true_of_all_constants (f,e) =
  case e of 
    Constant x => f(x)
  | Negate exp => true_of_all_constants(f, exp)
  | Add (e1, e2) => true_of_all_constants(f, e1) andalso true_of_all_constants(f, e2)
  | Multiply (e1, e2) => true_of_all_constants(f, e1) andalso true_of_all_constants(f, e2)

val all_even = fn exp => true_of_all_constants((fn x => x mod 2 = 0), exp)

