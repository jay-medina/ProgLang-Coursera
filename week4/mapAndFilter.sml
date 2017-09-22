fun map (f,xs) = 
  case xs of 
    [] => []
  | y::ys => f(y) :: map(f, ys)

val test = map([1,2,3,4,5], fn x => x + 1)

fun filter (f,xs) =
  case xs of 
    [] => []
  | y::ys => if (f y) then y :: filter(f, ys) else filter(f, ys)

val test2 = filter([1,2,3,4,5], fn x => x mod 2 = 0)
