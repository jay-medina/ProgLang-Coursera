fun map (xs, f) = 
  case xs of 
    [] => []
  | y::ys => f(y) :: map(ys, f)

val test = map([1,2,3,4,5], fn x => x + 1)


