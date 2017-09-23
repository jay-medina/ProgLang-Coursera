fun fold (f, acc, xs) =
  case xs of
    [] => acc
  | x::xs' => fold(f, f(acc, x), xs')


fun sum xs = fold(fn (x, y) => x + y, 0, xs)

fun allPositive xs = fold(fn (x, y) => x andalso y >= 0, true, xs)
