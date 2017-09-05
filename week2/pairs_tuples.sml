val p1 = (4, 5)

fun swap ( pr: int*bool) =
  (#2 pr, #1 pr)

fun swap2 (x, y) = (y,x)

fun sum_two_pairs (pr1: int* int, pr2: int * int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod (x, y) =
  (x div y, x mod y)

fun sort_pair ( x, y ) = 
  if x < y then (x, y) else (y, x)

