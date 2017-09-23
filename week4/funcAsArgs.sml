fun increment_n_times_lame (n, x) =
  if n = 0
  then x
  else 1 + increment_n_times_lame(n-1, x)

fun double_n_times_lame (n, x) =
  if n = 0
  then x
  else 2 * double_n_times_lame(n-1, x)

fun nth_tail_lame (n, xs) =
  if n = 0
  then xs
  else tl (nth_tail_lame(n-1, xs))


fun n_times(f, n ,x ) =
  if n = 0
  then x
  else f(n_times(f, n - 1, x))

fun increment_n_times (n , x) = 
  n_times(fn y => y + 1, n, x)

fun double_n_times (n, x) = n_times(fn y => 2 * y, n , x)

fun nth_tail (n, xs) = n_times(tl, n, x)
