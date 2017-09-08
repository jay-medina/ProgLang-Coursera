val t1 = [1,2,3];

val emptyList = []

val c = 1::emptyList;

val isEmpty = null(emptyList);

val h1 = hd c;

val tee1 = tl c;

fun sum_list (xs: int list) =
  if null xs
  then 0
  else hd xs + sum_list (tl xs)

fun list_product xs =
  if null xs
  then 1
  else hd xs * list_product (tl xs)

fun countdown x =
  if x <= 1
  then [1]
  else x :: countdown(x - 1)

fun append (xs, ys) = 
  if null xs
  then ys
  else (hd xs) :: append (tl xs, ys)

fun sum (x, y) = x + y

fun sum_pair_list (xs: (int * int) list) =
 if null xs
 then 0
 else sum(hd xs) + sum_pair_list( tl xs )

fun firsts (xs: (int * int) list) =
 if null xs
 then []
 else #1 (hd xs) :: firsts(tl xs)

fun seconds (xs: (int * int) list) =
 if null xs
 then []
 else #2 (hd xs) :: seconds(tl xs)

 fun sum_pair_list2 (xs: (int * int) list) =
  sum_list (firsts xs) + sum_list (seconds xs)

fun factorial n = list_product (countdown n )
