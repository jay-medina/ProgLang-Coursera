exception ListLengthMismatch

fun zip3 (arr1, arr2, arr3) = 
  case (arr1, arr2, arr3) of
    ([],[],[]) => []
  | (x::xs, y::ys, z::zs) => (x,y,z) :: zip3 (xs, ys, zs)
  | _ => raise ListLengthMismatch

fun unzip3 lst =
  case lst of
   [] => ([], [], [])
  | (x,y,z) :: rst => let val (l1, l2, l3) = unzip3 rst
                      in 
                        (x::l1, y::l2, z::l3)
                      end


fun nondecreasing xs =
  case xs of
   [] => true
  | _ :: [] => true
  | x :: y :: xs => x <= y andalso nondecreasing (y::xs)

datatype sgn = P | N | Z

fun multsign (x1, x2) =
  let
    fun findSign (n) = if n < 0 then N else if n > 0 then P else Z
    val s1 = findSign x1
    val s2 = findSign x2
  in
    case (s1, s2) of
     (P, P) => P
   | (N, N) => P
   | (Z, _) => Z
   | (_, Z) => Z
   | _ => N
  end

fun len xs =
  case xs of 
  [] => 0
  | _::xs' => 1 + len xs'
