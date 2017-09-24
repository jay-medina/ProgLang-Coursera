fun sum_triple1 triple = 
 case triple of
  (x,y,z) => x + y + z

fun sum_triple2 triple = 
  let val (x,y,z) = triple 
  in 
   x + y + z
  end

fun sum_triple3 (x, y, z) = x + y + z

fun full_name {first=x, middle=y, last=z} =
  x ^ " " ^ y ^ " " ^ z
