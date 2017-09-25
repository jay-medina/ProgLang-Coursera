fun pow (x, y) =
  if y = 0
  then 1
  else x * pow (x, y-1)

fun cube x = pow(x, 3);

val sixtyfour = cube 4;

