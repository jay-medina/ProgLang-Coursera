fun compose (f, g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt ( Real.fromInt (abs i))

val sqrt_of_abs2 = Math.sqrt o Real.fromInt o abs

infix !>

fun x !> f = f x

fun sqrt_of_abs3 i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f, g) = fn x => case f x of NONE => g x 
                                      | SOME y => y
