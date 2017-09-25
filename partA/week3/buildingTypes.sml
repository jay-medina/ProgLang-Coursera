(* records *)
val x = { bar=(1+2, true), foo=7, baz=(false,9) }

val my_niece = {name="Amelia", id=41123-12}

val name = #name my_niece;

val brain_part = {id=true, ego=false, superego=false};

val a_pair = (3,4);

val a_record = {second= 6, first=4};

val another_pair = {2=5, 1=6};

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x = 
  case x of 
    Pizza => 3
  | Str s => 8
  | TwoInts(i1,i2) => i1 + i2
