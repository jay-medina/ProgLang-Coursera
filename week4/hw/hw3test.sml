(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["aPPLE","B","cAT"] = ["B"]
val test1b = only_capitals ["A","bAb","c"] = ["A"]
val test1c = only_capitals ["aPP", "b", "c"] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["Acd","bc","C"] = "Acd"
val test2b = longest_string1 ["A","bc","Cb"] = "bc"
val test2c = longest_string1 [] = ""

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["Acd","bc","C"] = "Acd"
val test3b = longest_string2 ["A","bc","Cb"] = "Cb"
val test3c = longest_string2 [] = ""

val test4 = longest_string3 ["A","bc","C"] = "bc"
val test4a = longest_string3 ["Acd","bc","C"] = "Acd"
val test4b = longest_string3 ["A","bc","Cb"] = "bc"
val test4c = longest_string3 [] = ""

val test4d = longest_string4 ["A","B","C"] = "C"
val test4e = longest_string4 ["Acd","bc","C"] = "Acd"
val test4f = longest_string4 ["A","bc","Cb"] = "Cb"
val test4g = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized ["abcd","bc","C"] = "C"
val test5b = longest_capitalized [] = ""

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7a = (first_answer (fn x => if x > 10 then SOME x else NONE) [1,2,3,4,5]) handle NoAnswer => ~1

val test8 = all_answers (fn x => if x > 1 andalso x < 7 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if true then SOME [x] else NONE) [] = SOME []
val test8b = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5] = SOME[2,3,4,5]

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
