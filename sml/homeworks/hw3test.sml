(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"];
val sam1 = ["Ncen","lIke","a","Doops","Park","heRE"];
val test1a = only_capitals sam1 = ["Ncen","Doops","Park"];
val test1b = only_capitals [] = [];
val test1c = only_capitals ["asama"] = [];

val test2 = longest_string1 ["A","bc","C"] = "bc"

val sam2a = ["birg","well","donec","fiska"];
val sam2b = [];
val sam2c = ["chandler","bigalowult"];
val sam3c = ["chandler","chandlep"];

val test2a = longest_string1 sam2a = "donec";
val test2b = longest_string1 sam2b = "";
val test2c = longest_string1 sam2c = "bigalowult";


val test3 = longest_string2 ["A","bc","C"] = "bc";
val test3a = longest_string2 sam2a = "fiska";
val test3b = longest_string2 sam2b = "";
val test3c = longest_string2 sam3c = "chandlep";


val test4a= longest_string3 ["A","bc","C"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"

val test4c = longest_string3 ["A","bc","C"] = "bc"
val test4d = longest_string3 sam2a = "donec";
val test4e = longest_string3 sam2b = "";
val test4f = longest_string3 sam2c = "bigalowult";

val test4h = longest_string4 ["A","bc","C"] = "bc";
val test4i = longest_string4 sam2a = "fiska";
val test4j = longest_string4 sam2b = "";
val test4l = longest_string4 sam3c = "chandlep";

val sam5a = ["william","Like","helllllllooo","Bas"];
val sam5b = [];
val sam5c = ["Fooking","Fuuking","hell","Yeah"];
val test5 = longest_capitalized ["A","bc","C"] = "A";
val test5a = longest_capitalized sam5a = "Like";
val test5b = longest_capitalized sam5b = "";
val test5c = longest_capitalized sam5c = "Fooking";


val test6 = rev_string "abc" = "cba";
val test6a = rev_string "dodol" = "lodod";
val test6b = rev_string "" = "";



val test7 = first_answer (fn x => if x > 3 then SOME x else NONE)
			 [1,2,3,4,5] = 4;

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE)
			[2,3,4,5,6,7] = NONE;

val test8a = all_answers (fn x => if x < 5 then SOME [x] else NONE)
                         [2,3,4,5,6,7] = SOME [2,3,4];

val test8b = all_answers (fn x => if x < 5 then SOME [x] else NONE)
                         [] = SOME [];

val test9a = count_wildcards Wildcard = 1;


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1;
(*
val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
*)
