(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

open List;
(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (s:string, ls: string list) =
    let val res = length (filter (fn n => same_string (s,n)) ls)
	fun helper [] acc = acc
	  | helper (x::xs) acc =
	    if same_string (x,s)
	    then helper xs acc
	    else helper xs (x::acc)
    in
	if res = 0
	then NONE
	else SOME (rev (helper ls []))
    end;

