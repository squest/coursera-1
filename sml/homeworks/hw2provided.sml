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


fun get_substitutions1 (ls : string list list, s:string) = 
    let 
	val res = mapPartial (fn l => all_except_option (s,l)) ls
    in concat res
    end;

fun get_substitutions2 (ls : string list list, s:string) = 
    let fun helper ls1 acc =
	    case ls1 of
		[] => acc
	      | x::xs => case (all_except_option (s, x)) of
			     NONE => helper xs acc
			   | SOME a => helper xs (a::acc)
    in concat (rev (helper ls []))
    end;

fun similar_names (ls : string list list,
		   fullname : {first:string, middle:string,
			       last:string}) =
    let val subs = get_substitutions2 (ls, #first fullname)
	fun helper lsub acc =
	    case lsub of
		[] => acc
	      | x::xs => helper xs ({first=x, middle= #middle fullname
				     , last = #last fullname} :: acc)
    in
	fullname :: (rev (helper subs []))
    end;


val sample1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];

val sample2 = {first="Fred", middle="W", last="Smith"};

fun card_color ((cs:suit, cr:rank)) =
    case cs of
	Hearts => Red
      | Diamonds => Red
      | Spades => Black
      | Clubs => Black;

fun card_value ((cs:suit, cr:rank)) =
    case cr of
	Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num a => a;

fun remove_card (cs:card list, c:card, e) =
    let fun helper lcs acc =
	    case lcs of
		[] => raise e
	      | l::ls => if l = c
			 then acc @ ls
			 else helper ls (acc @ [l])
    in helper cs []
    end;

fun all_same_color ls =
    case ls of
	[] => true
      | c::[] => true
      | c::cs => let val tmp = card_color c
		 in all (fn x => tmp = card_color x) cs
		 end;

fun sum_cards cs =
    let fun helper ls acc =
	    case ls of
		[] => acc
	      | l::lcs => helper lcs (acc + (card_value l))
    in helper cs 0
    end;


    




