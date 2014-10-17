(* Coursera Programming Languages, Homework 3, Provided Code *)

open List;



exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
val isUpper = Char.isUpper;
val size = String.size;
val sub = String.sub;

fun only_capitals ss = filter (fn x => isUpper (sub (x,0))) ss;

val sama = ["sabda", "Well","Done","like","hEll"];

fun add (x,y) = x+y;

fun longest_string1 ss =
    let fun maxi (a,b) = if (size a) > (size b) then a else b
    in
	case ss of
	    [] => ""
	  | (x::xs) => foldl maxi x xs
    end;

fun longest_string2 ss =
    let fun maxi (a,b) = if (size a) >= (size b) then a else b
    in
	case ss of
	    [] => ""
	  | (x::xs) => foldl maxi x xs
    end;

fun longest_string_helper f ss =
    let fun maxi (a,b) = if f (size a,size b) then a else b
    in
        case ss of
            [] => ""
          | (x::xs) => foldl maxi x xs
    end;

val longest_string3 =
    longest_string_helper (fn (x,y) => x > y);

val longest_string4 =
    longest_string_helper (fn (x,y) => x >= y);




