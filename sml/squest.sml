open Math;
open List;

val inc = (fn i => i + 1);
val dec = (fn i => i - 1);
val sqr = (fn i => i * i);

fun div' (n:int) (a:int) = 0 = (n mod a); 
val even' = (fn x => div' x 2);


val odd' = (fn x => not (even' x));

fun expt a 0 = 1
  | expt a m = a * (expt a (dec m));

fun range i j =
    let fun incdec m =
	    if m < j then inc m else dec m
	fun generate_list a res =
	    if a = j
	    then res
	    else generate_list (incdec a) (res @ [a])
    in
	generate_list i []
    end;

(*
fun srange (i) = srange (0, i, (if i > 0 then 1 else ~1))
  | srange (i, j) = srange (i, j, (if i > j then ~1 else 1))
  | srange (i, j, k) =
    let fun big_helper m res =
	    if m <= j then res else big_helper (m + k) (res @ [m])
	fun small_helper m res =
	    if m >= j then res else small_helper (m + k) (res @ [m])
    in
	if i >= j then big_helper i [] else small_helper i []
    end;
*)

fun elem' p [] = false
  | elem' p (x::xs) =
    if x = p
    then true
    else elem' p xs;

fun map_index f ls =
    let fun helper i [] = []
	  | helper i (x::xs) = (x,f x, i)::(helper (inc i) xs)
    in
	helper 0 ls
    end;

fun find_string x [] = NONE
  | find_string x ls =
    let fun helper res [] = []
	  | helper res (m::ms) =
	    if m = x then helper (m::res) ms else helper res ms
    in
	SOME (helper [] ls)
    end;












