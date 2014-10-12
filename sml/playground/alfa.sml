open Math;
open List;

fun inc i = 1 + i;
fun dec i = i - 1;

fun sqr (n:int) = n * n;

fun div' (n:int) (a:int) =  0 = (n mod a);

fun even' (n:int) = (div' n 2);

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

fun prime' (n:int) =
    let fun helper i =
	    if (sqr i) > n
	    then true
	    else if div' n i
	    then false
	    else helper (i+2)
    in
	if n < 10
	then elem' n [2,3,5,7]
	else if even' n
	then false
	else helper 3
    end;









