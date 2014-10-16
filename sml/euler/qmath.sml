open Math;
open List;

val inc = (fn i => i + 1);
val dec = (fn i => i - 1);
val sqr = (fn i => i * i);

val even' = (fn x => div' x 2);

val odd' = (fn x => not (even' x));

fun expt a 0 = 1
  | expt a m = a * (expt a (dec m));

fun div' (n:int) (a:int) =  0 = (n mod a);

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
