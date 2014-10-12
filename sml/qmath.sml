open Math;

use "squest.sml";

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
