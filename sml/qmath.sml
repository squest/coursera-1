open Math;

use "squest.sml";

fun add2 x = x + 2;

fun sqr x = x * x;

fun prime' (n:int) =
    let fun find_prime i =
	    if (sqr i) > n
	    then true
	    else if div' n i 
	    then false
	    else find_prime (add2 i) 
    in
	if n < 10
	then elem' n [2,3,5,7]
	else if even' n 
	then false
	else find_prime 3 
    end;

fun next_prime p =
    let fun find_next p =
	    if prime' p
	    then p
	    else find_next (add2 p)
    in
	if p < 2 
	then 2
	else if p = 2 
	then 3
	else if even' p
	then find_next (inc p)
	else find_next (add2 p)
    end;

fun iterate f i g =
    case g i of
	true => i :: (iterate f (f i) g)
      | false => [];

fun primes_under lim = iterate next_prime 2 (fn x => x < lim);
fun function x =
    let
        val t = Timer.startCPUTimer()
        val result = length (primes_under x)
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;
