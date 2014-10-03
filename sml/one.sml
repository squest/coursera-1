open Math;
val sample = [1,2,3,4,5,6,7,8,9];

fun inc (x) = x + 1;
fun dec (x) = x - 1;

val incr = fn x => x + 1;

val floor = Real.floor;
val ceil = Real.ceil;
val round = Real.round;

fun is_even (x:int) = (0 = (x mod 2));

fun expt (a:int, m:int) =
    if (m = 0) then 1 else a * expt (a, dec m);

fun prime_helper (p:int, i:int, lim:int) =
    if i > lim
    then true
    else
	if (p mod i) = 0
	then false
	else prime_helper (p, i + 2, lim) ;

fun is_prime (p:int) =
    if p < 2
    then false
    else
	if (p = 2)
	then true
	else 
	    if is_even p
	    then false
	    else prime_helper (p, 3, ceil (sqrt (real p)));

fun fibo_helper (n:int, i:int, ls) =
    if i > n
    then ls
    else fibo_helper (n, inc i, [(hd ls) + (hd (tl ls)), hd ls]) ;

fun fibo (i : int) =
    if i = 1
    then 1
    else
	if i = 2
	then 1
	else hd (fibo_helper (i, 2, [1,1]));




