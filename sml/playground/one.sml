open Math;
val sample = [1,2,3,4,5,6,7,8,9];

val inc = fn x => x + 1;
val dec = fn x => x - 1;

val floor = Real.floor;
val ceil = Real.ceil;
val round = Real.round;

fun is_even (x:int) = (0 = (x mod 2));

fun expt (a:int, m:int) =
    if (m = 0) then 1 else a * expt (a, dec m);

fun range (i:int, j : int) =
    if i = j then []
    else i :: (range ((inc i), j));

fun filter f nil = []
  | filter f (x::xs) =
    if f x then x :: (filter f xs)
    else (filter f xs);

fun is_prime (p:int) =
    if p < 2 then false
    else if (p = 2) then true
    else if is_even p then false
    else
	let val lim = ceil (sqrt (real p))
	    fun prime_helper (i:int) =
		if i > lim then true
		else if (p mod i) = 0
		then false
		else prime_helper (i + 2)
	in
	    prime_helper (3)
	end;

fun fibo (i : int) =
    if i = 1 then 1
    else if i = 2 then 1
    else
	let
	    fun fibo_helper (n:int, ls : int list) =
		if n > i then ls
		else fibo_helper (inc n, [(hd ls) + (hd (tl ls)), hd ls])
	in
	    hd (fibo_helper (2, [1,1]))
	end;




