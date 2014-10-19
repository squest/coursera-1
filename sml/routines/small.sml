use "generic.sml";

fun prime' 2 = true 
  | prime' (n:int) =
    let fun find_prime i =
	    if (sqr i) > n
	    then true
	    else if div' n i
	    then false
	    else find_prime (inc2 i)
    in
	if even' n then false else find_prime 3
    end;


fun next_prime 2 = 3
  | next_prime (n:int) =
    let fun find_next i =
	    if prime' i then i else find_next (inc2 i)
    in if even' n
       then find_next (inc n)
       else if prime' (inc2 n)
       then (inc2 n)
       else find_next (inc2 n)
    end;

fun primes_under (lim :int) =
    let fun find_primes i res =
	    if i >= lim
	    then res
	    else find_primes (next_prime i) (i::res)
    in find_primes 2 []
    end;

fun count_primes (lim:int) =
    let fun countPrimes i res =
	    if i >= lim
	    then res
	    else countPrimes (next_prime i) (inc res)
    in countPrimes 2 0
    end;

fun sum_primes (lim:int) =
    let fun sumPrimes i res =
	    if i >= lim
	    then res
	    else sumPrimes (next_prime i) (Int.toLarge ((Int.toLarge i) + res))
    in sumPrimes 2 0
    end;

fun function (x:int) =
    let
        val t = Timer.startCPUTimer()
        val result = count_primes x
    in
        print (Time.toString(#usr(Timer.checkCPUTimer(t))) ^ "\n");
        result
    end;

