open List

fun sum_even_fibo (lim: int) =
    let fun looper a b res =
	    if a > lim
	    then res
	    else if (0 = a mod 2)
	    then looper (a+b) a (res+a)
	    else looper (a+b) a res
    in looper 1 0 0
    end;


