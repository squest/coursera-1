
fun is_older (d1 : int*int*int, d2 : int*int*int) =
    let
	val dd1 = (#1 d1)*10000 + (#2 d1)*100 + (#3 d1)
	val dd2 = (#1 d2)*10000 + (#2 d2)*100 + (#3 d2)
    in
	dd1 < dd2
    end;

fun is_even (i) = 0 = (i mod 2);

fun filter f [] = []
  | filter f (x::xs) =
    if f x then x::(filter f xs) else filter f xs;

fun sum (x::nil) = x
  | sum (x::xs) = x + sum xs;

fun number_in_month (ds, mo) =
    let
	fun month (x : int*int*int) = (#2 x) = mo
    in
	length (filter month ds)
    end;

fun number_in_months (ds, mos : int list) =
    let
	fun months mo = number_in_month (ds,mo)
    in
	sum (map months mos)
    end;

