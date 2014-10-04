(* Homework no 1 *)

fun is_older (d1 : int*int*int, d2 : int*int*int) =
    let
	val dd1 = (#1 d1)*10000 + (#2 d1)*100 + (#3 d1)
	val dd2 = (#1 d2)*10000 + (#2 d2)*100 + (#3 d2)
    in
	dd1 < dd2
    end;

fun dates_in_month (ds : (int*int*int) list, mo : int) =
    if null ds
    then []
    else 
	if (#2 (hd ds)) = mo
	then (hd ds) :: dates_in_month (tl ds, mo)
	else dates_in_month (tl ds, mo);

fun append (ls1 , ls2) =
    if null ls1
    then ls2
    else (hd ls1) :: append (tl ls1, ls2);

fun dates_in_months (ds : (int*int*int) list, mos : int list) =
    if null mos
    then []
    else append (dates_in_month (ds, hd mos),
		 dates_in_months (ds, tl mos));

fun number_in_month (ds, mo) = length (dates_in_month (ds, mo));

fun number_in_months (ds, mos) = length (dates_in_months (ds, mos));

fun get_nth (ls, n) =
    if n = 1
    then hd ls
    else get_nth (tl ls, n - 1);

fun date_to_string (d : int*int*int) =
    let
	val months = ["January","February","March","April", "May",
		      "June","July","August","September", "October",
		      "November", "December"]
    in
	(get_nth (months, (#2 d))) ^ " "
	^ (Int.toString (#3 d)) ^ ", " ^ (Int.toString (#1 d))
    end;

fun number_before_reaching_sum (sum, ls) =
    let
	fun helper (res, ls, last) =
	    if (res + (hd ls)) >= sum
	    then last
	    else helper (res + (hd ls), tl ls, hd ls)
    in
	helper (0,ls,hd ls)
    end;





