fun is_older (d1 : int*int*int, d2 : int*int*int) =
    let val dd1 = (#1 d1)*10000 + (#2 d1)*100 + (#3 d1)
	val dd2 = (#1 d2)*10000 + (#2 d2)*100 + (#3 d2)
    in dd1 < dd2
    end;

fun dates_in_month (ds : (int*int*int) list, mo : int) =
    if null ds
    then []
    else if (#2 (hd ds)) = mo
    then (hd ds) :: dates_in_month (tl ds, mo)
    else dates_in_month (tl ds, mo);

fun append (ls1 : 'a list, ls2 : 'a list) =
    if null ls1
    then ls2
    else (hd ls1) :: append (tl ls1, ls2);

fun dates_in_months (ds : (int*int*int) list, mos : int list) =
    if null mos
    then []
    else append (dates_in_month (ds, hd mos),
		 dates_in_months (ds, tl mos));

fun number_in_month (ds : (int*int*int) list, mo : int) =
    length (dates_in_month (ds, mo));

fun number_in_months (ds : (int*int*int) list, mos : int list) =
    length (dates_in_months (ds, mos));

fun get_nth (ls : 'a list, n : int) =
    if n = 1
    then hd ls
    else get_nth (tl ls, n - 1);

fun date_to_string (d : int*int*int) =
    let val months = ["January","February","March","April", "May",
		      "June","July","August","September", "October",
		      "November", "December"]
    in (get_nth (months, (#2 d))) ^ " "
	^ (Int.toString (#3 d)) ^ ", " ^ (Int.toString (#1 d))
    end;

fun number_before_reaching_sum (sum : int, ls : int list) =
    let fun helper (res, ls, last) =
	    if (res + (hd ls)) >= sum
	    then last
	    else helper (res + (hd ls), tl ls, 1+last)
    in helper (0,ls,0)
    end;

fun what_month (n : int) =
    let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1 + number_before_reaching_sum (n,days)
    end;

fun month_range (d1 : int,d2 : int) =
    if d1 > d2
    then []
    else (what_month d1) :: (month_range (d1+1, d2));

fun oldest (ds : (int*int*int) list) =
    if null ds
    then NONE
    else
	let fun helper ds =
		if null (tl ds)
		then hd ds
		else
		    let val loldest = helper (tl ds)
		    in
			if is_older ((hd ds), loldest)
			then (hd ds)
			else loldest
		    end
	in 
	    SOME (helper ds)
	end;
	    



