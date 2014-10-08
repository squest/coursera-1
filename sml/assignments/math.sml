open Math;

val inc = fn x => x + 1;
val dec = fn x => x - 1;

val floor = Real.floor;
val ceil = Real.ceil;
val round = Real.round;
type big = IntInf.int;

fun is_even (x:int) = (0 = (x mod 2));

fun expt (a:int) (m:int) =
    if (m = 0) then big 1 else big (a * expt (a, dec m));

fun range (i:int) (j:int) =
    if i = j then []
    else i :: (range (inc i) j);









