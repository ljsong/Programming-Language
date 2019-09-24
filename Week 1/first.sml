(* This is a comment. This is our first program *)

val x = 34;
(* static environment: x: int *)
(* dynamic environment: x --> 34 *)

val y = 17;
(* static environment: x: int, y: int *)
(* dynamic enviroment: x --> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* static environment: x: int, y: int, z: int *)
(* dynamic environment: x --> 34, y --> 17, z --> 79 *)

val abs_of_z = if z < 0 then 0 - z else z;

val abs_of_z_simpler = abs z;

fun pow(x: int, y: int) =
    if y = 0 then 1 else x * pow(x, y - 1)

fun swap(pr: int * bool) =
    (#2 pr, #1 pr)
fun add_two_pairs(pr1: int * int, pr2: int * int) =
    (#1 pr1 + #1 pr2, #2 pr1 + #2 pr2)
