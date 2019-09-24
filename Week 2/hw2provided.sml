(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, lst) =
    case lst of
	[] => NONE
     |  x :: xs' => if same_string(x, s)
		    then SOME xs'
		    else case all_except_option(s, xs') of
			NONE => NONE
		     |  SOME y => SOME(x :: y)
				      
					  

fun get_substitutions1(lst, s) =
    let fun get_one_list NONE = []
	  | get_one_list (SOME lst) = lst
    in
	case lst of
	    [] => []
	  | x :: xs => get_one_list(all_except_option(s, x)) @ get_substitutions1(xs, s)
    end

fun get_substitutions2(lst, s) =
    let fun get_lists(acc, lst) =
	    case lst of
		[] => acc
	      | x :: xs =>
		case all_except_option(s, x) of
		    NONE => get_lists(acc, xs)
		  | SOME lst => get_lists(acc @ lst, xs)
    in
	get_lists([], lst)
    end

fun similar_names (lst, name as {first = f, middle = m, last = l}) =
    let val subs = f :: get_substitutions2(lst, f)
	fun get_all_names subs =
	    case subs of
		[] => []
	     |  x :: xs => {first = x, middle = m, last = l} :: (get_all_names xs)
    in
	get_all_names(subs)
    end
	

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color c =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value c =
    case c of
	(_, Ace) => 11
      | (_, Num x) => x
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
     |  x :: xs  => if x = c
		    then xs
		    else x :: remove_card(xs, c, e)

fun all_same_color cs =
    case cs of
	[] => true
     |  x :: []  => true
     |  x :: y :: xs => (card_color x) = (card_color y) andalso all_same_color (y :: xs)

fun sum_cards cs =
    let fun get_cards_sum (acc, cs) =
	    case cs of
		[] => acc
	     |  x :: xs => get_cards_sum(acc + (card_value x), xs)
    in
	get_cards_sum(0, cs)
    end

fun score (cs, goal) =
    let val score = sum_cards cs
	val pre_score = if score >= goal then 3 * (score - goal) else goal - score
    in
	if all_same_color cs then pre_score div 2 else pre_score
    end

fun officiate (cs, moves, goal) =
    let fun one_step(held_list, move) =
	    case (cs, move) of
		(_, Discard c) => remove_card(held_list, c, IllegalMove)
	     |  ([], Draw) => held_list
	     |  (x :: _, Draw) => x :: held_list
	fun run(held_list, moves, goal) =
	    case moves of
		[] => score(held_list, goal)
	     |  x :: xs =>
		let val list_after_drawing = one_step(held_list, x)
		    val s = sum_cards list_after_drawing
		in
		    if s > goal then score(list_after_drawing, goal) else run(list_after_drawing, xs, goal)
		end
    in
	run([], moves, goal)
    end
    
