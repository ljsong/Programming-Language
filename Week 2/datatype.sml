datatype mytype = TwoInts of int * int
       | Str of string
       | Pizza

fun f x =
    case x of
	Pizza => 3
     |  TwoInts(i1, i2) => i1 + i2
     |  Str s => String.size s 
						 
datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

fun eval e =
    case e of
	Constant(x) => x
      | Negate(x) => ~(eval x)
      | Add(x1, x2) => eval(x1) + eval(x2)
      | Multiply(x1, x2) => eval(x1) * eval(x2)

fun max_constant e =
    let fun max_of_two(e1, e2) =
	    let val m1 = eval e1
		val m2 = eval e2
	    in if m1 > m2 then m1 else m2
	    end
    in
	case e of
	    Constant i => i
	  | Negate e => max_constant e
	  | Add(e1, e2) => max_of_two(e1, e2)
	  | Multiply(e1, e2) => max_of_two(e1, e2)
    end

datatype ('a, 'b) tree =
	 Node of 'a * ('a, 'b) tree * ('a, 'b) tree
	 | Leaf of 'a

fun sum_of_tree t =
    case t of
	Leaf i => i
      | Node(i, lft, rgt) => i + sum_of_tree lft + sum_of_tree rgt	
