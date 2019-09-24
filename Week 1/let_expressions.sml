fun silly1() =
    let
	val x = 1
    in
	(let val x = 2 in x + 1 end) + (let val y = x + 1 in x + y end)
    end


fun max(xs: int list) =
    if null xs
    then NONE
    else
	let val tl_ans = max(tl xs)
	in
	    if isSome tl_ans andalso valOf tl_ans > hd xs
	    then tl_ans
	    else SOME (hd xs)
	end
