fun is_older(x: (int * int * int), y: (int * int * int)) =
  let
      val year_cond = #1 x < #1 y;
      val month_cond = #1 x = #1 y andalso #2 x < #2 y
      val day_cond = #1 x = #1 y andalso #2 x = #2 y andalso #3 x < #3 y
  in
      if year_cond orelse month_cond orelse day_cond
      then true
      else false
  end

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else
      let
	  val head = hd dates
      in
	  if #2 head = month
	  then 1 + number_in_month(tl dates, month)
	  else number_in_month(tl dates, month)
      end

fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null dates orelse null months
  then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else
    let
	    val head = hd dates
    in
	    if #2 head = month
      then head :: dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)
    end

fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null dates orelse null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(xs: 'a list, n: int) =
  if n > 1
  then get_nth(tl xs, n - 1)
  else hd xs

fun date_to_string(date: int * int * int) =
  let
    val months = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum: int, xs: int list) =
  let
     fun add_to_sum(cnt: int, partial_sum: int) =
        if partial_sum < sum
        then add_to_sum(cnt + 1, get_nth(xs, cnt + 1) + partial_sum)
        else cnt - 1
  in
      add_to_sum(0, 0)
  end		

fun what_month(day_of_year: int) =
  let
      val day_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day_of_year, day_of_month) + 1
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else let
        fun oldest_nonempty(dates: (int * int * int) list) =
	  if null (tl dates)
          then hd dates
          else let
	         val tl_ans = oldest_nonempty(tl dates)
               in
		 if is_older(hd dates, tl_ans)
		 then hd dates
	         else tl_ans
	       end
        in
            SOME (oldest_nonempty dates)
        end
      
