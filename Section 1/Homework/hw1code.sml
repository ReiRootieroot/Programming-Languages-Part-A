(*Code by Rei Rutkowski for HW 1 of Section 1 of Programming Languages, Part A, presented by University of Washington Coursera*)

(*PROBLEM 1*)
(*val 30_days = [4,6,9,11]
val 31_days = [1,3,5,7,8,10,12]*)


	
fun is_older (pr1 : int*int*int, pr2 : int*int*int) =
    let
	val thirty_days  = [4,6,9,11]
	fun get_days_bool (month : int, xs : int list) =
	    if not(null xs)
	    then
		if month = hd(xs)
		then true
		else get_days_bool (month, (tl xs))
	    else false
	fun total_days (month : int) =
	    if not (month = 2)
	    then
		if get_days_bool(month, thirty_days)
		then 30
		else 31
	    else 29
    in
	    
	if (#1 pr1) < (#1 pr2)
	then true
	else
	    if (#1 pr1) = (#1 pr2)
	    then
		if 1 <= (#2 pr1) andalso  (#2 pr1) < (#2 pr2) andalso (#2 pr2) <= 12
		then true
		else
		    if  (#2 pr1) = (#2 pr2)
		    then
			if 1 <= (#3 pr1) andalso (#3 pr1) < (#3 pr2) andalso (#3 pr2) <= total_days(#2 pr2)
			then true
			else false
		    else false
	    else false
    end
	
(*PROBLEM 2*)
fun number_in_month (xs : (int*int*int) list, month : int) =
    let fun count_month (xs: (int*int*int) list, counter : int) =
	    if null xs
	    then counter
	    else
		if #2 (hd xs) =  month
		then count_month (tl xs, counter + 1)
		else count_month (tl xs, counter)
    in
	count_month(xs, 0)
    end


(*PROBLEM 3*)
fun number_in_months (xs : (int*int*int) list, month : int list) =
    let fun count_month (xs: (int*int*int) list, month : int list, counter : int) =
	    if null month
	    then counter
	    else count_month (xs,
			      (tl month),
			      counter + (number_in_month (xs, (hd month)))
			     )
    in
	count_month(xs, month, 0)
    end
	

(*PROBLEM 4*)
fun dates_in_month (xs : (int*int*int) list, month : int) =
    let fun count_date (xs: (int*int*int) list, ys : (int*int*int) list) =
	    if null xs
	    then ys
	    else
		if #2 (hd xs) =  month
		then (hd xs) :: count_date(tl xs, ys)
		else count_date(tl xs, ys)
    in
	count_date(xs, [])
    end
	
(*PROBLEM 5*)
fun dates_in_months (xs : (int*int*int) list, month : int list) =
    let fun count_date (xs: (int*int*int) list, ys : int list, zs : (int*int*int) list) =
	    if null ys
	    then zs @ []
	    else count_date(xs,
			    (tl ys),
			    zs @ (dates_in_month(xs, (hd ys)))
			   )
    in
	count_date(xs, month, [])
    end

(*PROBLEM 6*)
fun get_nth (xs : string list, n : int) =
    let fun count_list (ys : string list, counter : int) =
	    if counter = n
	    then (hd ys)
	    else count_list (tl ys, counter + 1)
    in
	count_list (xs, 1)
    end
	
(*PROBLEM 7*)
fun date_to_string (date : int*int*int) =
    let
	val months_list = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
	val thirty_days  = [4,6,9,11]
	fun get_days_bool (month : int, xs : int list) =
	    if not(null xs)
	    then
		if month = hd(xs)
		then true
		else get_days_bool (month, (tl xs))
	    else false
	fun total_days (month : int) =
	    if not (month = 2)
	    then
		if get_days_bool(month, thirty_days)
		then 30
		else 31
	    else 29
	fun valid_day (month : int, day : int) =
	    if day > total_days(month)
	    then "(Invalid day)"
	    else (Int.toString day)
	fun get_month (xs : string list, counter : int) =
		if (#2 date) <= 12
		then
		    if counter = #2 date
		    then (hd xs)
		    else (get_month (tl xs, counter + 1))
		else "(Invalid month) "
	fun valid_year (year : int) =
	    if year > 0
	    then (Int.toString year)
	    else "(Invalid year)"
	in
	    get_month (months_list, 1) ^ valid_day(#2 date, #3 date) ^ ", " ^ valid_year(#1 date)
    end

(*PROBLEM 8*)
fun number_before_reaching_sum (sum : int, num_list : int list) =
    let fun sum_fun (xs : int list, counter : int, index : int) =
	    if (counter + (hd xs)) < sum
	    then sum_fun ((tl xs), counter + (hd xs), index + 1)
	    else index
    in
	sum_fun (num_list, 0, 0)
    end

(*PROBLEM 9*)
fun what_month (day : int) =
    let val days_in_each_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	if day < 1 orelse day > 365
	then 0
	else number_before_reaching_sum (day, days_in_each_month) + 1
    end

(*PROBLEM 10*)
fun month_range (day1 : int, day2 : int) =
    let fun generate_list (day_count : int, month_list : int list) =
	    if day_count < day1
	    then  month_list
	    else generate_list(day_count - 1, what_month(day_count) :: month_list)
    in
	if day1 > day2
	then []
	else generate_list (day2 - 1, [what_month(day2)])
		      
    end

(*PROBLEM 11*)
fun oldest (date_list : (int*int*int) list) =
    if null date_list
    then NONE
    else let
	fun get_oldest (xs : (int*int*int) list) =
	    if null (tl xs)
	    then hd xs
	    else let val tl_ans = get_oldest(tl xs)
		 in
		     if is_older (hd xs, tl_ans)
		     then hd xs
		     else tl_ans
		 end
    in
	SOME (get_oldest date_list)
    end

(*PROBLEM 12- CHALLENGE PROBLEM*)
fun numbers_in_months_challenge (zs : (int*int*int) list, month_list : int list) =
    let fun remove_duplicate (ys : int list, final_list : int list) =
	    if null ys
	    then final_list
	    else
		let fun remove_one_month (ws : int list, month : int) =
		    if null ws
		    then ws
		    else
			if (hd ws) = month
			then remove_one_month (tl ws, month)
			else (hd ws):: remove_one_month (tl ws, month)
	    in
		remove_duplicate(remove_one_month ((tl ys), (hd ys)), (hd ys) :: final_list)
	    end
    in
	 number_in_months(zs, remove_duplicate (month_list, []))
    end

fun dates_in_months_challenge (zs : (int*int*int) list, month_list : int list) =
    let fun reverse_order (vs : (int*int*int) list, list_counter :  (int*int*int) list) =
	    if null vs
	    then list_counter
	    else reverse_order((tl vs), (hd vs) :: list_counter)



	fun remove_duplicate (ys : int list, final_list : int list) =
	    if null ys
	    then final_list
	    else
		let fun remove_one_month (ws : int list, month : int) =
		    if null ws
		    then ws
		    else
			if (hd ws) = month
			then remove_one_month (tl ws, month)
			else (hd ws):: remove_one_month (tl ws, month)
	    in
		remove_duplicate(remove_one_month ((tl ys), (hd ys)), (hd ys) :: final_list)
	    end
    in
	reverse_order(dates_in_months(zs, remove_duplicate (month_list, [])), [])
    end

(*PROBLEM 13*)
fun reasonable_date (date : int*int*int) =	
    let
	fun valid_year () =
	    1 <= (#1 date)
	fun valid_month () =
	    1 <= (#2 date) andalso (#2 date) <= 12
	val thirty_days  = [4,6,9,11]
	fun valid_day () =
	    let
		fun total_days (month : int) =
		    if not (month = 2)
		    then
			let
			    fun get_days_bool (month : int, xs : int list) =
				if not(null xs)
				then
				    if month = hd(xs)
				    then true
				    else get_days_bool (month, (tl xs))
				else false
			in
			    if get_days_bool(month, thirty_days)
			    then 30
			    else 31
			end
		    else
			if (#1 date) mod 400 = 0
			then 29
			else 28
	    in
		1 <= (#3 date) andalso (#3 date) <= total_days(#2 date)
	    end
    in
	valid_year() andalso valid_month() andalso valid_day()
    end
