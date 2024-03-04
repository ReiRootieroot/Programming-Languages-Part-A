(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1code.sml";
val test1 = is_older ((1,2,3),(2,3,4)) = true;
(*val test1_2 = is_older ((3,4,5), (2,3,5)) = false;
val test1_3 = is_older ((10,4,900),(30,5,90)) = false;
val test1_total_days = total_days(5) = 31;
val test1_getdaysbool = get_days_bool(5, [4,6,9,11])*)

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;
											    val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
											    val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];

											    val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there";

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013";
(*val test7_1 = date_to_string (~1, 13, 40) = "(Invalid month) (Invalid day), (Invalid year)";*)
					      
val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;
(*val test8_1 = number_before_reaching_sum (30, [10,20,3,4,5]) = 1;
*)
val test9 = what_month 70 = 3;

val test_10 = month_range (31, 34) = [1,2,2,2];
(*val test_10_1 = month_range (43, 32) = [];
val test_10_2 = month_range (31, 32) = [1, 2];*)

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);

val test12_1 =  numbers_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = 3;

val test12_2 =  dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];

val test13_1 = reasonable_date (2012,2,28) = true;
val test13_2 = reasonable_date (1600,2,29) = true;
val test13_3 = reasonable_date (1601,2,28) = true;
