(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(*Code by Rei Rutkowski for HW 3 of Section 3 of Programming Languages, Part A, presented by University of Washington Coursera*)
(*PROBLEM 1*)
fun only_capitals los = List.filter(fn s => Char.isUpper(String.sub(s, 0))) los

(*PROBLEM 2*)		       
fun longest_string1 los = foldl(fn (a, b) => if String.size (a) > String.size (b) then a else b) "" los

(*PROBLEM 3*)
fun longest_string2 los =  foldl(fn (a, b) => if String.size (a) >= String.size (b) then a else b) "" los

(*PROBLEM 4*)
fun longest_string_helper f = fn los =>  foldl f "" los
val longest_string3 = longest_string_helper (fn (a, b) => if String.size (a) > String.size (b) then a else b)
val longest_string4 = longest_string_helper (fn (a, b) => if String.size (a) >= String.size (b) then a else b)

(*PROBLEM 5*)
val longest_capitalized =  longest_string3 o only_capitals

(*PROBLEM 6*)
val rev_string = String.implode o List.rev o String.explode

(*PROBLEM 7*)
fun first_answer f lot =
    case lot of
	[] => raise NoAnswer
      | x :: xs' => case f x of
			  SOME v =>  v
			| NONE => first_answer f xs'

(*PROBLEM 8*)
fun all_answers f lot =
    let
	fun create_list lot acc =
	    case lot of
		[] => SOME acc
	      | x :: xs' => case f x of
				SOME v => create_list xs' (acc @ v)
			      | NONE => NONE
    in
	create_list lot []
    end

(*---------------------------------------------------*)
(*PART 2- below code is provided by Dan Grossman*)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
					    
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(*PROBLEM 9- code provided by Rei Rutkowski*)
(*PART A*)
val count_wildcards = g (fn _ =>  1) (fn _ => 0)

(*PART B*)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size(x))

(*PART C*)
fun count_some_var s p = g (fn _ => 0) (fn x => if s = x then 1 else 0) p

(*PROBLEM 10*)
fun check_pat p =
    let
	fun get_names p =
	    case p of
	        Variable x => [x]
	      | TupleP ps => List.foldl(fn (a, b) => get_names(a) @ b) [] ps
	      | ConstructorP (_, c) => get_names (c) 
	      |  _ => []
	fun is_unique name_list =
	    case name_list of
		[] => true
	      | (x :: xs') =>  if List.exists(fn y => y = x) xs'
			       then false
			       else is_unique xs'
    in
	is_unique(get_names p)
    end
