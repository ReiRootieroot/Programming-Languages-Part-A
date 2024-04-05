(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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
