
(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2code.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
(*val test1_1 = all_except_option ("alphabet", ["string"]) = NONE
val test1_2 = all_except_option ("string", ["string", "alphabet"]) = SOME ["alphabet"]
val test1_3  = all_except_option ("string", ["westfield", "string", "alphabet"]) = SOME ["westfield", "alphabet"]
val test1_4  = all_except_option ("string", ["westfield", "alphabet", "string"]) = SOME ["westfield", "alphabet"]
val test1_5  = all_except_option ("string", ["string", "westfield", "alphabet"]) = SOME ["westfield", "alphabet"]
val test1_6  = all_except_option ("string", ["westfield", "alphabet", "cow", "string", "two"]) = SOME ["westfield", "alphabet", "cow", "two"]*)

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =  ["Fredrick","Freddie","F"]
val test2_2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") =  ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
(*val test3_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =  ["Freddie","F","Fredrick"]
val test3_2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") =  ["Geoff", "Jeffrey","Jeffrey"]
 *)					  
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
(*val test7_1 = ((remove_card ([(Hearts, Jack)], (Hearts, Ace), IllegalMove); false)
	      handle IllegalMove => true)
val test7_2 = remove_card ([(Hearts, Jack), (Hearts, Ace), (Clubs, Num 3), (Spades, Num 9)], (Hearts, Ace), IllegalMove) = [(Hearts, Jack), (Clubs, Num 3), (Spades, Num 9)]
val test7_3 = remove_card ([(Hearts, Ace), (Hearts, Ace), (Clubs, Num 3), (Spades, Num 9)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Clubs, Num 3), (Spades, Num 9)]*)
									    
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
(*val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Ace), (Diamonds, Ace)] = true
val test8_2 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Ace), (Hearts, King)] = false*)

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
(*val test10_1 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test10_2 = score ([(Hearts, Num 2),(Diamonds, Num 4)],9) = 1
val test10_3 = score ([(Spades, Num 2),(Clubs, Num 4), (Spades, Num 4)],9) = 1*)

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test14 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test15 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test16 = ((officiate_challenge ([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test17 = officiate_challenge ([(Hearts,Num 2),(Hearts,Num 3),(Diamonds,Num 2),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 24
		   
val test18 = careful_player ([(Hearts, Num 2),(Clubs, Num 4)], 4) = [Discard (Hearts, Num 2), Draw]

val test19 = careful_player ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],42)
             = [Draw,Draw,Draw,Draw]

val test20 = careful_player ([(Clubs,Jack),(Spades, Num(8))],42) = [Draw, Draw]

val test21 = careful_player ([(Hearts,Num 2),(Hearts,Num 3),(Diamonds,Num 2),(Spades,Ace)],42)
             = [Draw,Draw,Draw,Draw]

