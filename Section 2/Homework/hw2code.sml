(*Code by Rei Rutkowski for HW 2 of Section 2 of Programming Languages, Part A, presented by University of Washington Coursera*)

(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*PART A*)

fun all_except_option (name, []) = NONE
  | all_except_option (name, x :: xs') =
    case same_string (x, name) of
	true => SOME xs'
     |  false  => case all_except_option (name, xs') of
		      NONE => NONE
		    | SOME v => SOME(x :: v)
	
fun get_substitutions1 ([], s) = []
  | get_substitutions1 (x::xs', s) = 
    case all_except_option(s, x) of
	NONE =>  get_substitutions1(xs', s)
      | SOME v =>  v @ get_substitutions1(xs', s)
	
(*PART C*)
fun get_substitutions2 (substitutions, s) =
    let
	fun tail_recursion ([], acc) = acc
	  | tail_recursion (x :: xs', acc) = 
	    case all_except_option(s, x) of
		NONE => tail_recursion(xs', acc)
	      | SOME v => tail_recursion(xs', v @ acc)
    in
	tail_recursion (substitutions, [])
    end

	
(*PART D*)
	(*
fun similar_names ([], _) = []
  | similar_names (xs, name : {first : string, last : string, middle : string}) =
    case (get_substitutions1 (xs, #first name), name)  of
	([], name) => []
      | (xs, name) => let fun make_list (ys) =
				    case ys of
					[] => []
				      | y :: ys' => {first = y, last = #last name, middle = #middle name} :: make_list(ys')
			   in name :: make_list(xs)
			   end
	*)
fun similar_names (list_names : string list list, full_name : {first : string, last : string, middle : string}) =
    let val {first = f, last = l, middle = m} = full_name
	fun get_names (list_names_i : string list) =
	    case list_names_i of
		[] => []
	      | x :: xs' => {first = x, last = l, middle = m} :: get_names(xs')
    in
	get_names(f :: get_substitutions1(list_names, f))
    end

	

(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*PART A*)
fun card_color (c : card) : color =
    case c of
	(Diamonds, _) => Red 
      | (Hearts, _) => Red
      | (Clubs, _) => Black
      | (Spades, _) => Black 

(*PART B*)
fun card_value (c : card) =
    case c of
	(_, Num i) => i
      | (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
		   
(*PART C*)
fun remove_card (cs : card list, c : card, e) =
    case cs of
	[] => raise e
      | x :: xs' => case (x = c) of
			true => xs'
		      | false => case remove_card(xs', c, e) of
					  [] => [x]
					| y :: ys' => x :: y :: ys'

(*PART D*)
fun all_same_color (cs : card list) =
    case cs of
	[] => true
      | x :: [] => true
      | x :: y :: xs' => (card_color(x) = card_color(y) andalso all_same_color(y :: xs'))
							  
(*PART E*)
fun sum_cards (cs : card list) =
    let fun count_card (xs : card list, acc : int) =
	    case xs of
		[] => acc
	      | x :: xs' => count_card (xs', card_value(x) + acc)
    in
	count_card(cs, 0)
    end


(*PART F*)
fun score (cs : card list, goal) =
    let val sum = sum_cards(cs)
	fun int_divider () =
	    if all_same_color (cs)
	    then 2
	    else 1
    in
	if sum > goal
	then ((sum - goal) * 3) div int_divider()
	else  (goal - sum) div int_divider() 
    end


(*PART G*)
fun officiate (cards : card list, moves: move list, goal : int) =
    let
	fun play_game (move_list : move list, deck : card list, current_hand : card list) =
	    case move_list of
		[] => score(current_hand, goal)  
	      | m :: ms' => case m of
				Draw => (case deck of
					    [] => score(current_hand, goal)
					  | d :: ds' => if sum_cards(d :: current_hand) > goal
							then score(d :: current_hand, goal)
							else play_game (ms', ds', d :: current_hand))
			      | Discard c1 => play_game(ms', deck, remove_card(current_hand, c1, IllegalMove))
    in
	play_game(moves, cards, [])
    end

(*problem 3- challenge problems*)
(*PART A*)
fun score_challenge (card_list : card list, goal) =
    let val score_before_function = score(card_list, goal)
	fun int_divider () =
	    if all_same_color (card_list)
	    then 2
	    else 1
	fun get_score (cs : card list, held : card list, goal_acc : int) =
	    case cs of
		[] => goal_acc
	      | (s, Ace) :: xs' => (let
				       val replace_ace_score = score(held @ [(s, Num 1)] @ xs', goal)
				   in
				       if replace_ace_score < goal_acc
				       then get_score(xs', (s, Num 1) :: held, replace_ace_score)
				       else get_score(xs', (s, Ace) :: held, goal_acc)
				   end
				   )
	      | xs :: xs' => get_score(xs', xs :: held, goal_acc)
    in
	get_score (card_list, [], score_before_function)
    end

	
fun officiate_challenge (cards : card list, moves: move list, goal : int) =
    let
	fun play_game (move_list : move list, deck : card list, current_hand : card list) =
	    case move_list of
		[] => score_challenge(current_hand, goal)
	      | m :: ms' => case m of
				Draw => (case deck of
					    [] => score_challenge(current_hand, goal)
					  | d :: ds' => if sum_cards(d :: current_hand) > goal
							then score_challenge(d :: current_hand, goal)
							else play_game (ms', ds', d :: current_hand))
			      | Discard c1 => play_game(ms', deck, remove_card(current_hand, c1, IllegalMove))
    in
	play_game(moves, cards, [])
    end

(*PART B*)
fun careful_player (cards : card list, goal : int) =
    let fun make_moves (cs : card list, c_acc : card list) =
	    case cs of
		[] => []
	      | cs :: [] => if score(c_acc, goal) = 0
			     then []
			     else 
				if score(c_acc, goal) + 10 < goal
				then Draw :: make_moves ([], cs :: c_acc)
				else []
	      | c1 :: (c2 :: cs') => if score(c2 :: c_acc, goal) = 0
				     then [Discard c1, Draw] @ make_moves(cs', c2 :: c_acc)
				     else
					 if score(c_acc, goal) + 10 < goal
					 then Draw :: make_moves (c2 :: cs', c1 :: c_acc)
					 else []
    in
	make_moves (cards, [])
    end
