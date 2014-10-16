(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2provided.sml";


val test1 = all_except_option("string", ["string"]) = SOME [];

val test2 = get_substitutions1([["foo"],["there"]], "foo") = []

val sample1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];

val sample2 = {first="Fred", middle="W", last="Smith"}

val test2a = ["Fredrick","Freddie","F"] = get_substitutions1 (sample1,"Fred");

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []

val test2a = ["Fredrick","Freddie","F"] = get_substitutions2 (sample1,"Fred");

val test4 = similar_names
([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]




val test5 = card_color((Clubs, Num 2)) = Black

val test6 = card_value((Clubs, Num 2)) = 2

val test7 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove)
	    = []

val test8 = all_same_color([(Hearts, Ace), (Hearts, Ace)]) = true

val test8a = all_same_color([(Hearts, Ace)]) = true

val test8b = all_same_color([]) = true

val test8c = all_same_color([(Hearts, Ace), (Spades, Ace)]) = false


val test9 = sum_cards([(Clubs, Num 2),(Clubs, Num 2)]) = 4

val test9a = sum_cards([(Clubs, Num 2),(Clubs, Jack)]) = 12

val test9b = sum_cards([(Clubs, Num 2),(Clubs, Ace), (Hearts, Queen)]) = 23


val test10 = score([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test10a = score([(Hearts, Num 2),(Clubs, Num 4), (Spades, Ace)],10
		   ) = 21

val test10b = score([(Spades, Num 2),(Clubs, Num 4),
		     (Spades,Ace)],30) = 6

val test10c = score([(Hearts, Num 2)], 15)

val test11 = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) =
	     6

val test11a = officiate([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15)


val test12 = officiate([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             


val test11b = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)]
				 ,[Draw], 15) = 6

val res11b = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)]
				 ,[Draw], 15)

val test11c = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15)


val test12a = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3


val test12b = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                       [Draw,Draw,Draw,Draw,Draw],
                       42)
             = 3

val test13a = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

