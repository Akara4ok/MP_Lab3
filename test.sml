use "task.sml"


fun test(function_name : string, true_result, fact_result) =
    if true_result = fact_result
    then (function_name, "Ok")
    else (function_name, "Failed");



(* 1 *)
(* a *)
test("all_except_option", SOME ["1", "3", "4"], all_except_option("2", ["2", "1", "3", "4"]));
test("all_except_option", SOME [], all_except_option("2", ["2"]));
test("all_except_option", NONE, all_except_option("2", ["1", "3", "4"]));


(* b *)
test("get_substitutions1", ["Fredrick","Freddie","F"], get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"));
test("get_substitutions1", ["Jeffrey","Geoff","Jeffrey"], get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff"));
test("get_substitutions1", [], get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"]], "Vlad"));


(* c *)
test("get_substitutions2", ["Fredrick","Freddie","F"], get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred"));
test("get_substitutions2", ["Jeffrey","Geoff","Jeffrey"], get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff"));
test("get_substitutions2", [], get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"]], "Vlad"));


(* d *)
test("similar_names", 
    [{first="Fred", last="Smith", middle="W"}, 
        {first="Fredrick", last="Smith", middle="W"},
        {first="Freddie", last="Smith", middle="W"},
        {first="F", last="Smith", middle="W"}], 
    similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}));

test("similar_names", 
    [{first="Fred", last="Smith", middle="W"}], 
    similar_names([["Fred"],["Elizabeth","Betty"]], {first="Fred", middle="W", last="Smith"}));

test("similar_names", 
    [{first="Fred", last="Smith", middle="W"}], 
    similar_names([["Elizabeth","Betty"]], {first="Fred", middle="W", last="Smith"}));



(* 2 *)
val test_card1 = (Hearts, Jack);
val test_card2 = (Clubs, Num 8);
val test_card3 = (Diamonds, Ace);

val test_card_list1 = [test_card1, test_card2, test_card3];
val test_card_list2 = [test_card1, test_card3];
val test_card_list3 = [];


(* a *)
test("card_color", Red, card_color(test_card1));
test("card_color", Black, card_color(test_card2));
test("card_color", Red, card_color(test_card3));

(* b *)
test("card_value", 10, card_value(test_card1));
test("card_value", 8, card_value(test_card2));
test("card_value", 11, card_value(test_card3));


(* с *)
test("remove_card", [test_card2, test_card3], remove_card(test_card_list1, test_card1, IllegalMove));
test("remove_card", [], remove_card(test_card_list2, test_card2, IllegalMove));
test("remove_card", [], remove_card(test_card_list3, test_card2, IllegalMove));


(* d *)
test("all_same_color", false, all_same_color(test_card_list1));
test("all_same_color", true, all_same_color(test_card_list2));
test("all_same_color", true, all_same_color(test_card_list3));


(* e *)
test("sum_cards", 29, sum_cards(test_card_list1));
test("sum_cards", 21, sum_cards(test_card_list2));
test("sum_cards", 0, sum_cards(test_card_list3));


(* f *)
test("score", 3, score(test_card_list1, 28));
test("score", 1, score(test_card_list2, 23));
test("score", 2, score(test_card_list1, 31));

(* g *)
test("officiate", 3, officiate(test_card_list1, [Draw, Draw, Draw, Draw], 28));
test("officiate", 5, officiate(test_card_list1, [Draw, Draw, Discard(Clubs, Num 8)], 20));
test("officiate", 9, officiate(test_card_list1, [Draw, Draw], 15));
