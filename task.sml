(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* a *)
fun all_except_option(str, strlist) = 
    let fun help_fun(strlist, acc, isFound) =
        case strlist of
            [] => (acc, isFound)
            |(x::xs) => if (same_string(x, str)) then help_fun(xs, acc, true) else help_fun(xs, x::acc, isFound)
    in
        let fun rev(lst, acc) =
            case lst of
            [] => acc
            | x::xs => rev(xs, x::acc)
        in
            case help_fun(strlist, [], false) of
            (_, false) => NONE
            | ([], true) => SOME([])
            | (x::xs, true) => SOME(rev(x::xs, []))
        end
    end


(* b *)
fun get_substitutions1(strlists, str) =
    case strlists of
    [] => []
    |(x::xs) => 
        case all_except_option(str, x) of
        SOME (y::ys) =>  (y::ys) @ get_substitutions1(xs, str)
        | _ =>  get_substitutions1(xs, str)
        

(* c *)
fun get_substitutions2(strlists, str) =
    let fun help_fun(strlists, acc) =
        case strlists of
        [] => acc
        |(x::xs) => 
            case all_except_option(str, x) of
            SOME (y::ys) =>  help_fun(xs, acc @ (y::ys))
            | _ =>  help_fun(xs, acc)
    in
        help_fun(strlists, [])
    end


(* d *)
fun similar_names(strlists, fullname) =
    let fun help_fun(simnames) =
        case simnames of
        [] => []
        |(l::ls) =>
            case fullname of
           {first=x,middle=y,last=z} => {first=l, middle=y, last=z} :: help_fun(ls)
    in
        case fullname of
            {first=x,middle=y,last=z} => fullname::help_fun(get_substitutions2(strlists, x))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* a *)
fun card_color(suit, rank) =
    case suit of
        Diamonds => Red
        |Hearts => Red
        | _ => Black


(* b *)
fun card_value(suit, rank) =
    case rank of
        Num n => n
        |Ace => 11
        | _ => 10


(* с *)
fun remove_card(cs, c, e) = 
    let fun help_fun(cs, acc, isFound) =
        case cs of
            [] => (acc, isFound)
            |(x::xs) => if x = c then (acc @ xs, true) else help_fun(xs, x::acc, isFound)
    in
     case help_fun(cs, [], false) of
            (_, false) => raise e
            | ([], true) => []
            | (x::xs, true) => x::xs
    end


(* d *)
fun all_same_color [] = true
    |all_same_color (x::[]) = true
    |all_same_color (x::y::[]) = (card_color(x) = card_color(y))
    |all_same_color (x::y::xs) = ((card_color(x) = card_color(y)) andalso all_same_color(xs))

(* e *)
fun sum_cards(cs) =
    let fun help_fun(cs, sum) =
    case cs of
        [] => sum
        |(x::xs) => help_fun(xs, sum + card_value(x))
    in
        help_fun(cs, 0)
    end


(* f *)
fun score(cs, goal) =
    let 
        val sum = sum_cards(cs) 
        val previous_sum = if sum > goal then 3*(sum - goal) else goal - sum
    in
        if (all_same_color(cs)) then
            previous_sum div 2
        else
            previous_sum
    end

(* g *)
fun officiate(cs, ms, goal) = 
    let 
    fun current_state(cs, ms, players_cards) =
        case ms of
        [] => score(players_cards, goal)
        |move::moves =>
            case move of
            Discard c => current_state(cs, moves, remove_card(players_cards, c, IllegalMove))
            |Draw =>
                case cs of
                [] => score(players_cards, goal)
                |x::xs => 
                    if sum_cards(x::players_cards) < goal then
                        current_state(xs, moves, x::players_cards)
                    else
                        score(x::players_cards, goal)
    in
    current_state(cs, ms, []) 
    end