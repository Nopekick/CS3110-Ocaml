let one_to_five = [1; 2; 3; 4; 5];;
let one_to_five2 = 1::2::3::4::[5];;
let one_to_five3 = [1] @ [2;3;4] @ [5];;

let rec product lst = 
    match lst with 
    | [] -> 1
    | h::t -> h * product t
;;

let rec concat lst = 
    match lst with 
    | [] -> ""
    | h::t -> h ^ concat t
;;

let bigred lst = 
    match lst with
    | [] -> false
    | h::t -> if h = "bigred" then true else false
;;

let eql lst = 
    match lst with 
    | [] -> false
    | h::t -> if h = List.nth t 0 then true else false
;;

let rec exact lst len = 
    match lst with
    | [] -> if len = 2 || len = 4 then true else false
    | h::t -> exact t (len+1)
;;

let fifth lst = 
    if List.length lst < 5 then 0 else
        List.nth lst 5
;;

let srt lst = 
    List.rev (List.sort Pervasives.compare lst)
;;

let pzle lst = 
    List.nth lst (List.length lst-1)
;;

let any_zeroes lst = 
    List.exists (fun x -> x = 0) lst
;;
  
let rec take n lst = 
    match (n = 0, lst) with 
    | (true, _) -> []
    | (false, []) -> []
    | (_, hd::tl) -> hd::take (n-1) tl
;;
        
let rec drop n lst = 
   match (n > 0, lst) with 
    | (true, []) -> []
    | (true, hd::tl) -> [] @ drop (n-1) tl 
    | (false, []) -> []
    | (_, hd::tl) -> hd::drop (n-1) tl 
;;

let is_unimodal lst = 
    let rec helper prevValue isInc lst = 
        match lst with 
        | [] -> true
        | h::t -> if (h > prevValue && isInc = true)
        || (h < prevValue && isInc = false)
        then helper h isInc t else 
            if (h < prevValue && isInc = true)
            then
                helper h (not isInc) t
            else false  
    in helper 0 true lst;
;;

(* DO *)
let rec powerset lst = ()
;;


type student = { first_name : string ;
 last_name : string ; gpa : float }

let nathan = {first_name="Nathan"; last_name="Kamm"; gpa=2.3};;
let name student = (student.first_name, student.last_name);;
let create_student f l g = {first_name=f; last_name=l; gpa=g};;


type poketype = Normal | Fire | Water;;
type pokemon = {name: string; hp: int; ptype: poketype};;
let charizard = {name="Charizard"; hp=78; ptype=Fire};;
let metapod = {name="Metapod"; hp=50; ptype=Normal};;

let safe_hd lst x = 
    match lst with 
    | [] -> None
    | h::_ -> if h=x then Some x else None
;;

let safe_tl lst = 
    match lst with 
    | [] -> None
    | h::t -> if List.length t > 0 then Some t else None
;;

let pokefun poke_list = 
    let rec helper (lst : pokemon list) (poke : pokemon option) = 
    match (poke, lst) with
    | (_, []) -> poke
    | (None, h::t) -> helper t (Some h)
    | (Some x, h::t) -> if h.hp > x.hp then helper t (Some h) else helper t (Some x)
    in helper poke_list None
;;

type date = int*int*int;;
let is_before (d1 : date) (d2 : date) = 
    let (y1,m1, d1) = d1 in 
    let (y2, m2, d2) = d2 in
    (y2 - y1 > 0) || (y2 - y1 > 0 && m2 - m1 > 0)
    || (y2 - y1 > 0 && m2 - m1 > 0 && d2 - d1 > 0)
;;

let earliest (list : date list) = 
    let rec tracker (lst : date list) (date : date option) =
    match (lst, date) with 
    | ([], _) -> date
    | (h::t, None) -> tracker t (Some h)
    | (h::t, Some date) -> if (is_before h date) then tracker t (Some h) else tracker t (Some date)
    in tracker list None
;;


type suit = Spades | Clubs | Hearts | Diamonds;;
type rank = Ace of int | Two of int | Three of int 
| Four of int | Five of int | Six of int | Seven of int |
Eight of int | Nine of int | Ten of int | Jack of int | 
Queen of int | King of int ;;
type card = {suit: suit ; rank: rank};;
let ace_clubs = {suit=Clubs ; rank= Ace 1};;
let queen_hearts = {suit=Hearts ; rank= Queen 12};;
let two_diamonds = {suit=Diamonds ; rank= Two 2};;
let seven_spades = {suit=Spades ; rank= Seven 7};;

(* 
(Some x)::tl     -> x is None
[Some 3110; None] -> Some 1229 is head
[Some x; _] -> x is None
h1::h2::tl  -> [1], list with only 1 item will break h2
h :: tl  -> only empty list will break, so impossible
 *)

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign = 
  if x = 0 then Zero else (if x > 0 then Pos else Neg)
;;

let quadrant : int*int -> quad option = fun (x,y) ->
  match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | (_, _) -> None
;;

let quadrant_when : int*int -> quad option = function
    | (x, y) when x > 0 && y > 0 -> Some I
    | (x, y) when x < 0 && y > 0 -> Some II
    | (x, y) when x < 0 && y < 0 -> Some III
    | (x, y) when x > 0 && y < 0 -> Some IV
    | (x, y) -> None
;;

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;
let rec depth tree = 
   match tree with 
   | Leaf -> 0
   | Node (_, l, r) -> 1 + depth (max l r)
;;

let rec same_shape a b = 
    match (a,b) with
    | (Leaf, Node (_, l2, r2)) -> false
    | (Node (_, l1, r1), Leaf) -> false
    | (Node (_, l1, r1), Node (_, l2, r2)) -> same_shape l1 l2 && same_shape r1 r2
    | (Leaf, Leaf) -> true
;;

let rec list_max list started max = 
    match (list, started) with
    | ([], _) -> Failure "list_max"
    | (h::t, false) -> list_max t true h
    | (h::t, true) -> if h > max then list_max t true h else list_max t true max
;;

let rec list_max_string list started (max : string) = 
    match (list, started) with
    | ([], _) -> Failure "list_max"
    | (h::t, false) -> list_max_string t true (string_of_int h)
    | (h::t, true) -> if h > int_of_string max then list_max_string t true (string_of_int h) else list_max_string t true max
;;

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;
(* For a BST, mins are passed to left children, maxes passed to right children, parent value is other   *)
let is_bst tree = 
    let rec helper bst (max : int option) (min : int option) = 
        match bst, max, min with 
        | Leaf, _, _ -> true
        | Node (v, l, r), Some ma, None -> if v <= ma then helper l (Some v) None && helper r (Some ma) (Some v) else false
        | Node (v, l, r), None, Some mi -> if v >= mi then helper l (Some v) (Some mi) && helper r None (Some v) else false
        | Node (v, l, r), None, None -> helper l (Some v) None && helper r None (Some v)
        | Node (v, l, r), Some ma, Some mi -> if v >= mi && v <= ma then helper l (Some v) (Some mi) && helper r (Some ma) (Some v) else false
    in helper tree None None 
;;


let poly_sign (x:int) = 
  if x = 0 then `Zero else (if x > 0 then `Pos else `Neg)
;;
 let poly_quadrant  = fun (x,y) ->
  match (poly_sign x, poly_sign y) with
    | (`Pos, `Pos) -> Some `I
    | (`Neg, `Pos) -> Some `II
    | (`Neg, `Neg) -> Some `III
    | (`Pos, `Neg) -> Some `IV
    | (_, _) -> None
;;


