(* Printing *)
print_endline "hey";
print_string "String";;
print_float 3.2;;
print_char 'A';;


(* Functions *)
let divide2 x y = 
    float_of_int x /. float_of_int y;;
let divide3 (x : float) (y : float) : float =
    x /. y;;
let larger x y = if x > y then x else y;;
let larger1 x = 
    fun y -> if x > y then x else y;;
(fun x y -> if x > y then x else y) 4 5;;
let nathan ?name:(arg1="Nathan") ~age:(arg2:int) arg3  =
    "My name is" ^ arg1 ^ "and I am" 
        ^ string_of_int arg2 ^ "years old";;
(* nathan ~name:"Thomas" ~age:34 true *)
let larger 2 = fun x y -> if x > y then x else y;;
let higher_order f x y = f x y;;
higher_order (fun x y -> x + 10) 2 3;;
let average a b = 
    (a /. b) +. 4.6;;
let sebekSort array = 
    Array.make (Array.length array) "Sebek";;



(* List *)
let list = ["Ocaml" ; "Perl" ; "C"];;
let nums = [1;2;3;4;5;6;7;8;9];;
"Java"::list;; (* prepend item to list *)
let inc_first lst =
  match lst with
  | [] -> []
  | h::t -> (h+1)::t
;;
let add1 list = List.map (fun x -> x + 1) list;;
let odd list = List.filter (fun x -> x mod 2 <> 0) list;;
let even list = List.filter (fun x -> x mod 2 = 0) list;;


(* Array *)
let array = [|1; 2; 3; 4; 5|];;
let array1 = Array.make 9 "Sebek";;

(* Tuple *)
let tuple = ("potato", 45, 23.4, "ace");;
let (a, b, c, d) = tuple;;
let fst_of_tuple (a, b, c) = a;;
let _ = (fun (i, s) -> print_string s) (23, "nathankamm");;


(* If statement *)
let i = 9 and j = 3;;
if j < i || false then
    ( 
        let x = 10 in
            x+2
    )
else    
    begin
        let f = 3 in 
        f+4
    end;;



(* for and while loops *)
for a = 0 to 9 do 
    print_string "value of i is : ";
    print_int a;
    print_string "\n";
    done;;

let x = ref 2 in 
    while !x < 10 do 
        print_string "ocaml learning blah\n";
        x := !x + 1;
        done;;



(* REFS: extract value with !, are mutable *)
let x = ref 5;;
print_int !x;;   
x := 3;
print_int !x;;


(* Pattern Matching *)
let int_to_month (i : int) : string = 
    match i with
      1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr"
    | 5 -> "May"
    | 6 -> "Jun"
    | 7 -> "Jul"
    | 8 -> "Aug"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | _  -> "None"
;;
let p = 12 in 
    match p with
    | 2 -> false
    | 12 -> true
    | _ -> false 
;;


(* Recursion *)
let rec factorial n = 
    if n == 1 then 1
    else n * factorial n-1
;;
let rec sum list = match list with  
    | [] -> 0
    | head::tail -> head + sum tail 
;;  (* head is first element, tail is rest of elements *)
let rec sum2 list = function 
    | [] -> 0
    | h::t -> h + sum t
;;
let rec list_length list = match list with 
    | [] -> 0 
    | _::tail -> 1 + list_length tail
;;
let rec add_list list sum = 
    match list with
    |  []   -> sum
    | h::t -> add_list t sum+h
;;


(* Variant *)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat;;
type point = float * float;;
let d : day = Tue;;

type shape =
  | Point  of point
  | Circle of point * float (* center and radius *)
  | Rect   of point * point (* lower-left and upper-right corners *)
;;                                      
let center = function
  | Point p -> p
  | Circle (p,_) -> p
  | Rect ((x1,y1),(x2,y2)) ->  ((x2 -. x1) /. 2.0, (y2 -. y1) /. 2.0)
;;

type t = Left of int | Right of int;;
let x = Left 1;;
let double_right = function
  | Left i -> i
  | Right i -> 2*i
;;

type string_or_int = String of string | Int of int;;
type string_or_int_list = string_or_int list;;
let rec sum : string_or_int list -> int = function
  | [] -> 0
  | (String s)::t -> int_of_string s + sum t
  | (Int i)::t -> i + sum t
;;
let three = sum [String "1"; Int 2]

(* Record *)
type ptype = TFire | TWater | TEarth | TGhost | TElectric | TDragon;;
type pokemon_record = {name: string; hp: int; ptype: ptype};;
let charmander = {name= "Charmander"; hp= 49; ptype= TFire};;
let c_hp = charmander.hp;;
let higher_hp_c = {charmander with hp=100};;


(* Option *)
let x = Some 43;;
let y = None;;
let extract e = 
    match e with
    | Some i -> string_of_int i
    | None -> ""
;;
(extract (Some 42), extract None);;
let rec list_max = function
  | []   -> None
  | h::t -> begin
      match list_max t with
        | None   -> Some h
        | Some m -> Some (max h m)
      end
;;

(* Assocation List *)
let d = [("Rectangle", 4); ("Triangle", 3); ("Dodecagon", 12)];;
let insert k v list = (k, v)::list;;
let rec lookup key list = 
    match list with
    | [] -> None
    | (k,v)::t -> if key=k then Some v else lookup key t
;;

(* Type Synonyms *)
type ispair = int * string;;
let x = (123, "hi");;
let y:ispair = x;;
let z:(int * string) = y;;

(*  Types  misc. *)
type node = {value:int; next:mylist}
and mylist = Nil | Node of node;;

type intlist = Nil | Cons of int * intlist;;

type 'a mylist = Nil | Cons of 'a * 'a mylist
let lst3 = Cons (3, Nil)  (* similar to [3] *)
let lst_hi = Cons ("hi", Nil)  (* similar to ["hi"] *)

type ('a,'b) pair = {first: 'a; second: 'b};;
let x = {first=2; second="hello"};;

(* Anonymous/Polymorphic Variants *)
let f = function
  | 0 -> `Infinity
  | 1 -> `Finite 1
  | n -> `Finite (-n)


(* Tree as Variant *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t = 
  Node(4,
    Node(2,
      Node(1,Leaf,Leaf),
      Node(3,Leaf,Leaf)
    ),
    Node(5,
      Node(4,Leaf,Leaf),
      Node(7,Leaf,Leaf)
    )
  )
  ;;
let rec size = function
  | Leaf -> 0
  | Node (_,l,r) -> 1 + size l + size r
;;

(* Tree as Record *)
type 'a tree = 
  | Leaf 
  | Node of 'a node
and 'a node = { 
  value: 'a; 
  left:  'a tree; 
  right: 'a tree
}


(* Natural Numbers *)
type nat = Zero | Succ of nat
let zero  = Zero
let one   = Succ zero
let two   = Succ one
let three = Succ two
let four  = Succ three

let rec add (n1:nat) (n2:nat) : nat = 
    
  match n1 with
    | Zero -> n2
    | Succ n_minus_1 -> add n_minus_1 (Succ n2)
;;

let rec 
  even (n:nat) : bool =
    match n with
      | Zero   -> true
      | Succ m -> odd m
and 
  odd (n:nat) : bool =
    match n with
      | Zero   -> false
      | Succ m -> even m
;;


(* Pipeline Operator *)
let square x = x*x;;
let sum = List.fold_left (+) 0;;
let (--) i j = 
    let rec helper n acc =
      if n < i then acc else helper (n-1) (n :: acc)
    in helper j [] 
;;

let sum_sq n =
  0--n |> List.map square |> sum              
;;



(* Modules and Scope *)
module ListStack = struct
  let empty = []
  let is_empty s = (s = [])

  let push x s = x :: s

  let peek = function
    | [] -> failwith "Empty"
    | x::_-> x

  let pop = function 
    | [] -> failwith "Empty"
    | _::xs -> xs
end

module M = struct
  let x = 42
end
let access = M.x;;
open M;;
let access2 = x;;

let s = "BigRed " 
let s' = s |> String.trim |> String.lowercase_ascii (*long way*)
let s' = String.(s |> trim |> lowercase_ascii) 

let f x = 
  let open List in filter ((>) 0) x  ;;


(* Module Signatures *)
module type Stack = sig
  type 'a stack
  val empty    : 'a stack
  val is_empty : 'a stack -> bool
  val push     : 'a -> 'a stack -> 'a stack
  val peek     : 'a stack -> 'a
  val pop      : 'a stack -> 'a stack
end

(* Stack is a signature. ListStack is a structure that
 matches the signature of stack *)
module ListStack2 : Stack = struct
  type 'a stack = 'a list
   let empty = []
  let is_empty s = (s = [])
  let push x s = x :: s
  let peek = function
    | [] -> failwith "Empty"
    | x::_-> x
  let pop = function 
    | [] -> failwith "Empty"
    | _::xs -> xs
end

