let square x = x * x;;
let ($) f x = f x;;
(* 
square $ 2 + 2;;
- : int = 16

square 2 + 2;;
- : int = 6 

$ is an infix operator that applies 
a value to a function
*)

let (@@) f g x = x |> g |> f;;
(* 
@@ is an infix operator that essentially performs
f (g x)
 *)

let rec repeat f n x = 
    if n > 0 then repeat f (n-1) (f x) else x
;;

let product_left lst = List.fold_left ( *. ) 1. lst;;
let product_right lst = List.fold_right ( *. ) lst 1.;;

let clip n = if n < 0 then 0 else (if n > 10 then 10 else n);;
let cliplist_map lst = List.map (fun x -> clip x) lst;;
let rec cliplist_rec (lst: int list) (acc : int list) = 
    match lst with
    | [] -> acc
    | h::t -> cliplist_rec t (clip h::acc)
;;

let (--) i j = 
    let rec helper n acc =
      if n < i then acc else helper (n-1) (n :: acc)
    in helper j [] 
;;
let sum_cube_odd n= 
    0--n 
    |> List.filter (fun x -> x mod 2 <> 0)
    |> List.map (fun x -> x * x * x)
    |> List.fold_left (+) 0
;;

let rec exists_rec p = function
    | [] -> false
    | h::t -> if p h then true else exists_rec p t
;;
let exists_fold p lst = List.fold_left (fun a n -> a || p n) false lst;;
let exists_lib p lst = List.filter (fun x -> p x) lst |> List.length > 0;;
let exists_lib2 p lst = List.exists p lst;;

let rec budget1 budget = function
    | [] -> budget
    | h::t -> budget1 (budget-h) t
;;
let rec budget2 budget lst = List.fold_left (fun a x -> a-x) budget lst;;
let rec budget3 budget lst = List.fold_right (fun x a -> a-x) lst budget;;

let uncurried_append (l1, l2) = List.append l1 l2;;
let uncurried_charcompare (ch1, ch2) = Char.compare ch1 ch2;;
let uncurried_max (arg1, arg2) = Pervasives.max arg1 arg2;;

let uncurry f a b= f (a, b);;
let curry f (a, b)= f a b;;

let map_comp f g lst= List.map (fun x -> f (g x)) lst;;

let filter_fun lst = List.filter (fun x -> String.length x > 3) lst;;
let map_fun lst = List.map (fun x -> x +. 1.0) lst;;
let fold_fun lst =  
    let s = List.fold_left (fun a x -> a^","^x) "" lst 
        in String.sub s 1 (String.length s-1)
;;

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;
let rec tree_map f tree = 
    match tree with
    | Leaf -> Leaf
    | Node (v ,l ,r) -> Node (f v, tree_map f l, tree_map f r)
;;

let unique_keys list = let (x, y) = (List.split list) in List.sort_uniq Pervasives.compare x;;

let valid_matrix list = 
    let rec helper list length = 
        match list, length with
        | [], l -> if l = 0 then false else true
        | h::t, 0 -> if List.length h > 0 then helper t (List.length h) else false
        | h::t, x -> if List.length h == x then helper t x else false
    in helper list 0
;;

let add_row_vectors rv1 rv2= List.map2 (fun x y -> x+y) rv1 rv2;;

let matrix_add mat1 mat2 = List.map2 (fun x y -> add_row_vectors x y) mat1 mat2;;



let rec transpose list = 
    match list with 
    | [] | []::_ -> []
    | lst -> List.map List.hd lst :: transpose (List.map List.tl lst)
;;
let rec row_vector_dot_product l m = 
    match m with
    | (h::[]) -> [List.fold_left (+) 0 (List.map2 (fun a1 b1 -> b1*a1 ) h l)]
    | (h::t) -> List.fold_left (+) 0 (List.map2 (fun a1 b1 -> b1*a1 ) h l)::row_vector_dot_product l t ; 
    | [] -> []
;;
let matrix_multiply m1 m2 = List.map (fun x -> row_vector_dot_product x (transpose m2)) m1;;
let rec matrix_multiply2 m1 m2 = 
    match m1 with 
    | [] -> []
    | (h::t) -> row_vector_dot_product h (transpose m2)::matrix_multiply t m2
;;

(* From Brown CS0170 *)
type 'a matrix = 'a list list;;

let rec transpose : 'a matrix -> 'a matrix = function
| [] | [] :: _ -> failwith "A matrix cannot be 0-dimensional."
| (hd1 :: []) :: tl -> (* TODO: base case: list of one-element lists *)
| (hd1 :: tl1) :: tl -> (* TODO: recursive case: list of longer lists *)
;;

(* Matrices for testing *)
let m1 = [
    [1;2;3];
    [4;5;6];
            ];;

let m2 = [
    [7;8];
    [9;10];
    [11;12]  ];;

let m2_transposed = [
    [7; 9; 11]; 
    [8; 10; 12]
            ];;

