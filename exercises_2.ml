let int_add = 4 + 3;;

let add2 = (+) 4 3;;

let string_concat = "CS" ^ string_of_int 3110;;

let int_mult = 42 * 10;;

let float_div = 3.13 /. 2.0;;

let exp x = sqrt x |> sqrt ;;

let sqrt x = x ** 2.;;

let rand = Random.int 3;;

let _ = if 2 > 1 then 42 else 7;;

let inc x = x * 2;;

let cube (x : float ) : float = x ** 3.;;

let sign x = if x > 0 then 1 else 
    (if x = 0 then 0 else -1);;

let area rad = 3.14 *. rad *. rad;;

let roms x y = sqrt( float_of_int ((x*x + y*y)/2) );;

let date_fun d m = 
    if (m="Jan" || m="Mar" || m="May" || m="July"
        || m="Sep" || m="Dec") && d <= 31 then true
    else (if (m="Feb") && d <= 28 then true else 
        (if (m="Apr" || m="Jun" || m="Aug"|| m="Nov") && d <= 30
             then true else false)) 
;;

let rec fib n = 
    if n <=2 then 1 else (fib(n-1) + fib(n-2))
;;

let fib_fast n = 
  let rec h n pp p = 
    if n = 1 then pp+p else
    h (n-1) p (pp+p)
    in h n 0 1
;;

let divide numerator denominator = numerator /. denominator;;

let (+/.) a b = (a +. b) /. 2.0;; (* 1.0 +/. 2.0 = 1.5 *)

let rec print_int_list list = match list with 
| [] -> () 
| h::t -> print_endline (string_of_int h); 
          print_int_list t  
;;

let print_int_list' lst = 
  List.iter (fun x -> print_endline (string_of_int x)) lst