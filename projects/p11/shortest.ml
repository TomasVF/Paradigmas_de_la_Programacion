
let min l =
    let rec aux l a l2 = match l with
        [] -> l2
        |h::t -> if List.length h < a then aux t (List.length h) h else aux t a l2
    in aux l (l |> List.hd |> List.length) (List.hd l)
;;

let valid (x,y) m n = 
    (x <= m) && (y <= n) && (x > 0) && (y > 0)
;;

let possible_movements (a,b) m n = 
    let l = (a-2,b-1) :: (a-2,b+1) ::
    (a-1,b-2) :: (a-1,b+2) ::
    (a+1,b-2) :: (a+1,b+2) ::
    (a+2,b-1) :: (a+2,b+1) :: [] in
    let rec aux l m n l2 = match l with
        [] -> l2
        | h::t -> if valid h m n then aux t m n (h::l2) else aux t m n l2
    in aux l m n []
;;

let rec ec l m n (xf,yf) lpm path f = match l with
    [] -> []
    |h::t -> (f m n (xf,yf) (possible_movements h m n ) (h::path)) @ ec t m n (xf,yf) lpm path f
;; 

let rec aux m n (xf, yf) lpm path = 
    let (xi,yi) = List.hd path in if xi = xf && yi = yf then [(List.rev path)] else
    if List.mem (xi,yi) (List.tl path) then [] else
    ec lpm m n (xf, yf) lpm path aux
;;

let shortest_tour m n (xi, yi) (xf, yf) =
    if (m < 1) || (n < 1) || (xi > m) || (yi > n) || (xf > m) || (yf > n) then [] else let lpm = possible_movements (xi, yi) m n in 
    match aux m n (xf, yf) lpm ((xi,yi)::[]) with
        [] -> raise Not_found
        |l -> min l
;;
