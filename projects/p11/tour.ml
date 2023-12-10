
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

let rec aux m n (xf, yf) lpm path = 
    let (xi,yi) = List.hd path in if xi = xf && yi = yf then List.rev path else
    if List.mem (xi,yi) (List.tl path) then raise Not_found else
    match lpm with
    [] -> raise Not_found
    | h::t -> try aux m n (xf, yf) (possible_movements h m n ) (h::path) with
                Not_found -> aux m n (xf, yf) t path
;;

let tour m n (xi, yi) (xf, yf) =
    if (m < 1) || (n < 1) || (xi > m) || (yi > n) || (xf > m) || (yf > n) then raise Not_found else let lpm = possible_movements (xi, yi) m n in aux m n (xf, yf) lpm ((xi,yi)::[])
;;
