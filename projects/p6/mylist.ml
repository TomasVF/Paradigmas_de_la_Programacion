let rec length = function
	[] -> 0
	| _::t -> 1 + length t
;;


let hd = function
	[] -> raise (Failure "hd")
	| h::_ -> h
;;


let tl = function
	[] -> raise (Failure "hd")
	| _::t -> t
;;


let compare_lengths l1 l2= 
	length l1 - length l2
;;


let rec nth = function
	[] -> raise (Failure "nth")
	| h::t -> (function
				| 0 -> h
				| i -> if i < 0 then raise (Invalid_argument "nth")
								else nth t (i-1))
;;


let rec append l1 l2 = match l1 with
	| [] -> l2
	| h::t -> h::append t l2
;;


let rec find f = function
	| [] -> raise (Not_found )
	| h::t -> if f h then h
			  else find f t
;;


let rec for_all f = function
	| [] -> true
	| h::t -> f h && for_all f t
;;


let rec exists f = function
	| [] -> false
	| h::t -> f h || exists f t
;;


let rec mem x = function
	[] -> false
	| h::t -> x = h || mem x t
;;


let rec filter f = function
	| [] -> []
	| h::t -> if f h then h :: filter f t else filter f t 
;;


let find_all  = filter;;


let rec partition f = function
    | [] -> ([], [])
    | h :: t -> let a, b = partition f t in
                    if f h then (h::a), b
                    else a, (h::b)
;;


let rec split = function
	| [] -> ([], [])
	|(h1, h2)::t -> let a, b = split t in (h1::a, h2::b)
;;


let rec combine = function
	| [] -> (function 
				| [] -> []
				| _ -> raise (Invalid_argument "combine"))
	| h::t -> (function
							| [] -> raise (Invalid_argument "combine")
							| h2::t2 -> (h,h2)::combine t t2)
;;


let rec init i f = match i with
	| 1 -> [f 1]
	| i -> (init (i-1) f) @ [f i]
;;


let rec rev = function
	| [] -> []
	| h::t -> rev t@[h]
;;


let rec rev_append l1 l2 = match l1 with
	| [] -> l2
	| h::t -> rev_append t (h::l2)
;;


let rec concat = function
	| [] -> []
	| h::t -> h@concat t
;;


let flatten = concat;;


let rec map f = function
	[] -> []
	| h::t -> f h :: map f t
;;


let rec rev_map f = function
	| [] -> []
	| h::t -> rev_map f t @ [f h]
;;


let rec map2 f = function
	| [] -> (function 
				| [] -> []
				| _ -> raise (Invalid_argument "map2"))
	| h1::t1 -> (function
					| [] -> raise (Invalid_argument "map2")
					| h2::t2 -> f h1 h2 ::map2 f t1 t2)
;;


let rec fold_left f i = function
	| [] -> raise (Invalid_argument "fold_left")
	| h::[] -> f i h
	| h::t -> f (fold_left f i t) h
;;


let rec fold_right f l i = match l with
	| [] -> raise (Invalid_argument "fold_left")
	| h::[] -> f h i
	| h::t -> f h (fold_right f t i)
;;


