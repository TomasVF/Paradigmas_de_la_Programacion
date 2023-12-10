let length l =
	let rec aux  i = function
		[] -> i
		| _::t -> aux (i+1) t
	in aux 0 l
;;


let hd = function
	[] -> raise (Failure "hd")
	| h::_ -> h
;;


let tl = function
	[] -> raise (Failure "hd")
	| _::t -> t
;;


let rec compare_lengths = function
	| [] -> (function 
				| [] -> 0
				| h::t -> -1)
	| h1::t1 -> (function
				| [] -> 1
				| h2::t2 -> compare_lengths t1 t2)

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


let for_all f l =
	let rec aux f l i = match l with
		| [] -> i
		| h::t -> aux f t (f h && i)
	in aux f l true
;;


let exists f l =
	let rec aux f l i = match l with
		| [] -> i
		| h::t -> aux f t (f h || i)
	in aux f l false
;;


let mem x l =
	let rec aux x l i = match l with
		| [] -> i
		| h::t -> aux x t (x = h || i)
	in aux x l false
;;


let filter f l =
	let rec aux f l l2 = match l with
		| [] -> l2
		| h::t -> if f h then aux f t (l2@[h]) else aux f t l2
	in aux f l []
;;


let find_all  = filter;;


let rev l = 
	let rec aux a = function
		| [] -> a
		| h::t -> aux (h::a) t
	in aux [] l
;;


let partition f l = 
	let rec aux f  l tu = match l with
    	| [] -> let a,b = tu in rev a, rev b
    	| h :: t -> let a, b = tu in
                    if f h then aux f t (h::a,b)
                    else aux f t (a,h::b)
	in aux f l ([], [])
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


let init i f = 
	let rec aux a i f = match i with
		| 0 -> []
		| 1 -> (f 0)::a
		| i -> if i < 0 then raise (Invalid_argument "init") else aux ([f(i-1)]@a) (i-1) f
	in aux [] i f
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


let rev_map f l =
	let rec aux f l l2 = match l with
		| [] -> l2
		| h::t -> aux f t (f h::l2)
	in aux f l []
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
		| h::[] ->f i h
		| h::t -> fold_left f (f i h) t
;;



let rec fold_right f l i = match l with
	| [] -> raise (Invalid_argument "fold_left")
	| h::[] -> f h i
	| h::t -> f h (fold_right f t i)
;;


