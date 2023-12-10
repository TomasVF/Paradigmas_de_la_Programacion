let rev l = 
	let rec aux a = function
		| [] -> a
		| h::t -> aux (h::a) t
	in aux [] l
;;

let to0from n =
	let rec aux n a l =
		if n < 0 then l
		else aux (n-1) (a+1) (a::l)
	in aux n 0 []
;;
	

let fromto m n =
	let rec aux m n  l=
		if m > n then l
		else aux m (n-1) (n::l)
	in aux m n []
;;


let from1to n =
	let rec aux n  l=
		if n < 1 then l
		else aux (n-1) (n::l)
	in aux n []
;;

let map f l =
	let rec aux f l2 = function
		| [] -> rev l2
		| h::t -> aux f (f h::l2) t
	in aux f [] l
;;


let power x y =
	let rec innerpower x y z =
		if y = 0 then z
		else innerpower x (y-1) (z*x)
	in
		if y >= 0 then innerpower x y 1
		else invalid_arg "power"
;;




let incseg l =
	let rec aux l l2 m = match l with
		| [] -> rev l2
		| h::t -> if m = 0 then aux t (h::l2) h
						   else aux t ((h+m)::l2) (h+m)
	in aux l [] 0
		
;;



let remove x l =
	let rec aux x l2 = function
		[] -> rev l2
		| h::t -> if x = h then aux x l2 t
		 	 	  else aux x (h::l2) t
	in aux x [] l
;;


let divide l =
	let rec aux l tu n = match l with
		| [] -> let a,b = tu in rev a, rev b
		| h::t -> let a,b = tu in
				  if n mod 2 = 0 then aux t (h::a, b) (n+1)
				  else aux t (a, h::b) (n+1)
	in aux l ([],[]) 2
;;


let compress l =
	let rec aux l l2 = match l with
		| h1::h2::t -> if h1 = h2 then aux (h2::t) l2
				   else aux (h2::t) (h1::l2)
		| h::[] -> rev (h::l2)
		| [] -> []
	in aux l []
;;
	
	
