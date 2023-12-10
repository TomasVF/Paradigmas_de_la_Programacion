let remove v l =
    let rec aux v l l2 = match l with
        | [] -> List.rev l2
        | h::t -> if h = v then l2@t  else aux v t (h::l2)
    in aux v l []
;;


(*let remove x = function
    [] -> []
    | h::t -> if x = h then t else h::remove x t
;;*)


let remove_all v l =
    let rec aux v l l2 = match l with
        | [] -> List.rev l2
        | h::t -> if h = v then aux v t l2  else aux v t (h::l2)
    in aux v l []
;;


(*let rec remove_all x = function
    [] -> []
    | h::t -> if x = h then remove_all x t else h::remove_all x t
;;*)


let rec ldif l1 = function
    | [] -> l1
    | h::t -> ldif (remove_all h l1) t
;;

let rec cartesian_product a = function
    | [] -> []
    | h::t -> (a, h)::cartesian_product a t
;;


let rec lprod l1 l2 = match l1 with
    | [] -> []
    | h::t -> (cartesian_product h l2)@lprod t l2
;;

let divide l =
	let rec aux l tu n = match l with
		| [] -> let a,b = tu in List.rev a, List.rev b
		| h::t -> let a,b = tu in
				  if n mod 2 = 0 then aux t (h::a, b) (n+1)
				  else aux t (a, h::b) (n+1)
	in aux l ([],[]) 2
;;
