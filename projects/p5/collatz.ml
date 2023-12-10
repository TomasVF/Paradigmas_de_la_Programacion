let f n = 
	if n mod 2 = 0 then n / 2 else 3 * n + 1
	
let rec orbit n = 
    if n = 1 then print_endline(string_of_int n)
    else let _ = print_string(string_of_int n ^ ", ") in orbit (f n)
;;

let rec length n =
	if n = 1 then 0
	else length (f n) + 1
;;

let rec top n =
	if n = 1 then 1
	else max n (top (f n))
;;

let rec length'n'top = function
	1 -> (0, 1)
	| n -> let l, t = length'n'top (f n) in 1 + l, max n t
;;

let rec longest_in m n=
	if n = m then m
	else if length m >= length n then longest_in m (n-1) else longest_in (m+1) n
;;

let rec highest_in m n=
	if n = m then m
	else if top m >= top n then highest_in m (n-1) else highest_in (m+1) n
;;
