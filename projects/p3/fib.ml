let rec fib n =
	if n <= 1 then n
	else fib (n-1) + fib (n-2)

let x = int_of_string(Sys.argv.(1))

let rec print_fib n = 
	if n <= x then let _ = print_endline(string_of_int(fib n)) in print_fib (n+1)
	else ();;
print_fib 0;;
