let rec fact = function 
    0 -> 1 
    |n -> n * fact (n - 1)
;;
try if Array.length(Sys.argv)=2 then print_endline(string_of_int(fact(int_of_string(Sys.argv.(1)))))
       else print_endline "fact: numero de argumentos invalido" with
       _ -> print_endline("fact: argumento invalido")
