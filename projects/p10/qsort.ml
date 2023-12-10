let rec qsort1 ord = function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              qsort1 ord before @ h :: qsort1 ord after
;;

(*No es una función con recursividad terminal, por lo tanto con listas muy grandes daría stack overflow*)

let rec qsort2 ord =
    let append' l1 l2 = List.rev_append (List.rev l1) l2 in
    function
        [] -> []
        | h::t -> let after, before = List.partition (ord h) t in
            append' (qsort2 ord before) (h :: qsort2 ord after)
;;

(*qsort2 no tiene ninguna ventaja ya que sigue sin tener recursividad terminal y es más lenta*)

let l1 = [];;

(*qsort2 es más lenta que qsort1 ya que, aunque List.rev_append tiene recusividad terminal y es más efectiva que @ List.rev hace que sea más lenta. Tiene una penalización de, aproximadamente, 15%*)




















