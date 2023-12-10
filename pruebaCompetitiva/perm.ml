(*Tomás Villalba Ferreiro*)
let rec fusion l1 l2 = match l1, l2 with
	| [], l | l, [] -> l
	| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: fusion t1 l2 
                                    else h2 :: fusion l1 t2
;;

let rec divide = function
	| h1::h2::t -> let l1,l2 = divide t in
				   h1::l1, h2::l2
	| l -> l, []
;;

(*ordena una lista menos el primer elemento*)
let sort_from2 = function
    | [] -> []
    | h::t -> (let rec m_sort l = match l with
                    | [] | [_] -> l
                    | _ -> let l1, l2 = divide l in
                                fusion (m_sort l1) (m_sort l2)
               in h::m_sort t)
;;


(*Encuentra el último número de la lista el cual es menor que el número que le sigue*)
let find_last_smaller_than_next l =
    let rec aux l a b = match l with
        | [] -> 0
        | h::[] ->  b
        | h1::h2::t -> if h1 < h2 then aux (h2::t) (a+1) (a+1) else aux (h2::t) (a+1) b
    in aux l 0 (-1)
;;


(*Encuentra el número mas pequeño que es mas grande que n en la lista l*)
let find_smallest_bigger_than l n = match l with
    | [] -> 0
    | h::[] -> 1 
    | h1::h2::t1 ->(let rec aux l n a b j = match l with
                        | [] -> b
                        | h::t -> if h > n then if h <= j then aux t n (a+1) (a+1) h else aux t n (a+1) (b) j else aux t n (a+1) (b) j;
                    in aux (h2::t1) n 1 1 h2)
;;

(*Devuelve una lista que contiene los elelementos de la lista entre la posicion a y b, incluidos los elementos en a y en b*)
let get_list_from_range l a b =
    let rec aux l a b n = match l with
        | [] -> []
        | h::t -> if n >= a then if n <= b then h::aux t a b (n+1) else aux [] a b n else aux t a b (n+1)
    in aux l a b 1
;;


(*Devuelve la lista quitando los elementos del priemro a n*)
let get_list_from l a =
    let rec aux l a n = match l with
        | [] -> []
        | h::t -> if n >= a then h::aux t a (n+1) else aux t a (n+1)
    in aux l a 0
;;

(*Encuentra el último elemento de la lista que es menor que el siguiente, después entre los elementos que lo siguen busca el más pequeño que es más grande que el primer número encontrado, se intercambian estos dos elementos (dándole la vuelta a la lista encerrada entre estos dos elementos) y se ordena en sentido descendente la lista desde la posición de el primer número encontrado. De esta forma se consigue la siguiente permutación*)
let next l =
    let x = find_last_smaller_than_next l in
    if x = (-1) then raise (Not_found) else
        let rec aux l l2 a = match l with
            | [] -> l2
            | h::t -> if a < x
                          then aux t (h::l2) (a+1)
                          else let y = find_smallest_bigger_than (h::t) h in aux [] ((List.rev l2) @ sort_from2(List.rev(get_list_from_range (h::t) 1 y) @ get_list_from (h::t) y)) a
        in aux l [] 1
;;



(*Encuentra el último número de la lista el cual es mayor que el número que le sigue*)
let find_last_bigger_than_next l =
    let rec aux l a b = match l with
        | [] -> 0
        | h::[] ->  b
        | h1::h2::t -> if h1 > h2 then aux (h2::t) (a+1) (a+1) else aux (h2::t) (a+1) b
    in aux l 0 (-1)
;;


(*ordena una lista menos el primer elemento en orden ascendente*)
let sort_from22 = function
    | [] -> []
    | h::t -> (let rec m_sort l = match l with
                    | [] | [_] -> l
                    | _ -> let l1, l2 = divide l in
                                fusion (m_sort l1) (m_sort l2)
               in h::(List.rev (m_sort t)))
;;

(*Encuentra el número mas grande que es mas pequeño que n en la lista l*)
let find_bigest_smaller_than l n = match l with
    | [] -> 0
    | h::[] -> 1 
    | h1::h2::t1 ->(let rec aux l n a b j = match l with
                        | [] -> b
                        | h::t -> if h < n then if h >= j then aux t n (a+1) (a+1) h else aux t n (a+1) (b) j else aux t n (a+1) (b) j;
                    in aux (h2::t1) n 1 1 h2)
;;

(*Encuentra el último elemento de la lista que es mayor que el siguiente, después entre los elementos que lo siguen busca el más grande que es más pequeño que el primer número encontrado, se intercambian estos dos elementos (dándole la vuelta a la lista encerrada entre estos dos elementos) y se ordena en sentido ascendente la lista desde la posición de el primer número encontrado. De esta forma se consigue la anterior permutación*)
let prev l =
    let x = find_last_bigger_than_next l in
    if x = (-1) then raise (Not_found) else
        let rec aux l l2 a = match l with
            | [] -> l2
            | h::t -> if a < x
                          then aux t (h::l2) (a+1)
                          else let y = find_bigest_smaller_than (h::t) h in aux [] ((List.rev l2) @ sort_from22(List.rev(get_list_from_range (h::t) 1 y) @ get_list_from (h::t) y)) a
        in aux l [] 1
;;


(*Devuelve una lista que contiene todas las permutaciones que siguen a la permutacion l*)
let allperms_prev l =
    let rec aux l2 = function
          | [] -> l2
          | l -> if find_last_bigger_than_next l = -1 then l2 else aux ((prev l)::l2) (prev l)
    in aux [] l
;;  

(*Devuelve una lista que contiene todas las permutaciones anteriores a la permutacion l*)
let allperms_next l =
    let rec aux l2 = function
          | [] -> l2
          | l -> if find_last_smaller_than_next l = -1 then l2 else aux (l2@[(next l)]) (next l)
    in aux [] l
;;
  
(*Devuelve todas las permutaciones de l en orden lexicográfico*)
let allperms l =
    (allperms_prev l) @ [l] @ (allperms_next l)
;;











