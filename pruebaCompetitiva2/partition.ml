(*Tomás Villalba Ferreiro*)

(*calcula el 'peso' de una lista, la suma de todos sus componentes*)
let rec weight = function
    [] -> 0
    | h::t -> h + weight t
;;

(*las siguientes tres funciones ordenan una lista menor a mallor*)
let rec merge x y = match (x,y) with 
    | ([],_) -> y
    | (_,[]) -> x
    | (h1::t1, h2::t2) -> 
    if h1 > h2 
        then h1::(merge t1 y)
        else h2::(merge x t2)
;;

let rec split x y z = match x with
     | [] -> (y,z)
     | x::t -> split t z (x::y)
;;

let rec mergesort x = match x with
    | ([] | _::[]) -> x
    | _ -> let (pri,seg) = split x [] [] 
    in merge (mergesort pri) (mergesort seg)
;;

(*crea una lista de n listas vacías *)
let rec create_list n i l =
    if i = n then l
             else create_list n (i+1) ([]::l)
;;

(*añade a una lista de listas un elemento en la lista con el menor peso, la lista no puede estar vacía*)
let add_smaller n (h::l) i = match h with
    [] -> ((n::[])::l, i)
    |h1::t -> let rec aux n l l2 l3 w i a = match l with
                [] -> ((n::l3)::l2, a)
                | h2::t2 -> let x = weight h2 in if x < w then aux n t2 (l3::l2) h2 x (i+1) (i+1) else aux n t2 (h2::l2) l3 w (i+1) a
              in aux n l [] h (weight h) i 1
;;

(*añade a una lista de listas l el elemento n en la posición b, si b es cero se añade en la lista con menor peso de l, devuelve la lista con el elemento añadido, si se añadió en la lista más pequeña la posición de esta lista en la lista de listas*)
let add2 n l b = match l with
    [] -> ([], -1)
    | h::t -> if b = 0 then add_smaller n l 1
                       else let rec aux n l l2 a = match l with
                                [] -> (l2, -1)
                                | h::t -> if a = b then  aux n [] (List.rev_append ((n::h)::l2) t) a else aux n t (h::l2) (a+1)
                            in aux n l [] 1
;;

(*comprueba si es la lista l es 'válida' respecto a n, si el peso de la lista l es menor a n*)
let rec valid l n = match l with
    []-> true
    |h::t -> weight h <= n && valid t n 
;;

(*prueba si hay alguna forma de añadir a la lista de listas el elemento n llegando así una de sus listas al peso a*)
let try_with n l a = 
    let rec aux n l l2 a found = match l with
        [] -> (l2, found)
        | h::t -> if weight (n::h) = a then aux n [] (List.rev_append ((n::h)::l2) t) a true else aux n t (h::l2) a found
    in aux n l [] a false
;;


(*función clave de partition, donde i es la cantidad de listas en las que se quiere dividir la lista l n es el peso que tiene que tener cada una de estas listas , l2 la lista ya creada que esta compuesta de i listas vacías donde se repartirán los elementos, b es la posición en la que se tiene que añadir el elemento a la posición en la que se añadió el elemento en el paso anterior y num se utiliza para guardar la posición en la que se añadió un elemento utilizando la función add_smaller para no intentarlo de nuevo en la recursividad, en caso de que no se haya añadido utilizando add_smaller es -1*)
let rec aux2 i l n l2 b a num =
    if l = [] then if valid l2 n then Some l2 else None
    else if b > i then None
    else let (li, found) = try_with (List.hd l) l2 n in if found then 
    match aux2 i (List.tl l) n li 0 b (-1) with
        None -> aux2 i l n l2 (a+1) (a+1) (-1)
        | s -> s
    else if b = num then aux2 i l n l2 (b+1) a num
    else let (li, nu) = add2 (List.hd l) l2 b in if not(valid li n) then if b = 0 then None else aux2 i l n l2 (b+1) a nu else
    match aux2 i (List.tl l) n li 0 b (-1) with
        None -> aux2 i l n l2 (a+1) (a+1) nu
        | s -> s
;;

(*función principal partition comprueba que n no sea 1 y que n sea múltiplo exacto del peso de l*)
let partition l = function
    1 -> Some [l]
    | n -> let x = weight l in if x mod n = 0 then let l2 = create_list n 0 [] in aux2 n (mergesort l) (x/n) l2 1 1 (-1) else None
;;


