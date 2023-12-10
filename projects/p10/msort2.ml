
let rec divide l = match l with
h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
| _ -> l, [];;

let rec merge l = function 
    | ([],y) -> y
    | (x,[]) -> x
    | (h1::t1, h2::t2) ->
    if l h1 h2 
        then h1::(merge l (t1, h2::t2))
        else h2::(merge l (h1::t1, t2))
;;

let rec msort1 l x = match x with
    | ([] | _::[]) -> x
    | _ -> let (l1,l2) = divide x 
    in merge l ((msort1 l l1), (msort1 l l2))
;;

let l2 = [];;



let divide' = function
    | l -> (let rec aux l (x, y) = match l with
                    | [] -> (x, y)
                    | h1::[] -> (h1::x, y)
                    | h1::h2::t -> aux t (h1::x, h2::y)
                 in aux l ([], []))
;;

let merge' f l = 
    let rec aux f l l2 = match l with
        | ([],y) -> List.rev_append l2 y
        | (x,[]) -> List.rev_append l2 x
        | (h1::t1, h2::t2) ->
        if f h1 h2 
            then aux f (t1, h2::t2) (h1::l2)
            else aux f (h1::t1, t2) (h2::l2)
    in aux f l []
;;

let rec msort2 l x = match x with
    | ([] | _::[]) -> x
    | _ -> let (l1,l2) = divide' x 
    in merge' l ((msort2 l l1), (msort2 l l2))
;;

(*msort1 es la implementación más rápida aunque msort2 no es mucho más lenta, qsort2 es bastante más lenta que las msort*)


