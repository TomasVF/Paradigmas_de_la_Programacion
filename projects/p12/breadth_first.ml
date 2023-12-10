
let node = function
    G_tree.Gt (r, _) -> r
;;


let rec breadth_first = function
      G_tree.Gt (x, []) -> [x]
    | G_tree.Gt (x, (G_tree.Gt (y, t2))::t1) -> x :: breadth_first (G_tree.Gt (y, t1@t2))
;;


let breadth_first_t g =
    let rec aux g l = match g with
          G_tree.Gt (x, []) -> List.rev (x::l)
        | G_tree.Gt (x, (G_tree.Gt (y, t2))::t1) -> aux (G_tree.Gt (y, (List.rev_append(List.rev t1) t2))) (x::l)
    in aux g []
;;




let create_tree x =
    G_tree.Gt(x,[])
;;

let l = List.init 3000000 (create_tree);;

let t = G_tree.Gt (1, l);;
