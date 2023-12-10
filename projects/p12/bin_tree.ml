type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;	

(* Implemente sum, prod, size, inorder y mirror usando fold_tree *)

let sum3 a b c =
    a + b + c
;;


let sum g =
    fold_tree (sum3) 0 g
;;

let prod3 a b c =
    a *. b *. c
;;

let prod g = 
    fold_tree (prod3) 1. g
;;

let count a b c =
    1 + b + c
;;

let size g = 
    fold_tree (count) 0 g
;;

let fuse a b c =
    b@[a]@c
;;

let inorder g= 
    fold_tree (fuse) [] g
;;

let mir a b c =
    Node (a, c, b)
;;

let mirror g = 
    fold_tree (mir) Empty g
;;

