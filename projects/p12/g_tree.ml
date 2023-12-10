type 'a g_tree = Gt of 'a * 'a g_tree list;;


let subtrees = function
    Gt (_,l) -> l
;;


let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t))
;;


let rec height = function
     Gt (_,[]) -> 1
    |Gt (r,l) -> 1 + List.fold_left (max) 0 (List.map (height) l)
;;


let rec leaves = function
     Gt (r,[]) -> [r]
    |Gt (r, h::[]) -> leaves h
    |Gt (r,h::t) -> leaves h @ leaves (Gt (r,t))
;;


let rec mirror = function
      Gt (r, h::t) -> Gt (r, (subtrees(mirror (Gt(r, t))))@[mirror h])
    | t -> t
;;


let rec preorder = function 
      Gt (r, []) -> [r]
    | Gt (r, h::t) -> let l = preorder (Gt(r, t)) in r::(preorder h) @ (List.tl l)
;;


let rec postorder = function
      Gt (r, []) ->   [r]
    | Gt (r, h::t) -> (postorder h)@(postorder (Gt (r, t)))
;;
