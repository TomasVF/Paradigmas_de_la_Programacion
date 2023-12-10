type log_exp =
      Const of bool
    | Var of string
    | Neg of log_exp
    | Disj of log_exp * log_exp
    | Conj of log_exp * log_exp
    | Cond of log_exp * log_exp
    | BiCond of log_exp * log_exp
;;

let rec eval ctx = function
      Const b -> b
    | Var s -> List.assoc s ctx
    | Neg e -> not (eval ctx e)
    | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
    | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
    | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
    | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2)
;;

type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
      C of bool
    | V of string
    | Op of oper * prop
    | BiOp of biOper * prop * prop
;;


let rec prop_of_log_exp = function
      Const b -> C b
    | Var s -> V s
    | Neg e -> Op (Not, (prop_of_log_exp e))
    | Disj (e1, e2) -> BiOp (Or, (prop_of_log_exp e1), (prop_of_log_exp e2))
    | Conj (e1, e2) -> BiOp (And, (prop_of_log_exp e1), (prop_of_log_exp e2))
    | Cond (e1, e2) -> BiOp (If, (prop_of_log_exp e1), (prop_of_log_exp e2))
    | BiCond (e1, e2) -> BiOp (Iff, (prop_of_log_exp e1), (prop_of_log_exp e2))
;;


let rec log_exp_of_prop = function
      C b -> Const b
    | V s -> Var s
    | Op (n, e) -> Neg (log_exp_of_prop e)
    | BiOp (bo, e1, e2) -> (match bo with
                                  Or -> Disj ((log_exp_of_prop e1), (log_exp_of_prop e2))
                                | And -> Conj ((log_exp_of_prop e1), (log_exp_of_prop e2))
                                | If -> Cond ((log_exp_of_prop e1), (log_exp_of_prop e2))
                                | Iff -> BiCond ((log_exp_of_prop e1), (log_exp_of_prop e2))
                           )
;;


let opval = function
    Not -> (not)
;;

let biopval = function
      Or -> (||)
    | And -> (&&)
    | If -> (let aux b a = (not a) || b in (aux))
    | Iff -> (=)
;;


let rec peval ctx = function
      C b -> b
    | V s -> List.assoc s ctx
    | Op (o, e) -> (peval ctx e) |> (opval o)
    | BiOp (bo, e1, e2) -> (peval ctx e1) |> (biopval bo) (peval ctx e2)
;;


let get_var p = 
    let rec aux p l = match p with
          V s -> if List.mem s l then l else s::l
        | C b -> l
        | Op (o,e) -> aux e l
        | BiOp (bo, e1, e2) -> (aux e2 (aux e1 l))
    in aux p []
;;


let rec combinations = function
  | 0 -> [[]]
  | n ->
    let rest = combinations (n - 1) in
    let comb_f = List.map (fun l -> false::l) rest in
    let comb_t = List.map (fun l -> true::l) rest in
    comb_t @ comb_f
;;

let rec is_tau p =
    let lp = get_var p in
    let n = List.length lp in
    let lt = combinations n in
	let rec aux p lp n lt = match lt with
	      [] -> true
	    | h::t -> (peval (List.combine lp h) p) && (aux p lp n t)
	in aux p lp n lt
;;




























