
let merge f a m c fi =
    let n1 = ref (c - m + 1) and n2 = ref (fi - c) in
    let la = Array.make (!n1) a.(0) and ra = Array.make (!n2) a.(0) in
    for i = 0 to (!n1 - 1) do
        la.(i) <- a.(i)
    done;
    for i = 0 to (!n2 - 1) do
        ra.(i) <- a.(c + 1 + i)
    done;
    let i = ref 0 and j = ref 0 in
    for k = m to fi do
        if !j >= !n2 then
            (a.(k) <- la.(!i) ;
            i := !i + 1)
        else if !i >= !n1 then
            (a.(k) <- ra.(!j) ; 
            j := !j + 1) 
        else
        if f la.(!i) ra.(!j) then
            (a.(k) <- la.(!i) ;
            i := !i + 1)  
        else 
            (a.(k) <- ra.(!j) ; 
            j := !j + 1) 
    done
;;


let rec mergesort f a i fi =
    let c = (i + fi) / 2 in
    if i < fi then
    (mergesort f a i c ;
    mergesort f a (c+1) fi;
    merge f a i c fi)
;;


let asort f a =
    mergesort f a 0 (Array.length a - 1)
;;


