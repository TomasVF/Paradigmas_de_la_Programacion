let rec power x y = 
	if y = 0 then 1 else x * power x (y-1);;
	


let rec power' x y = 
	if y = 0 then 1 else 
		if y mod 2 = 0 then power x (y/2) * power x (y/2) else x * power x (y/2) * power x (y/2);;



let rec powerf x y = 
	if y = 0 then 1. else 
		if y mod 2 = 0 then powerf x (y/2) *. powerf x (y/2) else x *. powerf x (y/2) *. powerf x (y/2);;


