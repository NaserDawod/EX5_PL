(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91));;


let fresh_var used_vars : string = 
		if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
		then raise (OutOfVariablesError)
		else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars);;


let rec fv term = match term with
							Variable x -> StringSet.singleton x
						| Abstraction (s, t) -> StringSet.diff (fv t) (StringSet.singleton s)
						| Application (t1, t2) -> StringSet.union (fv t1) (fv t2);;

let rec variables = function
    | Variable x -> StringSet.singleton x
    | Abstraction (x,y) -> StringSet.union (StringSet.singleton x) (variables y)
    | Application (t1,t2) -> StringSet.union (variables t1) (variables t2);;

let rec substitute x t1 t2 = match t2  with
		| Variable y -> if x = y then t1 else t2
		| Abstraction (y, t) -> if x = y then t2
																else if not (StringSet.mem y (fv t1)) then Abstraction(y, substitute x t1 t)
																else let z = fresh_var (fv t1) in let t_tag = substitute y (Variable(z)) t
																			in (Abstraction(z, substitute x t1 t_tag))
		| Application(t11,t22) -> Application(substitute x t1 t11, substitute x t1 t22);;

let get_val opt = match opt with
		| None -> raise (SyntaxError "Error.\n")
		| Some x -> x;;	

let rec reduce_cbv t = match t with
		| Abstraction (x, t1) -> None
		| Application (t1, Application (t21, t22)) -> let t2 = Application (t21, t22) in 
		  Some (Application (t1, get_val (reduce_cbv t2)))
		| Application (Abstraction (x, t1), s) -> Some (substitute x s t1)
		| Application (Application (t11, t12), s) -> let t' = Application (t11, t12) in 
			Some (Application (get_val (reduce_cbv t'), s))
		| _ -> None;;

let rec reduce_cbn t = match t with
		| Abstraction (x, t1) -> None
		| Application (Abstraction (x, e1), s) -> Some (substitute x s e1)
		| Application (Application (t1, t2), s) -> let t' = Application (t1, t2) in
			Some (Application (get_val (reduce_cbn t'), s))
		| _ -> None;;



