(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars : string = 
	if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
	then raise (OutOfVariablesError)
	else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)

(* Question 1 *)
let rec fv (e : term) : StringSet.t = 
	match e with
	| Variable x -> StringSet.singleton x
	| Abstraction (x, e) -> StringSet.diff (fv e) (StringSet.singleton x)
	| Application (e1, e2) -> StringSet.union (fv e1) (fv e2)

(* Question 2 *)
let rec substitute (x: string) (t1 : term ) (t2: term) :term = 
	match t2 with
	| Variable y when x = y -> t1
	| Variable y when x != y -> Variable y
	| Abstraction (y, e) when x = y -> Abstraction (y, e)
	| Abstraction (y, e) when x != y && not(StringSet.mem y (fv t1)) -> Abstraction (y, substitute x t1 e)
	| Abstraction (y, e) when x != y && StringSet.mem y (fv t1) -> let z = fresh_var (StringSet.union ( StringSet.union(fv t1) (fv e)) (StringSet.singleton x)) 
																														in Abstraction (z, substitute x t1 (substitute y (Variable z) e))
	| Application (e1, e2) -> Application (substitute x t1 e1, substitute x t1 e2)
	| _ -> raise OutOfVariablesError

	

(* Question 3 *)
let reduce_cbv (e : term) : term option = 
	match e with
	| Application (t1, t2) -> (match t1 with
														| Abstraction (id1, t1') -> (match t2 with
																												| Abstraction (_, _) -> Some (substitute id1 t2 t1') (* only one step so switch id1 in the remaining t1' *)
																												| _ -> None) (* The only rule is abstraction *)
														| _ -> None)
	| _ -> None

	
(* Question 4 *)
let reduce_cbn (e : term) : term option = 
	match e with
	| Application (t1, t2) -> (let t1' = reduce_cbv t1 in (
															match t1' with
															| Some t1'' -> Some (Application (t1'', t2)) (* wait with the calculation of t2 *)
															| _ -> reduce_cbv e ))
	| _ -> None