(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)
open Utils
open Parser
open Reducer


let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None -> 
    if verbose then print_string " =/=>\n\n" else ();
    t
  | Some t' -> 
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'


let s1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"


let s2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) tru)
"

let s3 = "
((\\id1.(t1 id1)) (\\id2.(t1 t2)))
"
(* Question 5 *)
let s4 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let not = (\\p.((p fls) tru)) in
(not fls)
"

let s5 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let not = (\\p.((p fls) tru)) in
(not tru)
"

(* End *)

let () =
  printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s1;
  ignore (evaluate ~verbose:true reduce_cbn (parse s1));
  printf "\n\nEvaluating:\n%s\nin cbv semantics:\n\n" s2;
  ignore (evaluate ~verbose:true reduce_cbv (parse s2));

  printf "\n\n Testing on:\n%s\nReduce cbv\n\n" s3;
  ignore (evaluate ~verbose:true reduce_cbv (parse s3));
  printf "\n\n Testing on:\n%s\nReduce cbn\n\n" s3;
  ignore (evaluate ~verbose:true reduce_cbn (parse s3));
  
  (* Continue Question 5 *)
  printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s4;
  ignore (evaluate ~verbose:true reduce_cbn (parse s4));
  printf "\n\nEvaluating:\n%s\nin cbv semantics:\n\n" s4;
  ignore (evaluate ~verbose:true reduce_cbv (parse s4));
  
  printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s5;
  ignore (evaluate ~verbose:true reduce_cbn (parse s5));
  printf "\n\nEvaluating:\n%s\nin cbv semantics:\n\n" s5;
  ignore (evaluate ~verbose:true reduce_cbv (parse s5));
  
  (* End *)

  (*Format Term Conv Tests*)
  
  printf "%s\n" (format_term_conv(parse("(\\t.(\\f.t))" )));
  printf "%s\n" (format_term_conv(parse("(t1 (t2 t3))")));
  printf "%s\n" (format_term_conv(parse("((t1 t2) t3)")));
  printf "%s\n" (format_term_conv(parse(s1)));
  printf "%s\n" (format_term_conv(parse("(\\b.(\\c. ((b (not c)) c)))")));

 (* Noam Cohen tests *)
 print_string ("------------------(Test 1 - Substitution)-------------------\n");; 
print_string "Expression: (\\z.(z z)) substitute with Var(r). Your ouput: ";;
let z = "z";;
let new_val = Variable("r");;
let e = parse "(\\z.(z z))";;
print_string (format_term (substitute z new_val e));;

print_string ("\n-------------------(Test 2 - Call By Name)----------------\n");; 
let e = (Parser.parse "(((\\f.(\\x.(f (x x)))) (\\f.(\\x.(f (x x))))) ((\\y.(y y)) (\\z.z)))");;
print_string ("In Call-By-Name: \n");;
ignore (evaluate ~verbose:true reduce_cbn (e));;

print_string ("-------------------(Test 3 - Call By Value)----------------\n");; 
print_string ("In call by call by value: \n");;
ignore (evaluate ~verbose:true reduce_cbv (e));; 

print_string ("------------------(Test 4 - Substitution)-------------------\n");; 
print_string "substitute: ((\\x.((\\f.(\\x.(f (x x)))) (x x))) ((\\y.(y y)) (\\z.z)))[x->((\\y.y y) (\\z.z))]\n";;
print_string "Your output: ";;
let x = "x";;
let t1 = Parser.parse "((\\y.(y y)) (\\z.z))";;
let t2 = Parser.parse "((\\x.((\\f.(\\x.(f (x x)))) (x x))) ((\\y.(y y)) (\\z.z)))";;
print_string (format_term (substitute x t1 t2));;
print_string "\n";;

print_string ("------------------(Test 5 - Substitution)-------------------\n");; 
print_string "(\\x. ((\\y. (\\x. x)) x))[x->d]\n";;
print_string "Your output: ";;
let x = "x";;
let t1 = Parser.parse "d";;
let t2 = Parser.parse "(\\x. ((\\y. (\\x. x)) x))";;
print_string (format_term (substitute x t1 t2));;
print_string "\n";;

print_string "\nTest:\n";;
print_string ("evaluating by call-by-value: test (or tru fls)\n");;
let e_str = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let test = (\\l.(\\m.(\\n.((l m) n)))) in
let or = (\\b.(\\c.((b tru) c))) in
(test ((((or tru) fls) a) b))
";;
let e = parse e_str;;
ignore (evaluate ~verbose:true reduce_cbv (e));;
print_string ("-------------------------------------------------\n");;
print_string ("evaluating by call-by-name: test (or tru fls) a b\n");;
let e_str = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let test = (\\l.(\\m.(\\n.((l m) n)))) in
let or = (\\b.(\\c.((b tru) c))) in
(test ((((or tru) fls) a) b))
";;
let e = parse e_str;;
ignore (evaluate ~verbose:true reduce_cbn (e));;

(* End of Noam Cohen Tests *)