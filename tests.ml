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
    evaluate ~verbose reduce t';;



let s1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
";;


let s2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) tru)
";;

let s3 = "
((\\id1.(t1 id1)) (\\id2.(t1 t2)))
";;

let s4 = "
let id = (\\x.(x)) in
(id (id (\\z.(id z))))
";;

let s5 = "let test = (\\l.(\\m.(\\n. ((l m) n)))) in
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
(((test tru) (\\y.y)) (\\z.z))";;

let s6 = "let id = (\\x.(x)) in
let omega = ((\\x. (x x)) (\\x. (x x))) in
((\\x. (\\y. (x))) (id omega))";;


printf "\nFree VAriabels Tests\n";;
let t1 = "((\\x.(\\y.(a y))) (\\z.z))";;
let t2 = "((\\x.(\\y.(x y))) (\\z.(c v)))";;
let t3 = "(((\\x.(\\y.(x y))) (\\z.(c v))) (\\b.(b f)))";;
printf "\nEvaluating:\n%s\n" t1;
print_string_set (fv (parse t1));;
printf "\nEvaluating:\n%s\n" t2;
print_string_set (fv (parse t2));;
printf "\nEvaluating:\n%s\n" t3;
print_string_set (fv (parse t3));;
printf "\n***********************************************************************\n
Call By Value Tests\n\n";

printf "\nEvaluating:\n%s\nin cbv semantics:\n\n" s1;
ignore (evaluate ~verbose:true reduce_cbv (parse s1));

printf "\n\nEvaluating:\n%s\nin cbv semantics:\n\n" s2;
ignore (evaluate ~verbose:true reduce_cbv (parse s2));

printf "\n\n nEvaluating on:\n%s\nReduce cbv\n\n" s3;
ignore (evaluate ~verbose:true reduce_cbv (parse s3));

printf "\nEvaluating:\n%s\nin cbv semantics:\n\n" s4;
ignore (evaluate ~verbose:true reduce_cbv (parse s4));;

printf "\nEvaluating:\n%s\nin cbv semantics:\n\n" s5;
ignore (evaluate ~verbose:true reduce_cbv (parse s5));;

printf "\nEvaluating:\n%s\nin cbv semantics:\n\n" t2;
ignore (evaluate ~verbose:true reduce_cbv (parse t2));;


printf "\n************************************************************
\nCall By Name Tests\n\n";

printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s1;
ignore (evaluate ~verbose:true reduce_cbn (parse s1));

printf "\n\nEvaluating:\n%s\nin cbn semantics:\n\n" s2;
ignore (evaluate ~verbose:true reduce_cbn (parse s2));

printf "\n\n Testing on:\n%s\nReduce cbn\n\n" s3;
ignore (evaluate ~verbose:true reduce_cbn (parse s3));

printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s4;
ignore (evaluate ~verbose:true reduce_cbn (parse s4));;

printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s5;
ignore (evaluate ~verbose:true reduce_cbn (parse s5));;

printf "\nEvaluating:\n%s\nin cbn semantics:\n\n" s6;
ignore (evaluate ~verbose:true reduce_cbn (parse s6));;

