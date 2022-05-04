(* open Expr 
open Evaluation
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

  let a = Num 3 ;;
  let a1 = Var "variable" ;;
  let a2 = App (Var "x", Var "y");;
  let a3 = Unop (Negate, Num 4) ;;
  let a4 = Unop (Negate, Var "lol") ;;
  let a5 = Unop (Negate, Num ~-4) ;;
  let a6 = Binop (Plus, Var "x", Num 5) ;;
  let a7 = Binop (Times, Num 1, Num 2) ;;
  let a8 = Binop (Equals, Num 1, Num 2) ;;
  let a9 = Binop (Minus, Num 1, Num 2) ;;
  let a10 = Binop (LessThan, Num 1, Num 2) ;;
  let a11 = Conditional (Var "v", Num 3, Num 4) ;;
  let a13 = Fun ("f", Binop (Plus, Var "x", Num 3));;
  let a14 = Let ("face", Num 8, Binop (Plus, Var "face", Num 3));;
  let a15 = Letrec ("face", Num 8, Binop (Plus, Var "face", Binop (Plus, Var "face", Num 11) ));;

  let test_free_vars =
    let t = free_vars a in 
    assert (same_vars t SS.empty) ; 
    let t = free_vars a1 ; in 
    assert (same_vars t (SS.singleton "variable"));
    let t = free_vars a2 in 
    assert (t = SS.add "x" (SS.singleton "y"));
    let t = free_vars a3 in 
    assert (t = SS.empty); 
    let t = free_vars a4 in 
    assert (t = SS.singleton "lol" );; 

(* let _ = test_free_vars ;;  *)
let a = Env.empty () ;;
let vref = ref (Evaluation.Env.Val (Num(3))) in
let test_stage_6 = 
  let t = a in 
  assert (t = Env.empty()); 
  let t = Env.extend a "var" vref in 
  assert (t = [("var", vref)] ); *) 
