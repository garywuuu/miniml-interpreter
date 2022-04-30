(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
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
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let free_vars (exp : expr) : varidset =
  match exp with 
  | Num _
  | Bool _ 
  | Unop (Negate, _) 
  | Raise 
  | Unassigned -> SS.empty 
  Var ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var varid -> varid       
  | Num n -> string_of_int n         
  | Bool b -> string_of_bool b               
  | Unop (Negate, Num n ) -> string_of_int ~-n  
  | Unop (Negate, _) -> "Error: number must be positive"             
  | Binop (bin, e1, e2) -> let b = match bin with 
                            | Plus -> " + " 
                            | Times ->  " * " 
                            | Minus -> " - " 
                            | Equals -> " = " 
                            | LessThan -> " < " in "(" ^ exp_to_concrete_string e1 ^ b ^ exp_to_concrete_string e2 ^ ")"
  | Conditional (e1, e2, e3) -> "if" ^ exp_to_concrete_string e1 ^ "then" ^ exp_to_concrete_string e2 ^
                                "else" ^ (exp_to_concrete_string e3)     
  | Fun (v, e) -> "let " ^ v ^ " = " ^ exp_to_concrete_string e                  
  | Let (va, e1, e2) -> "let " ^ va ^ " = " ^ exp_to_concrete_string e1 ^ " in " ^ exp_to_concrete_string e2          
  | Letrec (va, e1, e2) -> "let rec " ^ va ^ " = " ^ exp_to_concrete_string e1 ^ "in" ^ exp_to_concrete_string e2     
  | Raise -> "parse error"                              
  | Unassigned -> "unassigned"                            
  | App (e1, e2) -> exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2
  ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var varid -> "Var(" ^ varid ^ ")"     
  | Num n ->  "Num(" ^ string_of_int n^ ")"                              
  | Bool b-> if b then "Bool(true)" else "Bool(false)"                          
  | Unop (Negate, Num i) -> "Unop(Negate(" ^ string_of_int i ^ ")"  
  | Unop (Negate, _) -> "Error: number must be positive"
  | Binop (bin, e1, e2) -> let b = match bin with 
                            | Plus -> " + "
                            | Times -> " * " 
                            | Minus -> " - " 
                            | Equals -> " = " 
                            | LessThan -> " < " in exp_to_abstract_string e1 ^ b ^ exp_to_abstract_string e2  
  | Conditional (e1, e2, e3) -> "Conditional(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (va, e) -> "Fun(" ^ va ^ ", " ^ exp_to_abstract_string e ^ ")"       
  | Let (va, e1, e2) -> "Let(" ^ va ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"  
  | Letrec (va, e1, e2) -> "Letrec(" ^ va ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")" 
  | Raise -> "parse error"                           
  | Unassigned -> "unassigned"                          
  | App (e1, e2) ->  "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  ;;
