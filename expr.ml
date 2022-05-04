(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | Abs
  | Sin 
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
  | GreaterThan 
  | Divide
  | Power
  | Concat 
;;

type varid = string ;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int
  | Float of float                           (* integers *)
  | String of string 
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
let rec free_vars (exp : expr) : varidset =
  match exp with 
  | Num _ | Float _ | Bool _| String _ | Raise | Unop _ | Unassigned -> SS.empty 
  | Var v -> SS.singleton v
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> SS.union (free_vars e3) 
    (SS.union (free_vars e2) (free_vars e1))
  | Fun (va, e) -> SS.remove va (free_vars e)
  | Let (va, e1, e2) -> SS.union (SS.remove va (free_vars e2)) (free_vars e1)
  | Letrec (va, e1, e2) -> SS.union (SS.remove va (free_vars e1)) 
    (SS.remove va (free_vars e2))
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)  
;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname =
  let temp = ref 0 in 
  fun () -> let output = "var" ^ string_of_int !temp in 
    temp := !temp + 1; output;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let sub n = subst var_name repl n in
  match exp with 
  | Num _ | Float _ | Bool _ | String _ | Raise | Unassigned | Unop _-> exp
  | Var v -> if v = var_name then repl else Var v
  | Binop (bin, e1, e2) -> Binop (bin, sub e1, sub e2) 
  | Conditional (e1, e2, e3) -> Conditional (sub e1, sub e2, sub e3)
  | Fun (va, e)-> if va = var_name then exp
                  else if not (SS.mem va (free_vars repl)) then Fun (va, sub e) 
                  else let va1 = new_varname () 
                    in sub (Fun (va1, subst va (Var va1) e)) 
  | Let (va, e1, e2) | Letrec (va, e1, e2) -> 
                  if va = var_name then Let(va, sub e1, e2) else
                  if not (SS.mem va (free_vars repl)) then Let (va, sub e1, sub e2)
                  else let va1 = new_varname () 
                    in Let (va1, sub e1, sub (subst va (Var va1) e2))
  | App (e1, e2) -> App (sub e1, sub e2)
;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with 
  | Var varid -> varid       
  | String s -> "'" ^ s ^ "'"
  | Num n -> string_of_int n  
  | Float f -> string_of_float f       
  | Bool b -> string_of_bool b               
  | Unop (un, e) -> (match un, e with
    | Negate, Num n ->  string_of_int ~-n
    | Negate, Float f -> string_of_float (-1. *. f)
    | Abs, Num n -> string_of_int (abs n) 
    | Abs, Float f -> string_of_float (abs_float f) 
    | Sin, Num n -> string_of_float (sin (float_of_int n))
    | Sin, Float f -> string_of_float (sin f)
    | _, _ -> "Error: can only take unop of int or float")
  | Binop (bin, e1, e2) -> let b = match bin with 
                            | Plus -> " + " 
                            | Times ->  " * " 
                            | Minus -> " - " 
                            | Equals -> " = " 
                            | LessThan -> " < "
                            | GreaterThan -> " > " 
                            | Divide -> " / " 
                            | Concat -> " ^ "
                            | Power -> " ** " 
                            in "(" ^ exp_to_concrete_string e1 
                            ^ b ^ exp_to_concrete_string e2 ^ ")"
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_concrete_string e1 
    ^ " then " ^ exp_to_concrete_string e2 ^
                                " else " ^ (exp_to_concrete_string e3)     
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_concrete_string e                  
  | Let (va, e1, e2) -> "let " ^ va ^ " = " ^ exp_to_concrete_string e1 
    ^ " in " ^ exp_to_concrete_string e2          
  | Letrec (va, e1, e2) ->
     "let rec " ^ va ^ " = " 
      ^ exp_to_concrete_string e1 ^ " in " ^ exp_to_concrete_string e2     
  | Raise -> "parse error"                              
  | Unassigned -> "unassigned"                            
  | App (e1, e2) -> exp_to_concrete_string e1 ^ " " ^ exp_to_concrete_string e2
  ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with 
  | Var varid -> "Var(" ^ varid ^ ")"     
  | String s -> "String(" ^ s ^ ")"
  | Num n ->  "Num(" ^ string_of_int n^ ")"                             
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b-> if b then "Bool(true)" else "Bool(false)"                          
  | Unop (un, e) -> (match un, e with
    | Negate, Num n ->  "Unop(Negate(Num(" ^ string_of_int n ^ ")))" 
    | Negate, Float f -> "Unop(Negate(Float(" ^ string_of_float f ^ ")))"
    | Abs, Num n -> "Unop(Abs(Num(" ^ string_of_int n ^  ")))" 
    | Abs, Float f -> "Unop(Abs(Float(" ^ string_of_float f ^ ")))"
    | Sin, Num n -> "Unop(Sin(Num(" ^ string_of_int n ^ ")))"
    | Sin, Float f -> "Unop(Sin(Float(" ^ string_of_float f ^ ")))"
    | _, _ -> "Error: can only take unop of int or float")
  | Binop (bin, e1, e2) -> let b = match bin with 
                            | Plus -> "Plus"
                            | Times -> "Times" 
                            | Minus -> "Minus" 
                            | Equals -> "Equals" 
                            | LessThan -> "LessThan" 
                            | GreaterThan -> " GreaterThan" 
                            | Divide -> "Divide"
                            | Concat -> "Concat"
                            | Power -> "Power" 
                            in "Binop(" ^ b ^ ", " ^ exp_to_abstract_string e1 
                              ^ ", " ^ exp_to_abstract_string e2 ^ ")"  
  | Conditional (e1, e2, e3) -> 
    "Conditional(" ^ exp_to_abstract_string e1 ^ ", " 
      ^ exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (va, e) -> "Fun(" ^ va ^ ", " ^ exp_to_abstract_string e ^ ")"       
  | Let (va, e1, e2) -> "Let(" ^ va ^ ", " ^ exp_to_abstract_string e1 
    ^ ", " ^ exp_to_abstract_string e2 ^ ")"  
  | Letrec (va, e1, e2) -> "Letrec(" ^ va ^ ", " ^ exp_to_abstract_string e1
     ^ ", " ^ exp_to_abstract_string e2 ^ ")" 
  | Raise -> "parse error"                           
  | Unassigned -> "unassigned"                          
  | App (e1, e2) ->  "App(" ^ exp_to_abstract_string e1 ^ ", " 
    ^ exp_to_abstract_string e2 ^ ")"
  ;;

 


