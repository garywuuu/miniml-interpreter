open Expr 
open Evaluation

open CS51Utils ;; 
open Absbook ;;

  let a = Num(2) ;;
  let a1 = Var("f") ;;
  let a11 = String("hello") ;;
  let a2 = App (Var "x", Num(2));;
  let a3 = Unop (Negate, Num 3) ;;
  let a31 = Unop(Negate, Float 3.0) ;;
  let a32 = Unop(Abs, Num 3) ;;
  let a33 = Unop(Abs, Float 3.0);;
  let a34 = Unop(Sin, Num 0) ;;
  let a35 = Unop(Sin, Float 0.0);;
  let a4 = Unop (Negate, String("hello")) ;;
  let a5 = (Binop(Plus, Num 3, Num 4)) ;;
  let a6 = (Binop(Concat, String("CS"), String("fifty")));;
  let a7 = (Conditional(Bool(true), Num 3, Num 4));;
  let a8 = (Fun("x", Num(3))) ;;
  let a9 = (Let("f", Num(3), Var("f")));;
  let a10 = Letrec("f", (App(Num (0), Num (2))), App (Num(3), Num(0))) ;;
  let a13 = Raise;;
  let a14 = Unassigned ;;

let test_exp_to_abstract_string = 
  unit_test (exp_to_abstract_string a1 = "Var(f)")
            "Var to abstract";
  unit_test (exp_to_abstract_string a = "Num(2)")
            "Num to abstract";
  unit_test (exp_to_abstract_string (String("hello")) = "String(hello)")
            "String to abstract";
  unit_test (exp_to_abstract_string (Float(3.0)) = "Float(3.)")
            "Float to abstract";
  unit_test (exp_to_abstract_string (Bool(true)) = "Bool(true)")
            "Bool true to abstract";
  unit_test (exp_to_abstract_string (Bool(false)) = "Bool(false)")
            "Bool false to abstract";
  unit_test (exp_to_abstract_string a3 = "Unop(Negate(Num(3)))")
            "Negate num to abstract";
  unit_test (exp_to_abstract_string a31 = "Unop(Negate(Float(3.)))")
            "Negate float to abstract";
  unit_test (exp_to_abstract_string a32 = "Unop(Abs(Num(3)))")
            "Abs num to abstract";
  unit_test (exp_to_abstract_string a33 = "Unop(Abs(Float(3.)))")
            "Abs float to abstract";
  unit_test (exp_to_abstract_string a34 = "Unop(Sin(Num(0)))")
            "Sin num to abstract";
  unit_test (exp_to_abstract_string a35 = "Unop(Sin(Float(0.)))")
            "Sin float to abstract";
  unit_test (exp_to_abstract_string a4 = "Error: can only take unop of int or float")
            "Unop error to abstract";
  unit_test (exp_to_abstract_string a5 = "Binop(Plus, Num(3), Num(4))")
            "Binop plus to abstract";
  unit_test (exp_to_abstract_string a6 = "Binop(Concat, String(CS), String(fifty))")
            "Binop concat to abstract";
  unit_test (exp_to_abstract_string a7 = "Conditional(Bool(true), Num(3), Num(4))")
            "Conditional to abstract";
  unit_test (exp_to_abstract_string a8 = "Fun(x, Num(3))")
            "Fun to abstract";
  unit_test (exp_to_abstract_string (Let("f", Num(3), Var("f"))) 
            = "Let(f, Num(3), Var(f))")
            "Let to abstract";
  unit_test (exp_to_abstract_string a10 
            = "Letrec(f, App(Num(0), Num(2)), App(Num(3), Num(0)))")
            "Complex letrec and conditional function to abstract" ;
  unit_test (exp_to_abstract_string a13 = "parse error")
            "Raise to abstract";
  unit_test (exp_to_abstract_string a14 = "unassigned")
            "Unassigned to abstract";
  unit_test (exp_to_abstract_string a2 = "App(Var(x), Num(2))")
            "App to abstract";;

let test_exp_to_concrete_string = 
  unit_test (exp_to_concrete_string a1 = "f")
            "Var to concrete";
  unit_test (exp_to_concrete_string a = "2")
            "Num to concrete";
  unit_test (exp_to_concrete_string a11 = "'hello'")
            "String to concrete";
  unit_test (exp_to_concrete_string (Float(3.0)) = "3.")
            "Float to concrete";
  unit_test (exp_to_concrete_string (Bool(true)) = "true")
            "Bool true to concrete";
  unit_test (exp_to_concrete_string (Bool(false)) = "false")
            "Bool false to concrete";
  unit_test (exp_to_concrete_string a3 = "-3")
            "Negate num to concrete";
  unit_test (exp_to_concrete_string a31 = "-3.")
            "Negate float to concrete";
  unit_test (exp_to_concrete_string a32 = "3")
            "Abs num to concrete";
  unit_test (exp_to_concrete_string a33 = "3.")
            "Abs float to concrete";
  unit_test (exp_to_concrete_string a34 = "0.")
            "Sin num to concrete";
  unit_test (exp_to_concrete_string a35 = "0.")
            "Sin float to concrete";
  unit_test (exp_to_concrete_string a4
             = "Error: can only take unop of int or float")
            "Unop error to concrete";
  unit_test (exp_to_concrete_string a5 = "(3 + 4)")
            "Binop plus to concrete";
  unit_test (exp_to_concrete_string a6 = "('CS' ^ 'fifty')")
            "Binop concat to concrete";
  unit_test (exp_to_concrete_string a7 = "if true then 3 else 4")
            "Conditional to concrete";
  unit_test (exp_to_concrete_string a8 = "fun x -> 3")
            "Fun to concrete";
  unit_test (exp_to_concrete_string a9 = "let f = 3 in f")
            "Let to concrete";
  unit_test (exp_to_concrete_string a10 = "let rec f = 0 2 in 3 0")
            "Complex letrec and conditional function to concrete" ;
  unit_test (exp_to_concrete_string (Raise) = "parse error")
            "Raise to concrete";
  unit_test (exp_to_concrete_string (Unassigned) = "unassigned")
            "Unassigned to concrete";
  unit_test (exp_to_concrete_string (App((Num 3), (Num 4))) = "3 4")
            "App to concrete";;

 let test_free_vars =
    unit_test ((free_vars a) = (vars_of_list [])) 
            "Free_vars empty"; 
    unit_test (free_vars a1 = vars_of_list ["f"])
            "Free_vars single";
    unit_test (free_vars a2 = vars_of_list ["x"])
            "Free_vars app ";
    unit_test (free_vars a3 = vars_of_list [])
            "Free_vars ";
    unit_test (free_vars (Let("f", Num(3), Var("f"))) = vars_of_list [])
            "Cancel" ;
    unit_test (free_vars (Let("f", Fun("x", Binop(Plus, Var("x"), Var("y"))), 
            App(Var("f"), Var("x")))) = vars_of_list ["x"; "y"])
            "Question 1" ;
    unit_test (free_vars (App(Fun("x", Binop(Plus, Var("x"), Num(1))), Num(3))) 
            = vars_of_list [])
            "Fun app";
    unit_test (free_vars (App(Fun("x", Var("x")), Fun("y", Var("x") ))) 
            = vars_of_list ["x"])
            "Double fun app";;

let test_subst =
    unit_test (subst ("x") (Num 10) (Binop(Plus, Var("x"), Var("x")))
           = Binop(Plus, Num(10), Num(10))) 
          "Binop Sub" ; 
    unit_test ((subst ("x") (Num 42) (Fun ("y", Var "x"))) = Fun ("y", Num(42)))
          "3.2.1 on practice midterm" ;
    unit_test ((subst "x" (Num 42) (App(Fun("y", Var("x")), Num(21)))) 
          = App(Fun("y", Num(42)), Num(21)))
          "3.2.2 on practice midterm" ;
    unit_test ((subst "x" (Num 42) (Let("x", Binop(Plus, Var("x"), Num(1)),
           Binop(Plus, Var("x"), Num(2)))) 
           = Let("x", Binop(Plus, Num(42), Num(1)), Binop(Plus, Var("x"), Num(2)))))
          "3.2.3 on practice midterm";
    unit_test ((subst "y" (Binop(Plus, Var("x"), Num(1))) (Let("x", Num(5), 
          App(Var("f"), Var("y"))))) 
          = Let("var0", Num(5), App(Var("f"), Binop(Plus, Var("x"), Num(1))))) 
          "3.2.4 on practice midterm";
    unit_test ((subst "x" (Num 5) (Fun("x", Binop(Plus, Var("x"),Num(1)))) 
          = Fun("x", Binop(Plus, Var("x"), Num(1)))))
          "not subst" ;
    unit_test ((subst "x" (Num 10) (Conditional(Var("x"), Num(8), Var("x")))) 
          = Conditional (Num(10), Num(8), Num(10)))
          "subst conditional";;

let empty_env = Env.empty () ;;
let vref = ref (Env.Val(Num(2))) ;;
let nenv = Env.extend (Env.empty()) "a" vref ;;

let test_env_module = 
    unit_test (Env.env_to_string empty_env = "") 
          "env empty" ;
    unit_test (Env.env_to_string (Env.extend empty_env "x" vref) = "{x -> 2}")
          "env extend";
    unit_test (Env.lookup nenv "a" = Env.Val (Num(2)))
          "env lookup" ;
    unit_test ((Env.close (Num 2) nenv) = Env.Closure (Num(2), nenv))
          "env close";;

(* these expressions return the same values regardless of the evaluator *)
let test_same_eval eval = 
    unit_test (eval (Num (3)) empty_env = Env.Val (Num(3)))
          "eval num";
    unit_test (eval (Float (3.0)) empty_env = Env.Val (Float(3.)))
          "eval float";
    unit_test (eval (Bool (true)) empty_env = Env.Val (Bool (true)))
          "eval bool";
    unit_test (eval (String ("hello")) empty_env = Env.Val (String("hello")))
          "eval string";
    unit_test (eval (Unop(Negate, Num (3))) empty_env = Env.Val (Num (~-3)))
          "eval unop negate" ;
    unit_test (eval (Unop(Abs, Float (-3.0))) empty_env = Env.Val (Float(3.)))
          "eval unop abs" ; 
    unit_test (eval (Unop(Sin, Num(0))) empty_env = Env.Val (Float(0.)))
          "eval unop sin" ;
    unit_test (eval (Unop(Abs, Num(~-5))) empty_env = Env.Val (Num (5)))
          "eval unop abs" ;
    unit_test (eval (Binop(Plus, Num(3), Num(5))) empty_env = Env.Val (Num(8)))
          "eval binop plus" ;
    unit_test (eval (Binop(Divide, Num(3), Num(2))) empty_env = Env.Val (Float(1.5)))
          "eval binop divide";
    unit_test (eval (Binop(GreaterThan, Num(3), Num(2))) empty_env 
          = Env.Val (Bool(true)))
          "eval binop greaterthan";
    unit_test (eval (Binop(Power, Float(3.0), Float(2.0))) empty_env
           = Env.Val (Float(9.0)))
          "eval binop power";
    unit_test (eval (Binop(Concat, String("gary"), String("wu"))) empty_env 
          = Env.Val (String("garywu")))
          "eval binop concat";
    unit_test (eval (Conditional(Bool(true), Num(5), Num(8))) empty_env 
          = Env.Val (Num(5)))
          "eval conditional" ;
    unit_test (try eval Raise 
           = raise EvalException with EvalException -> true | _ -> false) 
          "eval raise" ;
    unit_test (try eval Unassigned empty_env = raise (EvalError "unassigned") 
          with (EvalError "unassigned") -> true | _ -> false)
          "eval unassigned" ;;

let test_eval_s = 
    unit_test (eval_s (Let("x", Num(10), Let("f", Fun("y", Fun("z", 
          Binop(Times, Var("z"), Binop(Plus, Var("x"), Var("y"))))), 
          Let("y", Num(12), App(App(Var("f"), Num(11)), Num(2)))))) empty_env 
          = Env.Val (Num(42)))
          "eval_s all";;

let test_eval_d = 
    unit_test (eval_d (Let("x", Num(10), Let("f", Fun("y", Fun("z", 
          Binop(Times, Var("z"), Binop(Plus, Var("x"), Var("y"))))), 
          Let("y", Num(12), App(App(Var("f"), Num(11)), Num(2)))))) empty_env 
          = Env.Val (Num(44)))
          "eval_d all";;

let test_eval_l = 
    unit_test (eval_l (Let("x", Num(10), Let("f", Fun("y", Fun("z",
           Binop(Times, Var("z"), Binop(Plus, Var("x"), Var("y"))))), 
           Let("y", Num(12), App(App(Var("f"), Num(11)), Num(2)))))) empty_env
          = Env.Val (Num(42)))
          "eval_l all";;

let _ = test_env_module, test_eval_s, test_eval_d, test_eval_l, test_same_eval,
  test_exp_to_abstract_string, test_exp_to_concrete_string, 
  test_free_vars, test_subst;; 
