open Expr ;;
open Evaluation ;; 
open CS51Utils ;; 
open Absbook ;;

let value (exp : expr) = Env.Val exp ;;
let empty = Env.empty () ;;
let env_num5 = Env.extend (Env.empty ()) "x" (ref (value (Num 5))) ;;
let env_num10 = Env.extend (Env.empty ()) "x" (ref (value (Num 10)));;
let env_num20 = Env.extend (Env.empty ()) "x" (ref (value (Num 20)));;
let env_str = Env.extend (Env.empty ()) "s" (ref (value (String "hello ")));;

let env_test () =
  unit_test (Env.close (Fun ("x", Binop (Plus, Var "x", Num 10))) env_num10 
              = Closure (Fun ("x", Binop (Plus, Var "x", Num 10)), env_num10))
             "close [{x -> 10}, fun x -> x + 10]";

  unit_test (Env.lookup env_num10 "x" = Env.Val (Num 10))
             "lookup x in {x -> 10}"; 
  unit_test (try 
              Env.lookup empty "x" = Env.Val (Num 10)
             with EvalError "variable not found in environment" -> true)
             "lookup x in { }";
  
  unit_test (Env.extend empty "x" (ref (Env.Val (Num 10))) = env_num10)
             "extend x = 10 in empty";
  unit_test (Env.extend env_num10 "x" (ref (Env.Val (Num 20))) = env_num20)
             "extend x = 20 in {x -> 10}";;

let eval_s_test () = 
  unit_test (eval_s (Num 42) empty = value (Num 42))
             "eval_s 42"; 
  
  unit_test (eval_s (Bool false) empty = value (Bool false))
             "eval_s false"; 
  
  unit_test (try 
              eval_s (Var "y") empty = value (Var "y")
             with EvalError "evaluation of unbound variable" -> true)
             "eval_s y"; 
  
  unit_test (eval_s (Fun ("y", Binop (Minus, Var "y", Num 3))) empty 
                   = value (Fun ("y", Binop (Minus, Var "y", Num 3))))
             "eval_s fun y -> y - 3"; 
  
  unit_test (try 
              eval_s (Raise) empty = value (Raise)
             with EvalError("error in evaluation") -> true)
             "eval_s Raise"; 

  unit_test (eval_s (Unop (Negate, Binop (Minus, Num 10, Num 3))) empty 
                   = value (Num ~-7))
             "eval_s - (10 - 3)"; 

  unit_test (eval_s (Binop (Plus, Num 7, Num 4)) empty 
                   = value (Num 11))
             "eval_s 7 + 4";
  unit_test (eval_s (Binop (Minus, Num 10, Num 3)) empty 
                   = value (Num 7))
             "eval_s 10 - 3";           
  unit_test (eval_s (Binop (Times, Num 4, Num 6)) empty 
                   = value (Num 24))
             "eval_s 4 * 6";
  unit_test (eval_s (Binop (Equals, Num 4, Num 4)) empty 
                   = value (Bool true))
             "eval_s 4 = 4";     
  unit_test (eval_s (Binop (LessThan, Num 3, Num 5)) empty 
                   = value (Bool true))
             "eval_s 3 < 5";
  unit_test (eval_s (Binop (Fplus, Float 3.5, Float 2.5)) empty 
                   = value (Float 6.0))
             "eval_s 3.5 +. 2.5";
  unit_test (eval_s (Binop (Fminus, Float 3.5, Float 1.5)) empty 
                   = value (Float 2.0))
             "eval_s 3.5 -. 1.5";
  unit_test (eval_s (Binop (Ftimes, Float 2.0, Float 3.0)) empty 
                   = value (Float 6.0))
             "eval_s 2.0 *. 3.0";
  unit_test (eval_s (Binop (Fdivide, Float 6.0, Float 3.0)) empty 
                   = value (Float 2.0))
             "eval_s 6.0 /. 3.0";
  unit_test (eval_s (Binop (Concat, String "hello ", String "world")) empty 
                   = value (String "hello world"))
             "eval_s 'hello ' ^ 'world'";
  unit_test (eval_s (List [Num 1; Num 2; Num 3]) empty 
                   = value (List [Num 1; Num 2; Num 3]))
             "eval_s [1; 2; 3]";
  unit_test (eval_s (ListCons (Num 0, List [Num 1; Num 2])) empty 
                   = value (List [Num 0; Num 1; Num 2]))
             "eval_s 0 :: [1; 2]";
  unit_test (eval_s (ListAppend (List [Num 1; Num 2], 
                                 List [Num 3; Num 4])) empty 
                   = value (List [Num 1; Num 2; Num 3; Num 4]))
             "eval_s [1; 2] @ [3; 4]";
  unit_test (eval_s (Let ("z", (Binop (Plus, Num 20, Num 22)), Var "z")) empty
                   = value (Num 42))
             "eval_s let z = 20 + 22 in z";
  unit_test (eval_s (Letrec 
                      ("f", 
                       Fun ("x", Conditional (Binop(Equals, 
                       Var ("x"), Num(0)), Num(1),
                       Binop(Times, Var ("x"), App(Var("f"), 
                       Binop(Minus, Var ("x"), Num(1)))))), 
                    App(Var("f"), Num(5)))) empty
               = value (Num 120))
       "eval_s let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5";

  unit_test (try 
            eval_s (Unassigned) empty = value (Unassigned)
            with EvalError "unassigned cannot be evaluated" -> true)
            "eval_s unassigned"; 

  unit_test (eval_s (App ((Fun ("x", Binop (Times, Var "x", 
                                                   Num 10))), Num 12)) empty
                = value (Num 120))
            "eval_s (fun x -> x * 10) 12";
  unit_test (try 
            eval_s (App (Num 5, Num 12)) empty = value (Num 0)
            with EvalError "function application on non-function" -> true)
            "eval_s 5 12";;

let eval_d_test () =
  unit_test (eval_d (Num 42) empty = value (Num 42))
            "eval_d 42"; 
  
  unit_test (eval_d (Bool true) empty = value (Bool true))
            "eval_d true"; 
  
  unit_test (try 
              eval_d (Var "y") empty = value (Var "y")
              with EvalError "variable not found in environment" -> true)
            "eval_d y"; 

            unit_test (eval_d (Unop (Negate, Binop (Plus, Num 4, Num 6))) empty 
            = value (Num ~-10))
       "eval_d - (4 + 6)"; 

  unit_test (eval_d (Binop (Plus, Num 8, Num 7)) env_num10 
              = value (Num 15))
        "eval_d 8 + 7";

  unit_test (eval_d (Binop (Minus, Num 10, Num 4)) empty 
              = value (Num 6))
        "eval_d 10 - 4";           

  unit_test (eval_d (Binop (Times, Num 6, Num 7)) empty 
              = value (Num 42))
        "eval_d 6 * 7";

  unit_test (eval_d (Binop (Equals, Num 7, Num 7)) env_num10 
              = value (Bool true))
        "eval_d 7 = 7";     

  unit_test (eval_d (Binop (LessThan, Num 7, Num 10)) empty 
              = value (Bool true))
        "eval_d 7 < 10";

  unit_test (eval_d (Binop (Fplus, Float 3.0, Float 4.5)) empty 
              = value (Float 7.5))
        "eval_d 3.0 +. 4.5";

  unit_test (eval_d (Binop (Fminus, Float 10.0, Float 4.5)) empty 
              = value (Float 5.5))
        "eval_d 10.0 -. 4.5";

  unit_test (eval_d (Binop (Ftimes, Float 4.0, Float 3.0)) empty 
              = value (Float 12.0))
        "eval_d 4.0 *. 3.0";
  
  unit_test (eval_d (Binop (Fdivide, Float 6.0, Float 3.0)) empty 
              = value (Float 2.0))
        "eval_d 6.0 /. 3.0";

  unit_test (eval_d (Binop (LessThan, Float 3.5, Float 5.0)) empty 
              = value (Bool true))
        "eval_d 3.5 < 5.0";

  unit_test (eval_d (Binop (Equals, Float 5.0, Float 5.0)) empty 
              = value (Bool true))
        "eval_d 5.0 = 5.0";

  unit_test (eval_d (Binop (Power, Float 3.0, Float 2.0)) empty 
              = value (Float 9.0))
        "eval_d 3.0 ** 2.0";

  unit_test (eval_d (Conditional ((Binop (Equals, Num 5, Num 5)), 
                                         (Num 1), (Num 2))) empty
              = value (Num 1))
        "eval_d if 5 = 5 then 1 else 2";

  unit_test (eval_d (Conditional ((Binop (Equals, Num 4, Num 5)), 
                                         (Num 1), (Num 2))) env_num10
              = value (Num 2))
        "eval_d if 4 = 5 then 1 else 2";

  unit_test (eval_d (Let ("z", (Binop (Plus, Num 3, Num 4)), Var "z")) empty
              = value (Num 7))
          "eval_d let z = 3 + 4 in z";

  unit_test (eval_d (Let ("z", (Binop (Plus, Num 3, Num 4)), Var "z")) env_num10
              = value (Num 7))
          "eval_d let z = 3 + 4 in z";

  unit_test (eval_d (Let ("z", 
                      Num 1, 
                      Let ("f", 
                          Fun ("y", 
                                Binop (Plus, Var "z", 
                                Var "y")), 
                          Let ("z", 
                                Num 2, 
                                App (Var "f", Num 3)))))
                empty 
          = value (Num 5))
        "eval_d let z = 1 in let f = fun y -> z + y in let z = 2 in f 3";

  unit_test (eval_d (Var "x") env_num5 = value (Num 5))
            "eval_d x in x -> 5"; 

  unit_test (eval_d (String "world") empty = value (String "world"))
            "eval_d 'world'";

  unit_test (eval_d (Binop (Concat, Var "s", String "world")) env_str 
            = value (String "hello world"))
            "eval_d 'hello ' ^ 'world' in s -> 'hello '";

  unit_test (eval_d (List [Num 1; Num 2; Num 3]) empty 
            = value (List [Num 1; Num 2; Num 3]))
            "eval_d [1; 2; 3]";

  unit_test (eval_d (ListCons (Num 0, List [Num 1; Num 2])) empty 
            = value (List [Num 0; Num 1; Num 2]))
            "eval_d 0 :: [1; 2]";

  unit_test (eval_d (ListAppend (List [Num 1; Num 2], 
                                 List [Num 3; Num 4])) empty 
            = value (List [Num 1; Num 2; Num 3; Num 4]))
            "eval_d [1; 2] @ [3; 4]";

  unit_test (eval_d (Let ("z", (Binop (Plus, Num 20, Num 22)), Var "z")) empty
            = value (Num 42))
            "eval_d let z = 20 + 22 in z";

  unit_test (eval_d (Letrec 
                      ("f", 
                        Fun ("x", Conditional (Binop(Equals, 
                          Var ("x"), Num(0)), Num(1),
                          Binop(Times, Var ("x"), App(Var("f"), 
                          Binop(Minus, Var ("x"), Num(1)))))), 
                        App(Var("f"), Num(5)))) empty 
            = value (Num 120))
        "eval_d let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5";

  unit_test (try 
              eval_d (Unassigned) empty = value (Unassigned)
              with EvalError "unassigned cannot be evaluated" -> true)
            "eval_d unassigned"; 

  unit_test (eval_d (App ((Fun ("x", Binop (Times, Var "x", 
                                                  Num 10))), Num 12)) empty
            = value (Num 120))
            "eval_d (fun x -> x * 10) 12";

  unit_test (try 
              eval_d (App (Num 5, Num 12)) empty = value (Num 0)
              with EvalError "function application on non-function" -> true)
            "eval_d 5 12";;

let eval_l_test () =
  unit_test (eval_l (Num 42) empty = value (Num 42))
            "eval_l 42"; 
  
  unit_test (eval_l (Bool true) empty = value (Bool true))
            "eval_l true"; 
  
  unit_test (try 
              eval_l (Var "y") empty = value (Var "y")
              with EvalError "variable not found in environment" -> true)
            "eval_l y"; 

  unit_test (eval_l (Var "x") env_num10 = value (Num 10))
            "eval_l x in x -> 10"; 



            unit_test (eval_l (Unop (Negate, Binop (Plus, Num 3, Num 4))) empty 
            = value (Num ~-7))
       "eval_l - (3 + 4)"; 

  unit_test (eval_l (Binop (Plus, Num 7, Num 1)) env_num10 
              = value (Num 8))
        "eval_l 7 + 1";
  unit_test (eval_l (Binop (Minus, Num 4, Num 6)) empty 
              = value (Num ~-2))
        "eval_l 4 - 6";           
  unit_test (eval_l (Binop (Times, Num 3, Num 5)) empty 
              = value (Num 15))
        "eval_l 3 * 5";
  unit_test (eval_l (Binop (Equals, Num 7, Num 3)) env_num10 
              = value (Bool false))
        "eval_l 7 = 3";     
  unit_test (eval_l (Binop (LessThan, Num 3, Num 5)) empty 
              = value (Bool true))
        "eval_l 3 < 5";
  unit_test (eval_l (Binop (Fplus, Float 3.0, Float 4.5)) empty 
              = value (Float 7.5))
        "eval_l 3.0 +. 4.5";
  unit_test (eval_l (Binop (Fminus, Float 6.0, Float 3.5)) empty 
              = value (Float 2.5))
        "eval_l 6.0 -. 3.5";
  unit_test (eval_l (Binop (Ftimes, Float 5.0, Float 2.5)) empty 
              = value (Float 12.5))
        "eval_l 5.0 *. 2.5";
  unit_test (eval_d (Binop (Fdivide, Float 6.0, Float 3.0)) empty 
        = value (Float 2.0))
        "eval_d 6.0 /. 3.0";
  unit_test (eval_l (Binop (Equals, Float 5.0, Float 5.0)) empty 
              = value (Bool true))
        "eval_l 5.0 = 5.0";
  unit_test (eval_l (Binop (LessThan, Float 2.5, Float 5.0)) empty 
              = value (Bool true))
        "eval_l 2.5 < 5.0"; 
  unit_test (eval_l (Binop (Power, Float 2.0, Float 3.0)) empty 
              = value (Float 8.0))
        "eval_l 2.0 ** 3.0";

  unit_test (eval_l (Conditional ((Binop (Equals, Num 3, Num 3)), 
                                         (Num 1), (Num 2))) empty
              = value (Num 1))
        "eval_l if 3 = 3 then 1 else 2";
  unit_test (eval_l (Conditional ((Binop (Equals, Num 4, Num 3)), 
                                         (Num 1), (Num 2))) env_num10
              = value (Num 2))
        "eval_l if 4 = 3 then 1 else 2";

  unit_test (eval_l (Let ("x", (Binop (Plus, Num 6, Num 1)), Var "x")) empty
              = value (Num 7))
          "eval_l let x = 6 + 1 in x";
  unit_test (eval_l (Let ("x", (Binop (Plus, Num 6, Num 1)), Var "x")) env_num10
              = value (Num 7))
          "eval_l let x = 6 + 1 in x with x -> 10";
  unit_test (eval_l (Let ("x", 
                      Num 1, 
                      Let ("f", 
                          Fun ("y", 
                                Binop (Plus, Var "x", 
                                Var "y")), 
                          Let ("x", 
                                Num 2, 
                                App (Var "f", Num 3)))))
                (Env.empty ())
          = value (Num 4))
        "eval_l let x = 1 in let f = fun y -> x + y in let x = 2 in f 3";

  unit_test (eval_l (String "world") empty = value (String "world"))
            "eval_l 'world'";

  unit_test (eval_l (Binop (Concat, Var "s", String "world")) env_str 
            = value (String "hello world"))
            "eval_l 'hello ' ^ 'world' in s -> 'hello '";

  unit_test (eval_l (List [Num 1; Num 2; Num 3]) empty 
            = value (List [Num 1; Num 2; Num 3]))
            "eval_l [1; 2; 3]";

  unit_test (eval_l (ListCons (Num 0, List [Num 1; Num 2])) empty 
            = value (List [Num 0; Num 1; Num 2]))
            "eval_l 0 :: [1; 2]";

  unit_test (eval_l (ListAppend (List [Num 1; Num 2], List [Num 3; Num 4])) empty 
            = value (List [Num 1; Num 2; Num 3; Num 4]))
            "eval_l [1; 2] @ [3; 4]";

  unit_test (eval_l (Let ("z", (Binop (Plus, Num 20, Num 22)), Var "z")) empty
            = value (Num 42))
            "eval_l let z = 20 + 22 in z";

  unit_test (eval_l (Letrec 
                      ("f", 
                        Fun ("x", Conditional (Binop(Equals, 
                          Var ("x"), Num(0)), Num(1),
                          Binop(Times, Var ("x"), App(Var("f"), 
                          Binop(Minus, Var ("x"), Num(1)))))), 
                        App(Var("f"), Num(5)))) empty 
            = value (Num 120))
        "eval_l let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5";

            unit_test (eval_l (Letrec ("x", (Binop (Plus, 
                                                Num 5, Num 2)), Var "x")) empty
            = value (Num 7))
        "eval_l let rec x = 5 + 2 in x";

  unit_test (eval_l (Letrec ("x", (Binop (Plus, Num 5, 
                                                Num 2)), Var "x")) env_num10
              = value (Num 7))
          "eval_l let rec x = 5 + 2 in x with x -> 10";
  unit_test (eval_l (Letrec 
                  ("f", 
                  Fun ("x", Conditional (Binop(Equals, 
                      Var ("x"), Num(0)), Num(1),
                      Binop(Times, Var ("x"), App(Var("f"), 
                      Binop(Minus, Var ("x"), Num(1)))))), 
                  App(Var("f"), Num(5)))) empty 
              = value (Num 120))
        "eval_l let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5";

  unit_test (try 
          eval_l (Unassigned) empty = value (Unassigned)
        with EvalError "unassigned cannot be evaluated" -> true)
        "eval_l unassigned"; 

  unit_test (eval_l (App ((Fun ("x", Binop (Times, Var "x", 
                                                   Num 10))), Num 12)) empty
              = value (Num 120))
          "eval_l (fun x -> x * 10) 12";

  unit_test (eval_l (App ((Fun ("x", Binop (Times, Var "x", 
                                                  Num 10))), Num 12)) env_num10
              = value (Num 120))
          "eval_l (fun x -> x * 10) 12 with x -> 10";

  unit_test (eval_l (App ((Fun ("y", Binop (Times, Var "x", Num 10))), Num 12)) 
                  (Env.extend (Env.empty ()) "x" (ref (value (Num 10))))
              = value (Num 100))
          "eval_l (fun y -> x * 10) 12 with x -> 10";

  unit_test (try 
              eval_l (App (Num 5, Num 12)) empty = value (Num 0)
              with EvalError "function application on non-function" -> true)
            "eval_l 5 12";;
    

let _ = env_test () ;;
let _ = eval_s_test () ;;
let _ = eval_d_test () ;;
let _ = eval_l_test () ;;
