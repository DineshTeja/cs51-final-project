open Expr ;;
open Evaluation ;; 
open CS51Utils ;; 
open Absbook ;;
open Miniml ;;

let freevars_test () = 
     let empty_set = vars_of_list [] in 
     let set_a = vars_of_list ["a"] in 
     let set_a_b = vars_of_list ["a"; "b"] in
     let set_a_b_c = vars_of_list ["a"; "b"; "c"] in
     
     unit_test (same_vars empty_set (free_vars (Num 42)))
                "free_vars num constant";
 
     unit_test (same_vars empty_set (free_vars (Bool false)))
                "free_vars boolean constant"; 
 
     unit_test (same_vars empty_set (free_vars (Raise)))
                "free_vars exception raise"; 
 
     unit_test (same_vars empty_set (free_vars (Unassigned)))
                "free_vars unassigned variable"; 
 
     unit_test (same_vars set_a (free_vars (Var "a")))
                "free_vars single variable a"; 
 
     unit_test (same_vars set_a (free_vars (Unop (Negate, Var "a"))))
                "free_vars unary negation of a";
 
     unit_test (same_vars set_a (free_vars (Binop (Plus, Var "a", Num 10))))
                "free_vars a plus constant"; 
     unit_test (same_vars set_a (free_vars (Binop (Plus, Var "a", Var "a"))))
                "free_vars a plus a";
     unit_test (same_vars set_a_b (free_vars (Binop (Plus, Var "a", Var "b"))))
                "free_vars a plus b";
     unit_test (same_vars set_a_b (free_vars (Binop (Plus, Var "b", (Binop (Plus, Var "a", Var "a"))))))
                "free_vars b plus a plus a";
 
     unit_test (same_vars set_a_b (free_vars (Conditional ((Var "a"), (Var "a"), (Var "b")))))
                "free_vars conditional a then a else b";
     unit_test (same_vars set_a_b (free_vars (Conditional ((Binop (Equals, (Var "a"), (Num 10))), (Var "a"), (Var "b")))))
                "free_vars conditional a equals 10 then a else b";
     
     unit_test (same_vars empty_set (free_vars (Fun ("a", (Binop (Times, Var "a", Num 42))))))
                "free_vars function a times constant";
     unit_test (same_vars set_a (free_vars (Fun ("b", (Binop (Times, Var "b", Var "a"))))))
                "free_vars function b times a";
     
     unit_test (same_vars empty_set (free_vars (Let (("a"), (Num 10), (Binop (LessThan, Var "a", Num 20))))))
                "free_vars let a equals 10 in a less than 20";
     unit_test (same_vars set_a (free_vars (Let (("b"), (Num 10), (Binop (LessThan, Var "a", Num 20))))))
                "free_vars let b equals 10 in a less than 20";
     unit_test (same_vars (vars_of_list ["g"]) (free_vars (Let
                                                          ("g", 
                                                           Fun ("a", Conditional (Binop(Equals, 
                                                               Var ("a"), Num(0)), Num(1),
                                                               Binop(Times, Var ("a"), App(Var("g"), 
                                                               Binop(Minus, Var ("a"), Num(1)))))), 
                                                           App(Var("g"), Num(5))))))
                "free_vars let g = function a if a equals 0 then 1 else a times g(a minus 1) in g 5";
 
     unit_test (same_vars empty_set (free_vars (Letrec (("a"), (Num 10), (Binop (LessThan, Var "a", Num 20))))))
                "free_vars letrec a equals 10 in a less than 20";
     unit_test (same_vars set_a (free_vars (Letrec (("b"), (Num 10), (Binop (LessThan, Var "a", Num 20))))))
                "free_vars letrec b equals 10 in a less than 20";
     unit_test (same_vars empty_set (free_vars (Letrec 
                                             ("g", 
                                              Fun ("a", Conditional (Binop(Equals, 
                                                 Var ("a"), Num(0)), Num(1),
                                                 Binop(Times, Var ("a"), App(Var("g"), 
                                                 Binop(Minus, Var ("a"), Num(1)))))), 
                                                 App(Var("g"), Num(5))))))
                   "free_vars let rec g = fun a -> if a = 0 then 1 else a * g (a - 1) in g 5";
    
     unit_test (same_vars empty_set (free_vars (App (Fun ("a", Binop (Plus, Var "a", Num 10)), Num 15)))) 
               "free_vars app (fun a -> a + 10) 15";

     unit_test (same_vars set_a_b (free_vars (App (Fun ("b", Binop (Plus, Var "a", Num 10)), Var "b"))))
               "free_vars app (fun b -> a + 10) b";

     unit_test (same_vars empty_set (free_vars (List [])))
               "free_vars empty list";
     unit_test (same_vars set_a (free_vars (List [Var "a"])))
               "free_vars list with single variable a";
     unit_test (same_vars set_a_b (free_vars (List [Var "a"; Var "b"; Num 10])))
               "free_vars list with variables a and b";

     unit_test (same_vars set_a (free_vars (ListCons (Var "a", List []))))
               "free_vars list cons with a and empty list";
     unit_test (same_vars set_a_b (free_vars (ListCons (Var "a", List [Var "b"]))))
               "free_vars list cons with a and list containing b";
     unit_test (same_vars set_a_b_c (free_vars (ListCons (Var "a", ListCons (Var "b", Var "c")))))
               "free_vars list cons with a, b cons c";

     unit_test (same_vars empty_set (free_vars (ListAppend (List [], List []))))
               "free_vars list append of two empty lists";
     unit_test (same_vars set_a (free_vars (ListAppend (List [Var "a"], List []))))
               "free_vars list append of list with a and empty list";
     unit_test (same_vars set_a_b (free_vars (ListAppend (List [Var "a"], List [Var "b"]))))
               "free_vars list append of list with a and list with b";
     unit_test (same_vars set_a_b_c (free_vars (ListAppend (List [Var "a"; Var "b"], List [Var "c"]))))
               "free_vars list append of list with a, b and list with c";
;;

let subst_test () =
     unit_test (exp_to_abstract_string (subst "x" (Var "z") (Letrec("x", Var "x", Var "y"))) = "Letrec(x, Var(x), Var(y))") "subst letrec test";
     unit_test ((subst "z" (Num 42) (Num 42)) = (Num 42))  
                    "subst 42 with 42"; 
     
     unit_test ((subst "z" (Num 42) (Bool false)) = (Bool false))  
                    "subst false with 42";
     
     unit_test ((subst "z" (Num 42) Raise) = Raise)  
                    "subst raise with 42";
     
     unit_test ((subst "z" (Num 42) Unassigned) = Unassigned)  
                    "subst unassigned with 42";
     
     unit_test ((subst "z" (Num 42) (Var "z")) = (Num 42))  
                    "subst free z with 42 in z";
     unit_test ((subst "y" (Num 42) (Var "z")) = (Var "z"))  
                    "subst free y with 42 in z";
     
     unit_test ((subst "z" (Num 42) (Unop (Negate, Var "z"))) = (Unop (Negate, Num 42)))
                    "subst -z with 42";
     
     unit_test ((subst "z" 
                         (Num 42) 
                         (Binop (Equals, Var "z", Num 42))) 
                         = Binop (Equals, Num 42, Num 42))
                    "subst z = 42 with 42";
     unit_test ((subst "z" (
                         Num 42) 
                         (Binop 
                         (Equals, 
                              Var "z", 
                              Binop (Plus, Var "z", Num 1)))) 
                         = Binop (Equals, Num 42, Binop (Plus, Num 42, Num 1)))
                    "subst z = z + 1 with 42";
     
     unit_test ((subst "z" (Num 42) (Conditional (Var "z", Num 8, Var "z"))) 
                         = (Conditional (Num 42, Num 8, Num 42)))
                    "subst if z then 8 else z with 42";
     
     unit_test ((subst "z" (Num 42) (Fun ("z", Binop (Plus, Var "z", Num 2)))) 
                         = (Fun ("z", Binop (Plus, Var "z", Num 2))))
                    "subst fun z -> z + 2 with z = 42";
     unit_test ((subst "z" (Num 42) (Fun ("y", Binop (Plus, Var "z", Num 2)))) 
                         = (Fun ("y", Binop (Plus, Num 42, Num 2))))
                    "subst fun y -> z + 2 with z = 42";
     unit_test ((subst "z" 
                         (Binop (Plus, Var "y", Num 1)) 
                         (Fun ("y", Binop (Plus, Var "z", Num 2)))) 
                         = (Fun ("var0", Binop (Plus, Binop (Plus, Var "y", Num 1), Num 2))))
                    "subst fun y -> z + 2 with z = y + 1";
     
     unit_test (subst "z" 
                         (Num 42)
                         (Let ("z", (Binop (Plus, Var "z", Num 2)), Var "z")) 
                         = ((Let ("z", (Binop (Plus, Num 42, Num 2)), Var "z"))))
                    "subst let z = z + 2 in z with z = 42";
     unit_test (subst "z" 
                         (Num 42)
                         (Let ("y", (Binop (Plus, Var "z", Num 2)), Var "z")) 
                         = ((Let ("y", (Binop (Plus, Num 42, Num 2)), Num 42))))
                    "subst let y = z + 2 in z with z = 42";

     unit_test (subst "z" 
                         (Binop (Plus, Var "y", Num 1))
                         (Let ("y", (Binop (Plus, Var "z", Num 2)), Var "z")) 
                         = ((Let ("var1", 
                              (Binop (Plus, (Binop (Plus, Var "y", Num 1)), Num 2)), 
                              (Binop (Plus, Var "y", Num 1))))))
                              "subst let y = z + 2 in z with z = y + 1";


     unit_test (subst "x" (Num 5) (Letrec ("x", Fun ("y", Binop (Plus, Var "x", Var "y")), App (Var "x", Num 10))) = Letrec ("x", Fun ("y", Binop (Plus, Var "x", Var "y")), App (Var "x", Num 10)))
               "subst letrec with shadowing";
     unit_test (subst "y" (Num 5) (Letrec ("x", Fun ("y", Binop (Plus, Var "x", Var "y")), App (Var "x", Var "y"))) = Letrec ("x", Fun ("y", Binop (Plus, Var "x", Var "y")), App (Var "x", Num 5)))
               "subst letrec without shadowing";

     unit_test (exp_to_abstract_string (subst "x" (Var "z") (Letrec("x", Var "x", Var "y"))) = "Letrec(x, Var(x), Var(y))") "subst letrec test";
     unit_test (exp_to_abstract_string (subst "y" (Var "z") (Letrec("x", Var "x", Var "y"))) = "Letrec(x, Var(x), Var(z))") "subst letrec test 2";
     unit_test (exp_to_abstract_string (subst "z" (Var "x") (Letrec("x", Var "x", Var "y"))) = "Letrec(x, Var(x), Var(y))") "subst letrec test 3";
              
     unit_test (subst "z" (Binop (Plus, Var "y", Num 1)) 
               (Letrec ("y", (Binop (Plus, Var "z", Num 2)), Var "z")) = 
               (Letrec ("y", (Binop (Plus, (Binop (Plus, Var "y", Num 1)), Num 2)), 
               (Binop (Plus, Var "y", Num 1)))))
               "subst let rec y = z + 2 in z with z = y + 1  --redo";

     unit_test (subst "z" (Num 42) (Letrec ("y", (Binop (Plus, Var "z", Num 2)), 
          Var "z")) = ((Letrec ("y", (Binop (Plus, Num 42, Num 2)), Num 42)))) 
          "subst let rec y = z + 2 in z with z = 42";

     unit_test (subst "z" 
                         (Binop (Plus, Var "y", Num 1))
                         (Letrec ("y", (Binop (Plus, Var "z", Num 2)), Var "z")) 
                    = ((Letrec ("var2", 
                                   (Binop (Plus, (Binop (Plus, Var "y", Num 1)), Num 2)), 
                                   (Binop (Plus, Var "y", Num 1))))))
               "subst let rec y = z + 2 in z with z = y + 1";

     unit_test (subst "z" (Num 42) (App ((Fun ("z", Binop (Plus, Var "z", Num 2))), Num 1)) 
                    = (App ((Fun ("z", Binop (Plus, Var "z", Num 2))), Num 1)))
               "subst (fun z -> z + 2) 1 with z = 42";
     unit_test (subst "z" (Num 42) (App ((Fun ("y", Binop (Plus, Var "z", Num 2))), Var "z")) 
                    = (App ((Fun ("y", Binop (Plus, Num 42, Num 2))), Num 42)))
               "subst (fun y -> z + 2) z with z = 42";
     
     unit_test (subst "z" (Float 3.14) (Float 3.14) = (Float 3.14))
                    "subst float 3.14 with 3.14";

     unit_test (subst "y" (Float 2.71) (Var "x") = (Var "x"))
          "subst float 2.71 in variable x, no substitution";

     unit_test (subst "x" (Float 3.14) (Unop (Negate, Var "x")) = (Unop (Negate, Float 3.14)))
               "subst float 3.14 in unary negation of x";

     unit_test (subst "x" (Float 3.14) (Binop (Plus, Var "x", Num 2)) = (Binop (Plus, Float 3.14, Num 2)))
               "subst float 3.14 in x + 2";

     unit_test (subst "z" (String "hello") (String "hello") = (String "hello"))
                    "subst string hello with hello";
     unit_test (subst "z" (String "world") (Binop (Concat, String "hello", Var "z")) = (Binop (Concat, String "hello", String "world")))
                    "subst string world in hello ^ z";

     unit_test (subst "z" (Num 42) (List [Var "z"; Num 1; Var "z"]) = (List [Num 42; Num 1; Num 42]))
                    "subst list [z; 1; z] with z = 42";
     unit_test (subst "z" (Num 42) (ListCons (Var "z", List [Num 1; Var "z"])) = (ListCons (Num 42, List [Num 1; Num 42])))
                    "subst list cons z :: [1; z] with z = 42";
     unit_test (subst "z" (Num 42) (ListAppend (List [Var "z"], List [Num 1; Var "z"])) = (ListAppend (List [Num 42], List [Num 1; Num 42])))
                    "subst list append [z] @ [1; z] with z = 42";
;;     

let _ = freevars_test () ;;
let _ = subst_test () ;;

