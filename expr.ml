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
  | Divide
  | Equals
  | NotEquals
  | LessThan
  | GreaterThan
  | Power
  | Concat
  | ListCons
  | ListAppend
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Float of float                       (* floats *)
  | String of string                     (* strings *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | List of expr list                   (* lists of expressions *)
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
  let open SS in 
  match exp with
  | Num _ | Float _| Bool _ | String _ | Raise | Unassigned -> empty 
  | Var x -> singleton x
  | Unop (_, expr) -> free_vars expr
  | Binop (_, expr1, expr2) -> union (free_vars expr1) (free_vars expr2)
  | Conditional (expr1, expr2, expr3) -> 
      (free_vars expr1) |> union (free_vars expr2) |> union (free_vars expr3)
  | Fun (x, expr) -> remove x (free_vars expr)
  | Let (x, expr1, expr2) ->
      (remove x (free_vars expr2)) |> union (free_vars expr1)
  | Letrec (x, expr1, expr2) -> 
      remove x (union (free_vars expr1) (free_vars expr2))
  | App (expr1, expr2) -> (free_vars expr1) |> union (free_vars expr2) 
  | List exprs -> List.fold_left (fun acc exp -> 
      SS.union acc (free_vars exp)) SS.empty exprs
;;
  
(* new_varname () -- Returns a freshly minted `varid` with prefix
   "var" and a running counter a la `gensym`. Assumes no other
   variable names use the prefix "var". (Otherwise, they might
   accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid = 
  let counter = ref 0 in 
  fun () ->
   let count = !counter in
   counter := !counter + 1; 
   "var" ^ (string_of_int count) ;;

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
	let is_fv_of (exp : expr) (var : varid) = SS.mem var (free_vars exp) in
  match exp with
  | Num _ | Float _ | Bool _ | String _ | Raise | Unassigned -> exp
  | Var x -> if x <> var_name then exp else repl
  | Unop (unop, expr) -> Unop (unop, subst var_name repl expr)
  | Binop (binop, expr1, expr2) -> Binop (binop, 
                                          subst var_name repl expr1,
                                          subst var_name repl expr2)
  | Conditional (expr1, expr2, expr3) -> 
    Conditional (subst var_name repl expr1,
                 subst var_name repl expr2,
                 subst var_name repl expr3)
  | Fun (x, expr) -> 
    if x = var_name then exp 
    else if x <> var_name && x |> is_fv_of repl then
      let new_var = if SS.mem x (free_vars repl) then new_varname () else x in
      Fun (new_var, subst var_name repl (subst x (Var new_var) expr))
    else 
      Fun (x, subst var_name repl expr)
  | Let (x, expr1, expr2) ->
		if x = var_name then Let (x, subst var_name repl expr1, expr2)
		else if x <> var_name && x |> is_fv_of repl then
      let new_var = if SS.mem x (free_vars repl) then new_varname () else x in
			Let (new_var, 
					 subst var_name repl expr1, 
					 subst var_name repl (subst x (Var new_var) expr2))
		else 
			Let (x, subst var_name repl expr1, subst var_name repl expr2)
  | Letrec (x, expr1, expr2) -> 
      if x = var_name then 
          Letrec (x, expr1, expr2)
      else if not (SS.mem x (free_vars repl)) then
          Letrec (x, subst var_name repl expr1, subst var_name repl expr2)
      else
          let new_var = if SS.mem x (free_vars repl) 
                        then new_varname () else x in
          Letrec (new_var, 
                  subst var_name repl (subst x (Var new_var) expr1),
                  subst var_name repl (subst x (Var new_var) expr2))
  | App (expr1, expr2) -> App (subst var_name repl expr1, 
															 subst var_name repl expr2) 
  | List exprs -> List (List.map (subst var_name repl) exprs)
;;

(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> x
  | Num num -> string_of_int num
  | Float num -> string_of_float num
  | String str -> "\"" ^ str ^ "\""
  | Bool bool -> string_of_bool bool
  | Unop (unop, expr) -> 
    (match unop with 
      | Negate -> "-" ^ exp_to_concrete_string expr)
  | Binop (binop, expr1, expr2) -> 
    let operator = match binop with
      | Plus -> (match (expr1, expr2) with
                 | (Float _, _) | (_, Float _) -> " +. "
                 | _ -> " + ")
      | Minus -> (match (expr1, expr2) with
                  | (Float _, _) | (_, Float _) -> " -. "
                  | _ -> " - ")
      | Times -> (match (expr1, expr2) with
                  | (Float _, _) | (_, Float _) -> " *. "
                  | _ -> " * ")
      | Equals -> " = "
      | NotEquals -> " <> "
      | LessThan -> " < "
      | GreaterThan -> " > "
      | Power -> " ** "
      | Concat -> " ^ "
      | ListCons -> " :: "
      | ListAppend -> " @ "
      | _ -> failwith "Unsupported operation for concrete string representation"
    in
    exp_to_concrete_string expr1 ^ operator ^ exp_to_concrete_string expr2
  | Conditional (expr1, expr2, expr3) -> 
    "if " ^ exp_to_concrete_string expr1 ^ " then " ^ 
      exp_to_concrete_string expr2 ^ " else " ^ exp_to_concrete_string expr3 
  | Fun (x, expr) -> "fun " ^ x ^ " -> " ^ exp_to_concrete_string expr    
  | Let (x, expr1, expr2) -> 
    "let " ^ x ^ " = " ^ exp_to_concrete_string expr1 ^ " in " 
      ^ exp_to_concrete_string expr2
  | Letrec (x, expr1, expr2) -> 
    "let rec " ^ x ^ " = " ^ exp_to_concrete_string expr1 ^ " in " 
      ^ exp_to_concrete_string expr2
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (e1, e2) -> "(" ^ exp_to_concrete_string e1 ^ ")" ^ " " ^ "(" ^ 
                          exp_to_concrete_string e2 ^ ")"
  | List exprs -> "[" ^ String.concat "; " 
                      (List.map exp_to_concrete_string exprs) ^ "]"
;;

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num num -> "Num(" ^ string_of_int num ^ ")"
  | Float num -> "Float(" ^ string_of_float num ^ ")"
  | String str -> "String(" ^ str ^ ")"
  | Bool bool -> "Bool(" ^ string_of_bool bool ^ ")"
  | Unop (unop, expr) -> 
    (match unop with 
      | Negate -> "Unop(Negate, " ^ exp_to_abstract_string expr ^ ")")
  | Binop (binop, expr1, expr2) -> 
    (match binop with
      | Plus -> "Binop(Plus, " ^ exp_to_abstract_string expr1 ^ ", " 
                  ^ exp_to_abstract_string expr2 ^ ")"
      | Minus -> "Binop(Minus, " ^ exp_to_abstract_string expr1 ^ ", " 
                  ^ exp_to_abstract_string expr2 ^ ")"
      | Times -> "Binop(Times, " ^ exp_to_abstract_string expr1 ^ ", " 
                  ^ exp_to_abstract_string expr2 ^ ")"
      | Divide -> "Binop(Divide, " ^ exp_to_abstract_string expr1 ^ ", " 
                  ^ exp_to_abstract_string expr2 ^ ")"
      | Equals -> "Binop(Equals, " ^ exp_to_abstract_string expr1 ^ ", " 
                    ^ exp_to_abstract_string expr2 ^ ")"
      | NotEquals -> "Binop(NotEquals, " ^ exp_to_abstract_string expr1 ^ ", " 
                      ^ exp_to_abstract_string expr2 ^ ")"
      | LessThan -> "Binop(LessThan, " ^ exp_to_abstract_string expr1 ^ ", " 
                      ^ exp_to_abstract_string expr2 ^ ")"
      | GreaterThan -> "Binop(GreaterThan, " 
                      ^ exp_to_abstract_string expr1 ^ ", " 
                      ^ exp_to_abstract_string expr2 ^ ")"
      | Power -> "Binop(Power, " ^ exp_to_abstract_string expr1 ^ ", " 
                  ^ exp_to_abstract_string expr2 ^ ")"
      | Concat -> "Binop(Concat, " ^ exp_to_abstract_string expr1 ^ ", " 
                  ^ exp_to_abstract_string expr2 ^ ")"
      | ListCons -> "Binop(ListCons," ^ exp_to_abstract_string expr1 ^ ", " 
                      ^ exp_to_abstract_string expr2 ^ ")"
      | ListAppend -> "Binop(ListAppend," ^ exp_to_abstract_string expr1 ^ ", " 
                      ^ exp_to_abstract_string expr2 ^ ")")
  | Conditional (expr1, expr2, expr3) -> 
    "Conditional(" ^ exp_to_abstract_string expr1 ^ ", " 
      ^ exp_to_abstract_string expr2 ^ ", " ^ exp_to_abstract_string expr3 ^ ")" 
  | Fun (x, expr) -> "Fun(" ^ x ^ ", " ^ exp_to_abstract_string expr ^ ")"    
  | Let (x, expr1, expr2) -> 
    "Let(" ^ x ^ ", " ^ exp_to_abstract_string expr1 ^ ", " 
      ^ exp_to_abstract_string expr2 ^ ")"
  | Letrec (x, expr1, expr2) -> 
    "Letrec(" ^ x ^ ", " ^ exp_to_abstract_string expr1 ^ ", " 
      ^ exp_to_abstract_string expr2 ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (expr1, expr2) -> 
    "App(" ^ exp_to_abstract_string expr1 ^ ", " 
        ^ exp_to_abstract_string expr2 ^ ")"
  | List exprs -> "List(" ^ String.concat ", " 
        (List.map exp_to_abstract_string exprs) ^ ")"
;;
