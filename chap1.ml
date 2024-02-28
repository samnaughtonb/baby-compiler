module Env = Map.Make(String);;

type id = string

type binop =
  | Plus
  | Minus
  | Times
  | Div

type
  exp
    = IdExp of id
    | NumExp of int
    | OpExp of exp * binop * exp
    | EseqExp of stm * exp
  and
  stm
    = CompoundStm of stm * stm
    | AssignStm of id * exp
    | PrintStm of exp list

let rec maxargs stmt =
  match stmt with
  | AssignStm (id1, exp1) ->
      (match exp1 with
      | EseqExp (stmt1, exp2) -> maxargs stmt1
      | _ -> 0)
  | PrintStm exps -> List.length exps
  | CompoundStm (other, stm1) -> max (maxargs other) (maxargs stm1);;

let rec maxargs_exp expr =
  match expr with
  | IdExp _ -> 0
  | NumExp _ -> 0
  | OpExp (expr1, _, expr2) -> max (maxargs_exp expr1) (maxargs_exp expr2)
  | EseqExp (stmt, expr1) -> max (maxargs stmt) (maxargs_exp expr1);;

let interp stmt =
  let rec print_exprs env exprs =
    match exprs with
    | [] -> ()
    | expr :: tail ->
        print_endline (Printf.sprintf "%d" (evalexpr env expr)); print_exprs env tail
  and evalexpr env expr =
    match expr with
    | IdExp id -> Env.find id env
    | NumExp num -> num
    | OpExp (expr1, binop, expr2) ->
        (match binop with
         | Plus -> (evalexpr env expr1) + (evalexpr env expr2)
         | Minus -> (evalexpr env expr1) - (evalexpr env expr2)
         | Times -> (evalexpr env expr1) * (evalexpr env expr2)
         | Div -> (evalexpr env expr1) / (evalexpr env expr2))
    | EseqExp (stmt, expr) ->
        evalexpr (interp_helper env stmt) expr
  and interp_helper env stmt =
    match stmt with
    | PrintStm exprs ->
        print_exprs env exprs; env
    | AssignStm (id1, expr1) ->
        Env.add id1 (evalexpr env expr1) env
    | CompoundStm (stmt1, stmt2) ->
        let env' = interp_helper env stmt1
        in interp_helper env' stmt2
  in interp_helper Env.empty stmt;;

let prog =
  CompoundStm(
    AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(
      AssignStm("b",
        EseqExp(
          PrintStm [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
          OpExp(NumExp 10, Times, IdExp "a"))),
      PrintStm [IdExp "b"]));;

interp prog;;
