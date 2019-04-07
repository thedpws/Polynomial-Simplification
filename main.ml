open Lexing
open Parser
open Lexer
open Poly
open Expr

let test1 = Term(1,1)
let test2 = Plus([test1 ; test1])
let test3 = Times([test1 ; test1])
let test4 = Plus([test2; test3])
let _ = print_pExp test4
(*
let filename = Sys.argv.(1)

let () = 
  open_in filename |>
  Lexing.from_channel |>
  Parser.main Lexer.token |>
  print_expr |>
  from_expr |>
  simplify |>
  print_pExp
*)
