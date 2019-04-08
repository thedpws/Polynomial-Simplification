open Lexing
open Parser
open Lexer
open Poly
open Expr

let test0 = Term(0,3) (* {nothing} *)
let test1 = Term(1,1) (* 1x *)
let test2 = Plus([test1 ; test1]) (* (1x) + (1x) *)
let test3 = Times([test1 ; test1]) (* (1x)(1x) *)
let test4 = Plus([test2; test3]) (* ( (1x) + (1x) )((1x)(1x)) *)
let test5 = Plus([test2;test2;test2; test1])
let test6 = Term(0,1) (* 0 *)
let test7 = Term(1,0) (* 1 *)
(* let main _ = print_pExp test7 *)

let test1 = Term(1,1) (* 1x *)
let test2 = Plus([test1; test1]) (* 1x + 1x *)
let test3 = Plus([Plus([test1; test1])]) (* (1x + 1x) *)
let test4 = Plus([test3]) (* 1x + 1x *)
let test5 = Times([test4 ; test4]) (* (1x + 1x)(1x + 1x) *)
let test6 = Times([test4]) (* 1x + 1x *)
let test7 = Plus([test6]) (* 1x + 1x *)

let test8 = Plus([test7 ; Times([test1])]) (* (1x + 1x) + (1x) *)

let test8 = Plus([Times([Plus([Term(1,1); Term(1,1)])]); Times([Term(1,1)])]) (* 1x + 1x + 1x *)
let test9 = Plus([ Plus([ Term(1,1) ; Term(1,1) ]) ; Term(1,1) ])
let main test = flatten test |> print_pExp_d; print_endline ""; flatten test |> print_pExp

let _ = main test8

let test11 = equal_pExp (Plus([Term(3,2); Term(1,2)])) (Plus([Term(3,2); Term(2,2)])) |> string_of_bool |> print_endline (* 3x^2 + x^2 == 3x^2 + 2x^2*)
let test11 = equal_pExp (Plus([Term(3,2)])) (Plus([Term(3,2); Term(3,2)])) |> string_of_bool |> print_endline            (* 3x^2 == 3x^2 + 3x^2 *)
let test11 = equal_pExp (Plus([Term(3,2); Term(2,2)])) (Plus([Term(3,2); Term(2,2)])) |> string_of_bool |> print_endline (* 3x^2 + 2x^2 == 3x^2 + 2x^2*)


(* let filename = Sys.argv.(1)

let () = 
  open_in filename |>
  Lexing.from_channel |>
  Parser.main Lexer.token |>
  print_expr |>
  from_expr |>
  simplify |>
  print_pExp *)
