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

(* let _ = main test8 *)


let x1 = Term(1,1)
let x2 = Term(1,2)
let x3 = Term(1,3)
let sort1 = [x1 ; x3 ; x2 ; x3 ; x2 ] (* x^3 + x^2 + x^1 *)

(* let main test = sort test |> List.iter print_pExp *)
(* let _ = main sort1 *)

(* let equal1 = [x1 ; x1 ; x2] *)
(* let equal2 = [x1 ; x2 ; x1] *)
(* let main e1 e2 = equalQuestionMark e1 e2 *)
(* let _ = if main equal1 equal2 then print_string "bitch they equal" else print_string "faggot theyre not equal" *)

let reduceAdd1 = Plus([Term(2,1) ; x1 ; x2 ; x1])
let reduceAdd2 = Term(1,2)
let reduceAdd3 = flatten (Plus([ Times([Term(1,0)])  ; Term(1,0) ]))
let reduceAdd4 =  (Plus([ Times([Term(1,0)])  ; Term(1,0) ]))
let main test = reduce test |> print_pExp

let reduceMult1 = Times([x1 ; x1])
let reduceMult2 = Times([x1 ; x2])
let reduceMult3 = Times([])

let reduce1 = (Plus([ x1 ; (Times([ x1 ; x2 ])) ; x1 ]))
(* x + (x*x^2) + x  |-> (x^3 + 2x) *)
let reduce2 = (Times([ x1 ; (Plus([ x1 ; x2 ])) ; x1 ]))
(* x • (x + x^2) • x |-> x^2 • (x + x^2) *)

let reduce3 = Times([ (Plus([ x1 ; Term(1,0) ])) ; (Plus([ x1 ; Term(1,0) ]))  ])

let _ = main reduce3

let test11 = equal_pExp (Plus([Term(3,2); Term(1,2)])) (Plus([Term(3,2); Term(2,2)])) |> string_of_bool |> print_endline (* 3x^2 + x^2 == 3x^2 + 2x^2*)
let test11 = equal_pExp (Plus([Term(3,2)])) (Plus([Term(3,2); Term(3,2)])) |> string_of_bool |> print_endline            (* 3x^2 == 3x^2 + 3x^2 *)
let test11 = equal_pExp (Plus([Term(3,2); Term(2,2)])) (Plus([Term(3,2); Term(2,2)])) |> string_of_bool |> print_endline (* 3x^2 + 2x^2 == 3x^2 + 2x^2*)

(* let filename = Sys.argv.(1) *)

(*   open_in filename |> *)
(*   Lexing.from_channel |> *)
(*   Parser.main Lexer.token |> *)
(*   print_expr |> *)
(*   from_expr |> *)
(*   simplify |> *)
(*   print_pExp *)

let x = Term(1,1)
let one = Term(1,0)
let minusOne = Term(-1,0)
let main test = test |> flatten |> reduce |> flatten |> simplify1 |> print_pExp

let simplify2 = Plus([ x1 ; x1])
let simplify3 = Times([ x1 ; x1 ; x2 ; (Plus([ x1 ; x2 ])) ])
let simplify4 = Times([ one ; minusOne ])
let simplify5 = Plus([ one ; minusOne ])
let simplify6 = Times([ x; Plus([ x ; one ]) ])
let simplify7 = Times([ one; Plus([ x ; one ]) ])
let simplify9 = Times([ Plus([ x ; one ]) ; Plus ([ x ; minusOne ]) ])
let _ = main simplify7
