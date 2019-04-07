(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)


(*
  Function to traslate between AST expressions
  to pExp expressions
*)
let rec from_expr (_e: Expr.expr) : pExp =
  match _e with
  | Num(n)            -> Term(n, 0)
  | Var(v)            -> Term(1, 1)
  | Add(expr1, expr2) -> 
  (
    let evalExpr1 = from_expr expr1 in
    let evalExpr2 = from_expr expr2 in
    Plus([evalExpr1; evalExpr2])                       
  )
  | Sub(expr1, expr2) ->
  (
    let evalExpr1 = from_expr expr1 in
    let evalExpr2 = from_expr expr2 in
    Plus([evalExpr1; Times([Term(-1,0); evalExpr2])])               
  )
  | Mul(expr1, expr2) ->
  (
    let evalExpr1 = from_expr expr1 in
    let evalExpr2 = from_expr expr2 in
    Times([evalExpr1; evalExpr2])                   
  )
  | Pow(expr, n)     -> 
  (
    if n > 1 then from_expr (Mul(expr, Pow(expr, n-1)))
    else if n < 1 then from_expr (Mul(expr, Pow(expr, n+1)))
    else from_expr expr
  )
  | Pos(expr)         -> 
  (
    (* Need to fix *)
    let evalExpr = from_expr expr in
    Times([Term(1, 0); evalExpr])
    
  )
  | Neg(expr)         ->  
  (
    let evalExpr = from_expr expr in
    Times([Term(-1, 0); evalExpr])
  )

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let rec degree (_e:pExp): int =
  match _e with
  | Term(n,m) -> m
  | Plus(p::ps) -> if degree p > degree (Plus ps) then degree p else degree (Plus ps )
  | Times(p::ps) -> degree p + degree (Times ps)
  | _ -> 0

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
(*
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2
(* returns true if in order *)
  *)

let rec sort (l: pExp): pExp =
    match l with
    | Plus(p1::[])
    | Times(p1::[])     -> l

    | Plus(p1::p2::[])   -> if degree p1 > degree p2 then Plus(p2::[p1]) else l
    | Times(p1::p2::[])   -> if degree p1 > degree p2 then Times(p2::[p1]) else l
    | Plus(p1::p2::ps)    -> if degree p1 > degree p2 then Plus(p2::(sort p2::ps)) else Plus(p1::(sort p2::ps))
    | Times(p1::p2::ps)    -> if degree p1 > degree p2 then Times(p2::(sort p2::ps)) else Times(p1::(sort p2::ps))

(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)
let rec print_pExp (_e: pExp): unit =
  match _e with
  | Term(0,_) -> ();
  | Term(a,0) -> string_of_int a |> print_string;
  | Term(a,e) -> Printf.printf " %dx^%d " a e;
  | Plus(ps) -> 
      let add_print p =
        print_pExp p; print_string ") + (" in
      Printf.printf "("; List.iter add_print ps; Printf.printf ")";
  | Times(ps) -> 
      let mult_print p =
        print_pExp p; print_string ")(" in
      Printf.printf "("; List.iter mult_print ps; Printf.printf ")";
  print_newline()

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)


let rec multiply (p1: pExp) (p2: pExp) : pExp =
    match (p1, p2) with
    | (Times(ts), Term(_,_))     -> Times (p2::ts) (* 5x2 • 3  = (5x2)(3)  *)
    | (Times(ts), Plus(ps))  -> 
            (
            match ps with
            | pp::pps        -> Plus( (multiply p1 pp)::[(multiply p1 (Plus(pps)))] )  (* 5xx • (x+2+3)    = 5xxx + { 5xx • (t+3) } *)
            | []            -> p1
            )
    | (Times(ts), Times(tss)) -> Times(ts @ tss) (* Append times to the list *)
    | (Plus(_), Times(_))         -> multiply p2 p1 (* Reverse the order *)
    | (Plus(_), Term(_,_))          -> Times([p2]) |> multiply p1
    | (Plus(p1::p1s), Plus(_))          -> (Plus( multiply p1 p2 :: [multiply (Plus(p1s)) p2])) (* (5 + x) • (x + 3 + x) = 5•(x+3+x) + x•(x+3+x) *)
    | (Term(_,_), Times(_))         -> multiply p2 p1
    | (Term(_,_), Plus(_))          -> multiply p2 p1
    | (Term(_,_), Term(_,_))      -> Times([p1 ; p2])         (* 5 • 10x  = (5)(10x)  *)

(* If 1 elem in Plus/Times, flatten to Term *)
(* Call flatten on every element in the list *)
(* If same type as toptype, extract. *)
(* Else, flatten *)

(* Extracts the pExpressions from Plus/Times *)
let extract (p: pExp): pExp list =
    match p with
    | Plus(x) -> x
    | Times(x) -> x
    | _ -> [p]

let rec flatten (p: pExp): pExp =
    match p with
    | Plus(_) -> flattenPlus p
    | Times(_) -> flattenTimes p
    | Term(_,_) -> p
and flattenPlus (_p: pExp): pExp =
    match _p with
    | Plus(pp::pps) -> 
        (
        match pp with
        | Plus(s) -> flattenPlus ( Plus(s @ pps )) (* Plus -> flatten and extract *) (* Recursion: look *)
        | _         -> 
                let recursed = flattenPlus ( Plus(pps) ) in
                let other = flatten pp in
                ( Plus(other::(extract recursed)))
        )
    | _ -> print_endline "help"; Term(0,0)
and flattenTimes (_p: pExp): pExp =
    match _p with
    | Times(pp::pps) -> 
        (
        match pp with
        | Times(s) -> flattenTimes ( Times(s @ pps) ) (* Times -> flatten and extract *) (* Recursion: look *)
        | _         -> 
                let recursed = flattenTimes ( Times(pps )) in
                let other = flatten pp in
                ( Times(other::(extract recursed)))
        )
    | _ -> print_endline "help"; Term(0,0)

    (*
let rec reduce (p: pExp): pExp =
    match p with
    | Term -> p
    | Plus(ps) -> (* for each elem, look for a compatible term to add. If found, call again on self. If not, call on tail *)
    | Times(ts) -> (* same *)
            *)


    (*
let rec distribute (arg1: Times) (arg2: Plus) : Plus =
    (* For each in Plus, multiply by times. *)
    let rval = [] in
    match arg2 with
    | p::[]     -> multiply arg1 p
(* let rec add (p1: pExp) (p2: pExp) : Plus =
  match (p1, p2) with
  | ()
 *)
*)

let simplify1 (e:pExp): pExp =
    e

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  true

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      print_pExp rE;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)
