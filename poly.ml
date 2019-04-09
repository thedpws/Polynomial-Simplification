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

let rec print_pExp (_e: pExp): unit =
  match _e with
  | Term(a,0) -> string_of_int a |> print_string
  | Term(a,1) -> Printf.printf "%dx" a
  | Term(a,e) -> Printf.printf "%dx^%d" a e
  | Plus(p::ps) ->
          print_string "("; print_pExp p; print_string ")";
      let add_print p =
          print_string " + ("; print_pExp p; print_string ")" in
      List.iter add_print ps
  | Times(ps) -> 
      let mult_print p =
          print_string "("; print_pExp p; print_string ")" in
      List.iter mult_print ps
  | Plus([])
  | Times([]) -> ()
  | _ -> print_string "oopseeeee"


let rec print_pExp_d (_e: pExp): unit =
  match _e with
  | Term(a,e) -> Printf.printf "Term(%d,%d)" a e
  | Plus(p) -> Printf.printf "Plus("; List.iter print_pExp_d p; print_string ")"
  | Times(p) -> Printf.printf "Times("; List.iter print_pExp_d p; print_string ")"
  | _ -> print_string "oopsff"

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
    if n > 0 then from_expr (Mul(expr, Pow(expr, n-1)))
    else if n = 0 then Term(1,0)
    (* 
    else if n = -1 then 
    (
      match expr with
      | Var(v)            -> Term(1, n)
      | Add(expr1, expr2) -> 
      | Sub(expr1, expr2) ->
      | Mul(expr1, expr2) -> 
      | _                 -> Term(0,0)
    )
    else if n < -1 then from_expr (Mul(expr, Pow(expr, n+1)))  
    *)
    else from_expr expr                                         (* exponent = 1 *)
  )
  | _ -> Term(0,0)
  (* | Pos(expr)         -> 
  (
    match expr with
    | Num(n)            ->
    (
      let evalExpr = from_expr expr in
      if n < 0 then Times([Term(-1, 0); evalExpr])
      else evalExpr
    )
    | Var(v)            -> from_expr expr
    | Add(expr1, expr2) -> 
    (
      let evalExpr1 = from_expr expr1 in
      let evalExpr2 = from_expr expr2 in
    )
    | Sub(expr1, expr2) -> 
    (
      let evalExpr1 = from_expr expr1 in
      let evalExpr2 = from_expr expr2 in

    )
    | Mul(expr1, expr2) ->
    (
      let evalExpr1 = from_expr expr1 in
      let evalExpr2 = from_expr expr2 in
    )
    | Pow(expr1, num)    -> 
    (
      let evalExpr1 = from_expr expr1 in
      if (num mod 2) = 1 then 
      else from_expr expr
    )
    | Neg(expr1)         -> from_expr expr1
    
  )
  | Neg(expr)         ->  
  (
    let evalExpr = from_expr expr in
    Times([Term(-1, 0); evalExpr])
  ) *)

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

let sorted (p1: pExp) (p2: pExp) =
    match p1,p2 with
    | Term(_,_), Term(_,_) -> degree p1 > degree p2
    | Term(_,_), _ -> true
    | Plus(_), Times(_) -> true
    | _ -> false
(* Sort all terms  > Plus/Times *)
(* let rec sort (l: pExp): pExp = *)
(*     match l with *)
(*     | Plus(p1::[]) *)
(*     | Times(p1::[])     -> sort p1 *)

(*     | Plus(p1::p2::[])   -> if not (sorted p1 p2) then Plus(p2::[p1]) else l *)
(*     | Times(p1::p2::[])   -> if not ( sorted p1 p2 ) then Times(sort p2::[p1]) else l *)
(*     | Plus(p1::p2::ps)    -> if not ( sorted p1 p2 ) then Plus(p2::(sort p1::ps)) else Plus(p1::(sort p2::ps)) *)
(*     | Times(p1::p2::ps)    -> if not ( sorted p1 p2 ) then Times(p2::(sort p1::ps)) else Times(p1::(sort p2::ps)) *)
(*     | Plus([]) -> l *)
(*     | Times([]) -> l *)
(*     | Term(_,_) -> l *)
(*     | _ -> print_string "oops"; l *)



let rec equalQuestionMark (l1: pExp list) (l2:pExp list): bool =
    print_string "list1: "; List.iter print_pExp_d l1; print_string "list2: ";List.iter print_pExp_d l2; print_endline "";
    match l1,l2 with
    | [],[] -> true
    | [p1;p2], [q1;q2] -> print_endline "hey"; true
    | p::ps, q::qs ->
            (
                match p,q with
                | Term(a1,e1), Term(a2,e2) -> (a1 = a2) && ( e1 = e2 ) && equalQuestionMark ps qs
                | Plus(pp), Plus(qq)
                | Times(pp), Times(qq) -> equalQuestionMark pp qq && equalQuestionMark ps qs
                | _,_ -> print_string "type mismatch"; false
            )
    | _,_ -> print_endline "oopss"; false

and equal_pExp (p1: pExp) (p2: pExp): bool =
    match p1,p2 with
    | Term(_),Term(_) -> equalQuestionMark [p1] [p2]
    | Plus(ps),Plus(qs) -> equalQuestionMark ps qs
    | Times(ps),Times(qs) -> equalQuestionMark ps qs
    | _ -> false
            
(* let rec sort_help (l: pExp list) (*bool,pExp list*) = *)
(*     match l with *)
(*     | p1::p2::ps -> *) 
(*             if not (sorted p1 p2) *) 
(*             then let _, sortlist = sort_help (p1::ps) in *)
(*             true, p1::sortlist *)
(*             else let (sort2_bool, sort2_list) = sort_help (p1::ps) in *)
(*             sort2_bool, p2::sort2_list *)
(*     | _ -> false,l *)

(* let rec sort (l: pExp list): pExp list = *)
(*     let prev = l in *)
(*     let _,after = sort_help l in *)
(*     if (sortEqual prev after) then after *)
(*     else sort after *)


let compatible (p1: pExp) (p2: pExp): bool =
    match p1,p2 with
    | Term(_,e1),Term(_,e2) -> e1 = e2
    | Times(_), Times(_)    -> true
    | Plus(_), Plus(_)      -> true
    | _                     -> false

let rec reduce (p: pExp): pExp =
    match p with
    | Plus(xs) ->
            (
                let reducedXs = reduceAddition xs in
                (* print_string "cunt"; print_pExp (Times(xs)); print_pExp (Times(reducedXs)); *)
                if equalQuestionMark xs reducedXs
                then p
                else reduce (Plus(reducedXs))
            )
    | Times(xs) ->
            (
                let reducedXs = reduceMultiplication xs in
                (* print_string "cunt"; print_pExp (Times(xs)); print_pExp (Times(reducedXs)); *)
                if equalQuestionMark xs reducedXs
                then p
                else reduce (Times(reducedXs))
            )
    | Term(_) -> p
    | _ -> print_string "Reeeduuuuce oopsie!!"; p
(* ReduceAddition adds like terms within a list *)
and reduceAddition (_p: pExp list): pExp list =
    match _p with
    | p1::[]    -> _p
    | Times([])::ps
    | Plus([])::ps -> ps
    | Plus([x])::ps
    | Times([x])::ps -> x::ps
    | p1::p2::ps ->
            (
            match p1,p2 with
            | Term(a1,e1),Term(a2,e2) -> 
                    (* If two terms are compatible, create a new term and recurse *)
                    if compatible p1 p2 
                    then let newTerm = Term(a1+a2,e1) in
                    newTerm::ps
                    (* If not compatible and there's more to reduce, swap and recurse *)
                    else if (not (ps = [])) then ((p2)::(reduceAddition(p1::ps)))
                    else _p
            (* If not 2 terms, reduce head, swap and recurse *)
            | _ -> 
                    (
                    match ps with
                    | [] -> _p
                    | _ -> print_string"semi-OOPSIE"; (reduce p2)::(reduceAddition(p1::ps))
                    )
            )
    | _ -> print_string"reduce oopsie!"; _p

(* ReduceMultipication multiplies like terms *)
and reduceMultiplication (_p: pExp list): pExp list =
    match _p with
    | p1::[]    -> _p
    | Times([])::ps
    | Plus([])::ps -> ps
    | Plus([x])::ps
    | Times([x])::ps -> x::ps
    | p1::p2::ps ->
            (
            match p1,p2 with
            | Term(a1,e1),Term(a2,e2) -> 
                    let newTerm = Term(a1*a2, e1+e2) in
                    if (not (ps = [])) then newTerm::ps
                    else [newTerm]
            | _ ->
                    (
                    match ps with
                    | [] -> _p
                    | _ -> print_string"semi-OOPSIE";( (reduce p2)::(reduceMultiplication(p1::ps)))
                    )
            )
    | _ -> print_string"reduce oopsie!"; _p


(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()
  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)

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
let rec add (p1: pExp) (p2:pExp) : pExp = 
  match (p1, p2) with
  (* | (Term(_,_), Term(_,_))  -> Plus(p1::p2) *)
  | (Plus(ps), Term(_,_))   -> Plus(p2::ps)
  | (Term(_,_), Plus(_))    -> add p2 p1
  (* | (Times(_), Term(_,_))  -> Plus(p1::p2) *)
  (* | (Term(_,_), Times(_))   -> add p2 p1 *)
  | (Plus(ps), Plus(pss))   -> Plus(ps @ pss)
  (* | (Times(ts), Times(tss)) -> Plus(p1::p2) *)
  | (Times(_), Plus(ps))     -> Plus(p1::ps)
  (* | (Pluse(), Times())      -> add p2 p1 *)
  | (_,_) -> Plus([p1; p2])
   

let rec multiply (p1: pExp) (p2: pExp) : pExp =
    match (p1, p2) with
    | (Times(ts), Term(_,_)) -> Times (p2::ts) (* 5x2 • 3  = (5x2)(3)  *)
    | (Times(ts), Plus(ps))  -> 
            (
            match ps with
            | pp::pps        -> Plus( (multiply p1 pp)::[(multiply p1 (Plus(pps)))] )  (* 5xx • (x+2+3)    = 5xxx + { 5xx • (2+3) } *)
            | []             -> p1
            )
    | (Times(ts), Times(tss)) -> Times(ts @ tss) (* Append times to the list *)
    | (Plus(_), Times(_))         -> multiply p2 p1 (* Reverse the order *)
    | (Plus(pp::pps), Term(_,_))          -> Plus((multiply pp p2) :: [(multiply (Plus(pps)) p2)])
    | (Plus(p1::p1s), Plus(_))          -> (Plus( multiply p1 p2 :: [multiply (Plus(p1s)) p2])) (* (5 + x) • (x + 3 + x) = 5•(x+3+x) + x•(x+3+x) *)
    | (Term(_,_), Times(_))         -> multiply p2 p1
    | (Term(_,_), Plus(_))          -> multiply p2 p1
    | (Term(a1,e1), Term(a2,e2))      -> Term(a1*a2,e1+e2)         (* 5 • 10x  = (5)(10x)  *)
    | (_,_) -> p1

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
    | Plus([pp]) -> flattenPlus pp
    | Plus(pp::pps) -> 
        (
        match pp with
        | Plus(s) -> flatten ( Plus(s @ pps )) (* Plus -> flatten and extract *) (* Recursion: look *)
        | _         -> 
                let recursed = flatten ( Plus(pps) ) in
                let other = flatten pp in
                (* print_pExp_d recursed; print_endline "<- recursed"; print_pExp_d other; print_endline "<- other"; *)
                (
                match other,recursed with
                | Plus(x1), Plus(x2) -> Plus(x1@x2)
                | Times(x1), Times(x2) -> Times(x1@x2)
                | Plus(x1), Term(_) -> Plus(x1@[recursed])
                | Times(x1), Term(_) -> Times(x1@[recursed])
                | _ -> Plus(other::(extract recursed))
                )
        )
    | Plus([]) -> _p
    | _ -> print_endline "help"; _p
and flattenTimes (_p: pExp): pExp =
    match _p with
    | Times([pp]) -> flattenTimes pp
    | Times(pp::pps) -> 
        (
        match pp with
        | Times(s) -> flatten ( Times(s @ pps) ) (* Times -> flatten and extract *) (* Recursion: look *)
        | _         -> 
                let recursed = flatten ( Times(pps )) in
                let other = flatten pp in
                (
                match other,recursed with
                | Plus(x1), Plus(x2) -> Plus(x1@x2)
                | Times(x1), Times(x2) -> Times(x1@x2)
                | Plus(x1), Term(_) -> Plus(x1@[recursed])
                | Times(x1), Term(_) -> Times(x1@[recursed])
                | _ -> Times(other::(extract recursed))
                )
        )
    | Times([]) -> _p
    | _ -> print_endline "help"; _p

let flatten (p: pExp): pExp =
    match p with
    | Plus(ps) ->
            let rec flattenP ps =
            (
            match ps with
            | (Plus(pss))::xs -> pss@(flattenP xs)
            | x::xs -> x::(flattenP xs)
            | _ -> ps
            ) in Plus(flattenP ps)
    | Times(ps) ->
            let rec flattenP ps =
            (
            match ps with
            | (Times(pss))::xs -> pss@(flattenP xs)
            | x::xs -> x::(flattenP xs)
            | _ -> ps
            ) in Times(flattenP ps)
    | _ -> p

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

(* Simplify looks for opportunities to add/multiply *)
let rec simplify (e:pExp): pExp =
    let before = e in
    (* let e = flatten e |> reduce |> flatten in *)
    let e = 
        (
    match e with
    | Plus(xs) ->
            (
            match xs with
            | p1::[] -> p1
            | p1::p2::[] -> add p1 p2
            | p1::p2::ps -> simplify1 (Plus((add p1 p2)::ps))
            | [] -> print_string "Oh noes adds!"; Term(0,0)
            )
    | Times(xs) ->
            (
            match xs with
            | p1::[] -> p1
            | p1::p2::[] -> multiply p1 p2
            | p1::p2::ps -> simplify1 (Times( (multiply p1 p2)::ps ))
            | [] -> print_string "Oh noes times!"; Term(0,0)
            )
    | Term(_) -> e
        ) in
    (* let e = flatten e |> reduce |> flatten in *)
    let after = e in
    if equal_pExp before after then after
    else simplify1 after



(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
(* let rec equal_pExp (_e1: pExp) (_e2: pExp) : bool = *)
(*   match _e1 with *)
(*   | Term(a1, e1) -> *) 
(*   ( *)
(*     match _e2 with *)
(*     | Term (a2, e2) -> *) 
(*     ( *)
(*       if (a1 = a2 && e1 = e2) then true *)
(*       else false *)
(*     ) *)
(*     | _ -> false *)
(*   ) *)
(*   | Plus(ps1::pss1) -> *)
(*   ( *)
(*     match _e2 with *)
(*     | Plus(ps2::pss2) -> ((equal_pExp ps1 ps2) && (equal_pExp (Plus(pss1)) (Plus(pss2)))) *)
(*     | _               -> false *)
(*   ) *)
(*   | Plus([]) -> *)
(*   ( *)
(*     match _e2 with *)
(*     | Plus([]) -> true *)
(*     | Plus(ps) -> false *)
(*     | _        -> false *)
(*   ) *)
(*   | Times(ts) -> false *)
(*   | _         -> false *)

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
(* let rec simplify (e:pExp): pExp = *)
(*     let rE = simplify1(e) in *)
(*       print_pExp rE; *)
(*       if (equal_pExp e rE) then *)
(*         e *)
(*       else *)  
(*         simplify(rE) *)
