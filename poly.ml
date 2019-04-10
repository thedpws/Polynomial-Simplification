(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int
  | Plus of pExp list (* List of terms added *)
  | Times of pExp list (* List of terms multiplied *)
  | None
  | Error of string

let rec delist (l: string list): string = 
    match l with
    | [] -> ""
    | [x] -> x
    | x::xs -> x^(delist xs)

(* Turns p expression into string *)
let rec string_pExp (_e: pExp): string =
  match _e with
  | Term(1,0) -> "1"
  | Term(a,0) -> string_of_int a
  | Term(1,e) -> Printf.sprintf "x^%d" e
  | Term(a,1) -> Printf.sprintf "%dx" a
  | Term(a,e) -> Printf.sprintf "%dx^%d" a e
  | Plus(p::ps) ->
          let rval = "("^(string_pExp p)^ ")" in
      let add_print p = Printf.sprintf " + (%s)" (string_pExp p) in
      let recursed = List.map add_print ps in
      rval^(delist recursed)

  | Times(ps) -> 
      let add_print p = Printf.sprintf "(%s)" (string_pExp p) in
      let recursed = List.map add_print ps in
      (delist recursed)
  | Plus([]) -> "Empty Plus"
  | Times([]) -> "Empty Times"
  | None -> "None"
  | Error(msg) -> msg
  | _ -> "oopseeeee"

(* Turns p expression into debug string *)
let rec string_pExp_d (p: pExp): string =
    match p with
    | Term(a,e) -> Printf.sprintf "Term(%d,%d)" a e
    | Plus(xs) -> "Plus("^(List.map string_pExp_d xs |> delist)^")"
    | Times(xs) -> "Times("^(List.map string_pExp_d xs |> delist)^")"
    | Error(msg) -> Printf.sprintf "Error: %s" msg

(* Prints p expression as string *)
let print_pExp (p: pExp): unit =
    print_endline (string_pExp p)

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
    match expr with
    | Var(v) -> Term(1,n)
    | _ -> 
    (
      if n > 0 then from_expr (Mul(expr, Pow(expr, n-1)))
      else if n = 0 then Term(1,0)
      else from_expr expr                                         (* exponent = 1 *)
    )
  )
  | Pos(expr) -> from_expr expr
  | Neg(expr) -> 
  (
    match expr with
    | Num(n) -> Term(-1*n, 0)
    | Var(v) -> Term(-1, 1)
    | _ -> 
    (
      let evalExpr = from_expr expr in
      Times([Term(-1, 0); evalExpr])
    )
  )
  | _ -> Error("Fail to match a pExp")

let rec add (p1: pExp) (p2: pExp): pExp =
    let result =
        (
    match p1, p2 with
    | Term(a1,e1),Term(a2,e2) ->
            if e1 = e2 then Term(a1+a2,e2)
            else Plus([p1;p2])

    (* Base case *)
    | Term(a1,e1), Plus([]) -> Term(a1,e1)
    (* Recurse: if Compatible terms, add. Else, swap and recurse *)
    | Term(a1,e1), Plus(Term(a2,e2)::ps) ->
            (* Look for a like term to add *)
            if e1 = e2 then Plus(Term(a1+a2,e1)::ps)
            else let newTerms = ((Term(a2,e2)) :: (Term(a1,e1)) :: ps) in Plus(newTerms)
    (* Recurse: Swap and recurse *)
    | Term(a1,e1), Plus(p::ps) ->
            let newTerms = (p :: (Term(a1,e1)) :: ps) in Plus(newTerms)

    (* | Plus(p1s), Plus(p2s) -> Plus(p1s @ p2s) *)
    | Plus(p1s), Plus([]) -> p1
    | Plus(p1s), Plus(p2h::p2s) ->
            let sub = add p1 p2h in
            add sub (Plus p2s)

    | Plus(_), Term(_) -> add p2 p1

    | Times(_),_
    | _, Times(_) ->
            let msg = Printf.sprintf "This Times was never simplified before calling add: %s" (string_pExp p2) in Error(msg)

    | _,_ -> let msg = Printf.sprintf "Could not add these: (%s) (%s)" (string_pExp p1) (string_pExp p2) in Error(msg)
        ) in
    (* Printf.printf ("Add: (%s)+(%s) = (%s)\n") (string_pExp_d p1) (string_pExp_d p2) (string_pExp_d result); result *)
    (* Printf.printf ("Add: (%s)+(%s) = (%s)\n") (string_pExp p1) (string_pExp p2) (string_pExp result); result *)
    result

let rec multiply (p1: pExp) (p2: pExp): pExp =
    let result =
        (
    match p1,p2 with
    | _,Plus([]) -> Term(0,0)
    | _,Times([]) -> p1

    | Term(a1,e1), Term(a2,e2) -> Term(a1*a2, e1+e2)
    | Term(a,e), Plus(p::ps) ->
            let subproduct = multiply p1 p in
            let recursed = multiply p1 (Plus ps) in
            add subproduct recursed


    (* Base case: Plus[] -> return 0 *)
    | Term(a,e), Times([]) -> Term(a,e) (* TODO *)
    (* Recurse: Combine terms and recurse *)
    | Term(a,e), Times(t::ts) ->
            let subproduct = multiply p1 t in
            multiply subproduct (Times(ts))
    (* Base case: Times[] -> return p1 *)
    (* Recurse: Get first product and recurse, adding to other products *)
    | Plus(p1s), Plus(p2h::p2s) -> 
            let subproduct = multiply p1 p2h in
            let recursed = multiply (Plus p1s)  (Plus p2s)  in
            add subproduct recursed
    (* Base case: Times[] -> return p1 *)
    (* Recurse: Apply one factor at a time *)
    | Plus(p1s), Times(p2h::p2s) ->
            let subproduct = multiply p1 p2h in
            multiply subproduct (Times p2s)

    (* Recurse: multiply one factor at a time *)
    | Times(t1s), Times(t2::t2s) ->
            let subproduct = multiply (Times t1s) t2 in
            let recursed = multiply (Times t1s) (Times t2s) in
            multiply subproduct recursed
    (* Base case *)
    | Times(t1s), Times([]) -> (Times t1s)
    | Times([]),_
    | Plus([]),_ -> p2

    | _,Plus([])
    | _,Times([])
    | Times(_),_
    | Plus([]),Term(_)
    | Plus(_),Term(_) -> multiply p2 p1
    | _ -> 
            let msg = Printf.sprintf "Case not handled in multiply for terms: (%s) (%s)" (string_pExp p1) (string_pExp p2) in
            Error(msg)
        ) in
    (* Printf.printf ("Multiply: (%s)*(%s) = (%s)\n") (string_pExp_d p1) (string_pExp_d p2) (string_pExp_d result); result *)
    (* Printf.printf ("Multiply: (%s)*(%s) = (%s)\n") (string_pExp p1) (string_pExp p2) (string_pExp result); result *)
    result

let rec removeIdentities (p: pExp): pExp =
  match p with
  | Term(_) -> p
  | Plus([]) -> p
  | Plus(pp::pps) ->
          (
          match pp with
          | Term(0,_) -> removeIdentities (Plus pps)
          | _ -> 
                  (
                      match removeIdentities (Plus pps) with
                      | Plus(pps) -> Plus(pp::pps)
                      | Plus([]) -> Plus([pp])
                  )
          )
  | Times([]) -> p
  | Times(tt::tts) ->
          (
              match tt with
              | Term(1,0) -> removeIdentities (Times tts)
              | _ ->
                      (
                          match removeIdentities (Times tts) with
                          | Times(tts) -> Times(tt::tts)
                          | Times([]) -> Times([tt])
                      )
          )
  | _ -> Error("Unmatched case in removeIdentities")
  
(* Computes degree of expression *)
let rec degree (p: pExp): int =
    match p with
    | Term(_,e) -> e
    | Plus(x::[]) -> degree x
    | Plus(x::xs) ->
    (
      let d1 = degree x in
      let d2 = degree (Plus(xs)) in
      if (d1 > d2) then d1
      else d2
    )
    | _ -> 0 

(* Sorts expression by largest degree to lowest degree *)
let rec sort (p:pExp): pExp = 
  match p with
  | Term(_) -> p
  | Plus([]) -> p
  | Plus(_::[]) -> p
  | Plus(x::xs::[]) -> 
  (
    if (degree xs > degree x) then Plus(xs::x::[])
    else p
  )
  | Plus(x::xs::xss) -> let sorted = 
  (
    if (degree xs > degree x) then (xs::x::xss)
    else let (Plus tail) = sort (Plus(xs::xss)) in
    x::tail
  ) in Plus(sorted)
  
(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let rec equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  match _e1 with
  | Term(a1, e1) -> 
  (
    match _e2 with
    | Term (a2, e2) -> 
    (
      if (a1 = a2 && e1 = e2) then true
      else false
    )
    | _ -> false
  )
  | Plus(ps1::pss1) ->
  (
    match _e2 with
    | Plus(ps2::pss2) -> ((equal_pExp ps1 ps2) && (equal_pExp (Plus(pss1)) (Plus(pss2))))
    | _               -> false
  )
  | Plus([]) ->
  (
    match _e2 with
    | Plus([]) -> true
    | Plus(ps) -> false
    | _        -> false
  )
  | Times(ts) -> false
  | _         -> false
  
(* Incomplete. Requires Fixed point checks. Or we could perform this routing N times where N is the degree of the polynomial... *)
(* let rec addLikeTerms (p: pExp): pExp =
  match p with
  | Plus(p1::p2::pps) ->
          let newTerms = 
              (
          match p1, p2 with
          Term(a1,e1), Term(a2,e2) ->
              if (e1 = e2) then (Term(a1+a2,e1))::pps
              else let (Plus aaa) = addLikeTerms (Plus(p1::pps)) in
              p2::aaa
          ) in Plus(newTerms)
  | Plus(x::[]) -> p
  | Plus([]) -> p
  | _ -> p *)

let rec addLikeTerms (p: pExp): pExp =
  match p with
  | Plus(p1::p2::pps) ->
  (
    let newTerms = 
    (
      match p1, p2 with
      Term(a1,e1), Term(a2,e2) ->
      (
        if (e1 = e2) then (Term(a1+a2,e1))::pps
        else let (Plus aaa) = addLikeTerms (Plus(p2::pps)) in
        p1::aaa
      )
    ) in Plus(newTerms)
  )
  | Plus(x::[]) -> p
  | Plus([]) -> p
  | _ -> p


(* SimplifyPlus :   Times -> SimplifyTimes  | Plus -> extract   | Term -> add       *)
(* SimplifyTimes:   Plus -> SimplifyPlus    | Times -> extract  | Term -> multiply  *)
let rec simplify (p: pExp): pExp =
    (* Printf.printf "##### SIMPLIFY: (%s) #####\n" (string_pExp_d p); *)
    let result = 
      (
    match p with
    | Times(x::xs) | Plus(x::xs) ->
            let subresult = simplify x in
            (
            match p with
            | Times(_) -> multiply subresult (simplify (Times xs))
            | Plus(_) -> add subresult (simplify (Plus xs))
            | _ -> Error("oops")
            )
    | Error(msg) -> print_endline msg; p
    | _ -> p
      ) in result |> removeIdentities

let rec simplify2 (e:pExp) : pExp =
  let origSort = sort e in
  (* print_pExp origSort; *)
  if (equal_pExp origSort e) then
    let rE = addLikeTerms origSort in
    (* print_pExp rE; *)
    if(equal_pExp rE origSort) then
      rE
    else 
      simplify2 rE
  else
    simplify2 origSort


      