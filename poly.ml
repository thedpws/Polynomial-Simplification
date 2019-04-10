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
  | None
  | Error of string



let rec delist (l: string list): string = 
    match l with
    | [] -> ""
    | [x] -> x
    | x::xs -> x^(delist xs)

let rec string_pExp (_e: pExp): string =
  match _e with
  | Term(a,0) -> string_of_int a
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


let rec string_pExp_d (p: pExp): string =
    match p with
    | Term(a,e) -> Printf.sprintf "Term(%d,%d)" a e
    | Plus(xs) -> "Plus("^(List.map string_pExp_d xs |> delist)^")"
    | Times(xs) -> "Times("^(List.map string_pExp_d xs |> delist)^")"
    | Error(msg) -> Printf.sprintf "Error: %s" msg

let print_pExp (p: pExp): unit =
    print_endline (string_pExp p)

(* let rec print_pExp_d (_e: pExp): unit = *)
(*   match _e with *)
(*   | Term(a,e) -> Printf.printf "Term(%d,%d)" a e *)
(*   | Plus(p) -> Printf.printf "Plus("; List.iter print_pExp_d p; print_string ")" *)
(*   | Times(p) -> Printf.printf "Times("; List.iter print_pExp_d p; print_string ")" *)
(*   | _ -> print_string "oopsff" *)

let rec degree (_e:pExp): int =
  match _e with
  | Term(n,m) -> m
  | Plus(p::ps) -> if degree p > degree (Plus ps) then degree p else degree (Plus ps )
  | Times(p::ps) -> degree p + degree (Times ps)
  | _ -> 0


let rec add (p1: pExp) (p2: pExp): pExp =
    let prefix = Printf.sprintf "Add: (%s) (%s)\n" (string_pExp_d p1) (string_pExp_d p2) in
    let result =
        (
    match p1, p2 with
    (* | _,Plus([]) -> add p1 (Term(0,0) *)
    (* | Plus([]),_ -> add p2 (Term(0,0) ) *)
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
    (* Printf.printf ("Add: (%s)+(%s) = (%s)\n") (string_pExp p1) (string_pExp_d p2) (string_pExp_d result); result *)
    Printf.printf ("Add: (%s)+(%s) = (%s)\n") (string_pExp p1) (string_pExp p2) (string_pExp result); result

let rec multiply (p1: pExp) (p2: pExp): pExp =
    let result =
        (
    match p1,p2 with
    (* | None, _ -> multiply (Term (1,0)) p2 *)
    (* | _ , None -> multiply p1 (Term (1,0)) *)
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
    Printf.printf ("Multiply: (%s)*(%s) = (%s)\n") (string_pExp p1) (string_pExp p2) (string_pExp result); result




(* SimplifyPlus :   Times -> SimplifyTimes  | Plus -> extract   | Term -> add       *)
(* SimplifyTimes:   Plus -> SimplifyPlus    | Times -> extract  | Term -> multiply  *)
let rec simplifyComplete (p: pExp): pExp =
    Printf.printf "###########Simplify: (%s)\n" (string_pExp_d p);
    match p with
    | Times(x::xs) | Plus(x::xs) ->
            let subresult = simplifyComplete x in
            (
            match p with
            | Times(_) -> multiply subresult (simplifyComplete (Times xs))
            | Plus(_) -> add subresult (simplifyComplete (Plus xs))
            | _ -> Error("oops")
            )
    (* | Times([]) | Plus([]) -> None *)
    | Error(msg) -> print_endline msg; p
    | _ -> p
let rec clean (p: pExp): pExp =
    match p with
    | Term(0,_) -> None
    | _ -> p

(* let rec simplify (p: pExp): pExp = *)
(*     match p with *)
(*     | Plus(_) -> simplifyPlus p *)
(*     | Times(_) -> simplifyTimes p *)
(*     | _ -> p *)
(* and simplifyPlus (p: pExp): pExp = *)
(*     ( *)
(*     match p with *)
(*     | None -> None *)
(*     | Plus(x::xs) -> *)
(*             ( *)
(*                 let subresult = *)
(*                 ( *)
(*                 match x with *)
(*                 | Plus(_)   -> simplifyPlus x *)
(*                 | Times(_)  -> simplifyTimes x *)
(*                 | _         -> x *)
(*                 ) *) 
(*                 in add subresult (simplifyPlus(Plus(xs))) *)
(*             ) *)
(*     | _ -> Error("Tried to call simplifyPlus on nonPlus term") *)
(*     ) *)
(* and simplifyTimes (p: pExp): pExp = *)
(*     match p with *)
(*     | None -> None *)
(*     | Times(x::xs) -> *)
(*             ( *)
(*                 let subresult = *)
(*                 ( *)
(*                 match x with *)
(*                 | Plus(_)   -> simplifyPlus x *)
(*                 | Times(_)  -> simplifyTimes x *)
(*                 | _         -> x *)
(*                 ) *)
(*                 in multiply subresult (simplifyTimes(Times xs)) *)
(*             ) *)
(*     | _ -> Error("Tried to call simplifyTimes on nonTimes term") *)

(* let rec equal_pExp (p1: pExp) (p2: pExp): bool = *)
            
