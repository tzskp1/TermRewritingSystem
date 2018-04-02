(* not tested *)

(*
 A : base set
 A <-> 'a
 A^n <-> 'a list of length n 
 a multiset of A <-> 'a list
*)

type order =
  | GR
  | EQ
  | NGE

(* >_{Lex} *)
let rec lex' (ord : 'a -> 'a -> order) =
  function
  | _::_,[] | [],_::_ -> failwith "lex' : undefined"
  | [],[] -> EQ
  | x::xs,y::ys ->
     match ord x y with
     | EQ -> lex' ord (xs,ys)
     | NGE -> NGE 
     | GR -> GR

(* >_{lex} *)
let lex (ord : 'a -> 'a -> order) (xs,ys: 'a list * 'a list) =
  let rec iter =
    function
    | _::_,[] | [],_::_ -> failwith "never used"
    | [],[] -> EQ
    | x::xs,y::ys ->
       begin match ord x y with
       | EQ -> iter (xs,ys)
       | NGE -> NGE 
       | GR -> GR
       end
  in 
  if List.length xs != List.length ys
  then failwith "lex : undefined"
  else iter (xs,ys)

let rem1 (ord : 'a -> 'a -> order) (xs,y : 'a list * 'a) =
  List.filter (fun x -> ord x y != EQ) xs

let mdiff (ord : 'a -> 'a -> order) (xs,ys : 'a list * 'a list) =
  List.fold_left (fun xs' y -> rem1 ord (xs',y)) xs ys

let null = function
  | [] -> true
  | _ -> false

let mul (ord : 'a -> 'a -> order) (ms,ns: 'a list * 'a list) =
  let nms = mdiff ord (ns,ms) in
  let mns = mdiff ord (ms,ns) in
  if null nms && null mns
  then EQ
  else if List.for_all (fun n -> List.exists (fun m -> ord m n = GR) mns) nms (* not efficient *)
  then GR
  else NGE

(*
 a multiset of A <-> ('a * int) list
*)
let mdiff' (ord : 'a -> 'a -> order) (xs,ys : ('a * int) list * ('a * int) list) =
  List.map (fun (xk,xv) -> (xk,xv - List.assoc xk ys)) xs
  |> List.filter (fun (_,n) -> n > 0)

let mul' (ord : 'a -> 'a -> order) (ms,ns: ('a * int) list * ('a * int) list) =
  let nms = mdiff' ord (ns,ms) in
  let mns = mdiff' ord (ms,ns) in
  if null nms && null mns
  then EQ
  else if List.for_all (fun (n,_) -> List.exists (fun (m,_) -> ord m n = GR) mns) nms
  then GR
  else NGE
