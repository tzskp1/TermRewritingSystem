open Orders

let ord n m =
  if n > m
  then GR
  else if n = m
  then EQ
  else NGE

let mul = mul' ord

let m = [(1,1)]
let n = [(1,2)]

let test m n =
  match mul (m,n) with
  | GR -> print_endline "GR"
  | EQ -> print_endline "EQ"
  | NGE -> print_endline "NGE"

let () = test m n

