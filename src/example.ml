(** Example using the SO library to give a determination on some
second-order formulae *)

open SO
open SoOps

let formulas = [|
    "∃ X:1 . ∀ y . X(y)" ;
    "∀ X:1 . ∀ y . X(y)" ;
    "∃ X:1 . ∀ y . (X(y) ∧ ∃ z . ¬ y = z)" ;
  |]

let mk_structure i =
  { size = i; relations = RelMap.empty}

let s = mk_structure 1

;;

print_endline 
  (Format.sprintf 
    "Checking satisfiability in a structure of size %i" (size_of s))

;;

for i = 0 to 2 do
  let f = formulas.(i) in
  print_endline (Format.sprintf "\t%s: %b" f (model_check s (parse f)))
done

;;

let s = mk_structure 5

;;

print_endline 
  (Format.sprintf 
    "Checking satisfiability in a structure of size %i" (size_of s))

;;

for i = 0 to 2 do
  let f = formulas.(i) in
  print_endline (Format.sprintf "\t%s: %b" f (model_check s (parse f)))
done

;;
