(** Example using the SO library to give a determination on some
second-order formulae *)

let example_sat =
  let structure = SO.{ size = 5; relations = RelMap.empty } in
  let formula =
    SO.(
      let x = mk_fresh_sv () in
      let y = mk_fresh_fv () in
      SoAny (x, 1,
             FoAll (y, QRel (x, [Var y]))
            )
    )
  in
  structure, formula

let example_unsat =
  let structure = SO.{ size = 5; relations = RelMap.empty } in
  let formula =
    SO.(
      let x = mk_fresh_sv () in
      let y = mk_fresh_fv () in
      SoAll (x, 1,
             FoAll (y, QRel (x, [Var y]))
            )
    )
  in
  structure, formula
  
  
let () =
  let s, f = example_sat in
  let determination = SoOps.model_check s f in
  Printf.printf "Example Satisfiable Models: %b\n" determination;
  let s, f = example_unsat in
  let determination = SoOps.model_check s f in
  Printf.printf "Example Unsatisfiable Models: %b\n" determination
  
