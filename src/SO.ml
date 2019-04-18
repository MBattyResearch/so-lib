module U = Util

type element = int
  [@@deriving show]
type fo_var = F of string
  [@@deriving show]

(* Variables with same name but different arities *do* shadow each-other. *)
type so_var = S of string
  [@@deriving show]
type rel_sym = string
  [@@deriving show] (* same as so_var *)

type relation = int * int list list
  [@@deriving show]

module RelMap = Map.Make (String)

type structure =
  { size : int
  ; relations : relation RelMap.t }

let size_of { size; _ } = size

(* Special interpreted relations. See [SoOps.add_specials] for how to add them
to the structure. *)
let eq_rel = "(EQ)"

let specials = [eq_rel]

type term =
  | Var of fo_var
  | Const of element
  [@@deriving show]

type formula =
  | CRel of rel_sym * term list
  | QRel of so_var * term list
  | FoAny of fo_var * formula
  | FoAll of fo_var * formula
  | SoAny of so_var * int * formula
  | SoAll of so_var * int * formula
  | And of formula list
  | Or of formula list
  | Not of formula
  [@@deriving show]

let show_fo_var (F n) = n
let show_so_var (S n) = n

let show_isabelle_fo_var (F n) = n
let show_isabelle_so_var (S n) = n

                      
let show_term = function
    Var (F n) -> n
  | Const e -> string_of_int e

let show_isabelle_term = function
    Var (F n) -> Format.sprintf "%s" n
  | Const e -> Format.sprintf "U_%d" e
            
let show_eq_rel rel ts =
  Format.sprintf "(%s)" (U.map_join " = " show_term ts)

let show_isabelle_eq_rel rel ts =
  Format.sprintf "(%s)" (U.map_join " = " show_isabelle_term ts)
  
let show_rel rel ts =
  Format.sprintf "%s(%s)" rel (U.map_join ", " show_term ts)

let show_isabelle_rel rel ts =
  Format.sprintf "(%s %s)" rel (U.map_join " " show_isabelle_term ts)

(* We could provide an interface for show_special here, but as there's
   only one for now I've not bothered - sjc *)
let show_special_crel rel =
  if rel = eq_rel then
    show_eq_rel rel
  else
    show_rel rel

let show_isabelle_special_crel rel =
  if rel = eq_rel then
    show_isabelle_eq_rel rel
  else
    show_isabelle_rel rel
  
(* TODO *)
let rec show_formula = function
  | CRel (rel,ts) ->
    (show_special_crel rel) ts
  | QRel (rel, ts) ->
     Format.sprintf "%s(%s)" (show_so_var rel) (U.map_join ", " show_term ts)
  | FoAll (var, f) ->
     Format.sprintf "(!%s . (%s))" (show_fo_var var) (show_formula f)
  | FoAny (var, f) ->
     Format.sprintf "(?%s . (%s))" (show_fo_var var) (show_formula f)
  | SoAll (var, a, f) ->
     Format.sprintf "(!%s:%d . (%s))" (show_so_var var) a (show_formula f)
  | SoAny (var, a, f) ->
     Format.sprintf "(?%s:%d . (%s))" (show_so_var var) a (show_formula f)
  | And fs ->
     Format.sprintf "(%s)" (U.map_join " & " show_formula fs)
  | Or fs ->
     Format.sprintf "(%s)" (U.map_join " | " show_formula fs)
  | Not f ->
     Format.sprintf "~(%s)" (show_formula f)

let tannot a =
  Format.sprintf "%s ⇒ bool" (String.concat " ⇒ " (U.repeat a "univ"))
    
let rec show_isabelle_formula = function
  | CRel (rel,ts) ->
    (show_isabelle_special_crel rel) ts
  | QRel (rel, ts) ->
     Format.sprintf "(%s %s)" (show_isabelle_so_var rel) (U.map_join " " show_term ts)
  | FoAll (var, f) ->
     Format.sprintf "(∀ %s . (%s))" (show_isabelle_fo_var var) (show_isabelle_formula f)
  | FoAny (var, f) ->
     Format.sprintf "(∃ %s . (%s))" (show_isabelle_fo_var var) (show_isabelle_formula f)
  | SoAll (var, a, f) ->
     Format.sprintf "(∀ (%s::%s) . (%s))"
                    (show_isabelle_so_var var) (tannot a) (show_isabelle_formula f)
  | SoAny (var, a, f) ->
     Format.sprintf "(∃ (%s::%s) . (%s))"
                    (show_isabelle_so_var var) (tannot a) (show_isabelle_formula f)
  | And fs ->
     Format.sprintf "(%s)" (U.map_join " ∧ " show_isabelle_formula fs)
  | Or fs ->
     Format.sprintf "(%s)" (U.map_join " ∨ " show_isabelle_formula fs)
  | Not f ->
     Format.sprintf "¬(%s)" (show_isabelle_formula f)

let show_isabelle_formula f =
  let body = show_isabelle_formula f in
  Format.sprintf ("lemma \"%s\"\n  nitpick\n  sorry\n") body
    
let show_structure s =
  (* Turn a list of lists into a nice-readable format of tuples *)
  (* [[1;2;3]; [4;5;6]] -> { (1 2 3) (4 5 6) } *)
  let f key (a, vals) acc =
    if List.mem key specials
    then acc
    else
      let v = List.map (U.map_join " " string_of_int) vals in
      let v = List.map (Printf.sprintf "(%s)") v in
      let vs = String.concat " " v in
      (Format.sprintf "\t\t%s:%d := { %s }\n" key a vs) ^ acc
  in
  let m = RelMap.fold f s.relations "" in
  Format.sprintf "{\n\t.size: 1..%d\n\t.relations:\n%s}\n" (s.size) m

let show_isabelle_structure theory s =
  let univ = String.concat " | " (List.map (Format.sprintf "U_%d") (U.range 1 s.size)) in
  let f key (a, vals) acc =
    if List.mem key specials
    then acc
    else
      let v = List.map (U.map_join ", " (Format.sprintf "U_%d")) vals in
      let v = List.map (Printf.sprintf "(%s)") v in
      let vs = Printf.sprintf "{%s}" (String.concat ", " v) in
      let args = U.map_join " " (Format.sprintf "X_%d") (U.range 1 a) in
      let tuple = U.map_join "," (Format.sprintf "X_%d") (U.range 1 a) in
      let vs = Printf.sprintf "\"%s %s = ((%s) ∈ %s)\"" key args tuple vs in
      (Format.sprintf "fun %s :: \"%s\" where\n  %s\n" key (tannot a) vs) ^ acc
  in
  let m = RelMap.fold f s.relations "" in
  let prefix = Format.sprintf
                 "theory \"%s\"\n  imports Main\nbegin\nnitpick_params[timeout=3600]\n"
                 (Filename.basename theory)
  in
  (Format.sprintf "%s\ndatatype univ = %s\n" prefix univ) ^ m

  
let pp_structure fmt s =
  Format.fprintf fmt "%s" (show_structure s)

let pp_formula fmt f =
  Format.fprintf fmt "%s" (show_formula f)

let id = ref 0
let mk_fresh_fv =
  fun ?(prefix = "F") () -> incr id; F (Printf.sprintf "%s%d" prefix !id)
let mk_fresh_sv =
  fun ?(prefix = "S") () -> incr id; S (Printf.sprintf "%s%d" prefix !id)

let digit  = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z']

type token = 
  | DOT | COLON | COMMA | LPAREN | RPAREN
  (* | TRUE | FALSE *)
  | NUM of int | FO of string | SO of string
  | EQUALS
  | FORALL | EXISTS | NEG | CONJ | DISJ 

let rec next_token buf =
  match%sedlex buf with
  | '.' -> Some DOT
  | ',' -> Some COMMA
  | ':' -> Some COLON
  | '(' -> Some LPAREN
  | ')' -> Some RPAREN
  | 0x2200 | '!' -> Some FORALL
  | 0x2203 | '?' -> Some EXISTS
  | 0x00AC | '~' -> Some NEG
  | 0x2227 | "/\\" -> Some CONJ
  | 0x2228 | "\\/" -> Some DISJ
  (* | 0x22A4 | "TRUE" -> Some TRUE
  | 0x22A5 | "FALSE" -> Some FALSE *)
  | number ->
    Some (NUM (int_of_string (Sedlexing.Latin1.lexeme buf)))
  | 'a'..'z', Star (letter | digit) ->
     Some (FO (Sedlexing.Latin1.lexeme buf))
  | 'A'..'Z', Star (letter | digit) ->
     Some (SO (Sedlexing.Latin1.lexeme buf))
  | '=' -> Some EQUALS
  | Plus white_space -> next_token buf
  | eof -> None
  | _ -> 
    failwith 
      (Format.sprintf 
        "Lexing error at position %i" (Sedlexing.lexeme_start buf))

let is_num =
  function
  | NUM _ -> true
  | _ -> false

let is_fo =
  function
  | FO _ -> true
  | _ -> false

let is_term tok = (is_num tok) || (is_fo tok) 

let is_so =
  function
  | SO _ -> true
  | _ -> false

let is_id tok = (is_fo tok) || (is_so tok)

let dest_num =
  function
  | NUM i -> i
  | _ -> failwith "Not a number!"

let dest_term =
  function
  | NUM i -> Const i
  | FO v -> Var (F v)
  | _ -> failwith "Not a term!"

let dest_id = 
  function
  | FO v -> `F v
  | SO v -> `S v
  | _ -> failwith "Not an identifier!"

let dest_so_id =
  function
  | SO v -> v
  | _ -> failwith "Not a second order identifier!"
  
open Opal

let parse_term =
  ((satisfy is_term)) >>= (dest_term % return)

let parse_term_list =
  sep_by parse_term (exactly COMMA)

let parse_equality =
  parse_term >>= (fun t ->
  (exactly EQUALS) >>
  parse_term >>= (fun t' -> 
  return (CRel (eq_rel, [t; t']))))

let parens p =
  between (exactly LPAREN) (exactly RPAREN) p

let parse_atomic scope =
  (satisfy is_so) >>= (dest_so_id % (fun id ->
  (parens parse_term_list) >>= (fun ts ->
  if List.exists (String.equal id) scope then
    return (CRel (id, ts))
  else
    return (QRel ((S id), ts)))))

let parse_formula =
  let rec parse_formula scope input =
    ( (parse_equality)
        <|>
      (parse_atomic scope)
        <|>
      ((exactly NEG) >> (parse_formula scope) >>= (fun f -> return (Not f)))
        <|>
      ((exactly FORALL) >> (satisfy is_id) >>= (dest_id % (function
        | `F v ->
          (exactly DOT) >> 
          (parse_formula (v::scope)) >>= (fun f ->
          (return (FoAll (F v, f))))
        | `S v ->
          (exactly COLON) >>
          (satisfy is_num) >>= (dest_num % (fun arity ->
          (exactly DOT) >> 
          (parse_formula (v::scope)) >>= (fun f ->
          (return (SoAll (S v, arity, f)))))))))
        <|>
      ((exactly EXISTS) >> (satisfy is_id) >>= (dest_id % (function
        | `F v ->
          (exactly DOT) >> 
          (parse_formula (v::scope)) >>= (fun f ->
          (return (FoAny (F v, f))))
        | `S v ->
          (exactly COLON) >>
          (satisfy is_num) >>= (dest_num % (fun arity ->
          (exactly DOT) >> 
          (parse_formula (v::scope)) >>= (fun f ->
          (return (SoAny (S v, arity, f)))))))))
        <|>
      (parens (sep_by1 (parse_formula scope) (exactly CONJ)) >>= (fun fs ->
      return (And fs)))
        <|>
      (parens (sep_by1 (parse_formula scope) (exactly DISJ)) >>= (fun fs ->
      return (Or fs)))
    ) input in
  parse_formula []

let parse str =
  let buf = Sedlexing.Utf8.from_string str in
  let input = Opal.LazyStream.of_function (fun () -> next_token buf) in
  let parser = parse_formula >>= (fun f -> eof f) in
  match parse parser input with
  | Some f ->
    f
  | None ->
    failwith "Parsing Error!"