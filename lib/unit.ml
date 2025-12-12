type dimension = Mass | Volume | None | Unknown

let dimension_equal : dimension -> dimension -> bool = Stdlib.(=)

type t = { name : string; dimension : dimension; factor_to_SI : float }

let name { name; _ } = name

let none = { name = "none"; dimension = None; factor_to_SI = 1.0 }

let mul ~name factor { dimension; factor_to_SI = f; _ } =
  { name; dimension; factor_to_SI = factor *. f }

module StringTbl = Hashtbl.Make(String)

let tbl = StringTbl.create 42

let register ({ name; _ } as t) =
  StringTbl.add tbl name t;
  t

let g = register { name = "g"; dimension = Mass; factor_to_SI = 1e-3 }

let ml = register { name = "ml"; dimension = Volume; factor_to_SI = 1e-6 }

let cc = register @@ mul ~name:"cc" 5. ml

let cs = register @@ mul ~name:"cs" 3. cc

let of_string_exn name =
  try StringTbl.find tbl name
  with Not_found -> { name; dimension = Unknown; factor_to_SI = 1.0 }

let equal u1 u2 =
  String.equal u1.name u2.name
  && u1.dimension = u2.dimension
  && Float.equal u1.factor_to_SI u2.factor_to_SI

let compare u1 u2 =
  match (compare : dimension -> dimension -> int) u1.dimension u2.dimension with
  | n when n < 0 -> n
  | n when n > 0 -> n
  | _ (* 0 *) ->
      (match String.compare u1.name u2.name with
      | n when n < 0 -> n
      | n when n > 0 -> n
      | _ (* 0 *) ->
          Float.compare u1.factor_to_SI u2.factor_to_SI
      )

let pp fmt u =
  let open Format in
  if u == none then
    pp_print_string fmt "(none)"
  else
    pp_print_string fmt u.name

let pp_quantity ~quantity fmt u =
  let open Format in
  if u == none then
    Fractional.pp fmt quantity
  else
    fprintf fmt "%a %s" Fractional.pp quantity u.name

let add q1 u1 q2 u2 =
  if not (dimension_equal u1.dimension u2.dimension) then
    failwith @@
      Format.asprintf
        "Cannot add non-commensurable units %a and %a" pp u1 pp u2;
  if equal u1 u2 then
    Fractional.Op.(q1 + q2), u1
  else if dimension_equal u1.dimension Unknown
          || dimension_equal u2.dimension Unknown then
    failwith @@
      Format.asprintf
        "Cannot add units %a and %a (unknown dimensionality)"
        pp u1
        pp u2
  else
    let quantity =
      let open Fractional in
      to_float q1 +. u2.factor_to_SI /. u1.factor_to_SI *. to_float q2
    in
    let quantity = Fractional.float quantity in
    quantity, u1
