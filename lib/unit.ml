type dimension = Mass | Volume | None | Unknown

type t = { name : string; dimension : dimension; factor_from_SI : float }

let name { name; _ } = name

let none = { name = "none"; dimension = None; factor_from_SI = 1.0 }

let g = { name = "g"; dimension = Mass; factor_from_SI = 1e-3 }

let of_string_exn =
  function
  | "g" -> g
  | name -> { name; dimension = Unknown; factor_from_SI = 1.0 }

let equal u1 u2 =
  String.equal u1.name u2.name
  && u1.dimension = u2.dimension
  && Float.equal u1.factor_from_SI u2.factor_from_SI

let compare u1 u2 =
  match (compare : dimension -> dimension -> int) u1.dimension u2.dimension with
  | n when n < 0 -> n
  | n when n > 0 -> n
  | _ (* 0 *) ->
      (match String.compare u1.name u2.name with
      | n when n < 0 -> n
      | n when n > 0 -> n
      | _ (* 0 *) ->
          Float.compare u1.factor_from_SI u2.factor_from_SI
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
