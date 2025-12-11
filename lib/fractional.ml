type t =
  | Integer of int
  | Fraction of { numerator : int; denominator : int }
  | Float of float

let int i = Integer i

let frac numerator denominator = Fraction { numerator; denominator }

let float f = Float f

let zero = Integer 0

let compare x y =
  match x,y with
  | Integer x, Integer y -> Int.compare x y
  | Integer _, _ -> -1
  | _, Integer _ -> 1
  | ( Fraction { numerator = nx; denominator = dx }
    , Fraction { numerator = ny; denominator = dy } ) ->
      Pair.compare Int.compare Int.compare (nx, dx) (ny, dy)
  | Fraction _, _ -> -1
  | _, Fraction _ -> 1
  | Float x, Float y -> Float.compare x y

let equal x y = compare x y = 0

let pp fmt =
  let open Format in
  function
  | Integer i -> Format.pp_print_int fmt i
  | Fraction { numerator; denominator } ->
      Format.fprintf fmt "%d/%d" numerator denominator
  | Float f ->
      Format.fprintf fmt "%.2f" f

let to_float = function
  | Integer x -> Float.of_int x
  | Fraction { numerator; denominator } ->
      Float.(of_int numerator /. of_int denominator)
  | Float x -> x

module Op = struct
  let (+) x y =
    match x,y with
    | Integer x, Integer y -> Integer (x + y)
    | x, y -> Float (to_float x +. to_float y)
end
