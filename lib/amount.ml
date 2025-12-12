type t = { quantity : Fractional.t; unit : Unit.t }

let quantity { quantity; _ } = quantity
let unit { unit; _ } = unit

let nothing = { unit = Unit.none; quantity = Fractional.zero }

let is_nothing { unit; quantity } =
  Unit.equal unit Unit.none && Fractional.(equal quantity zero)

let equal t1 t2 =
  Unit.equal t1.unit t2.unit && Fractional.equal t1.quantity t2.quantity

let compare t1 t2 =
  match Unit.compare t1.unit t2.unit with
  | n when n < 0 -> n
  | n when n > 0 -> n
  | _ (* 0 *) ->
      Fractional.compare t1.quantity t2.quantity

open struct
  open Angstrom

  let parse_fractional str =
    let res = ref 0. in
    let decimal_index = ref None in
    for i = 0 to String.length str - 1 do
      if Char.equal str.[i] '.' || Char.equal str.[i] ',' then
        match !decimal_index with
        | None -> decimal_index := Some i
        | Some _ -> failwith ("two decimal points in number " ^ str)
      else if str.[i] >= '0' && str.[i] <= '9' then
        res := 10. *. !res +. Float.of_int (Char.code str.[i] - Char.code '0')
      else
        failwith "unexpected character - not in [0-9,.]"
    done;
    match !decimal_index with
    | None -> !res
    | Some idx ->
        for _ = String.length str - 2 downto idx do
          res := !res /. 10.
        done;
        !res

  let is_digit = function '0'..'9' -> true | _ -> false

  let int =
    take_while1 is_digit >>| int_of_string

  let pow x n =
    let rec aux x = function
      | 0 -> 1.
      | 1 -> x
      | n when n mod 2 = 0 -> aux (x *. x) (n / 2)
      | n -> x *. aux (x *. x) (n / 2)
    in
    aux x n

  let decimal =
    let* int_part = int in
    let* decimals =
      (skip (function '.' | ',' -> true | _ -> false)
       *> take_while is_digit)
      <|> string ""
    in
    return @@
      match decimals with
      | "" -> Fractional.int int_part
      | _ ->
          Fractional.float @@
            Float.of_int int_part
            +. Float.of_string decimals /. pow 10. (String.length decimals)

  let fraction =
    let* a = int in
    let* _ = char '/' in
    let* b = int in
    return (Fractional.frac a b)

  let fractional =
    choice
      ~failure_msg:"Expected decimal number or fraction"
      [ fraction; decimal ]
end


let parser =
  let open Angstrom in
  let* quantity = fractional in
  let* _ = take_while (function ' ' -> true | _ -> false) in
  let* unit =
    take_while (Fun.const true) >>= fun unit ->
      match unit with
      | "" -> return Unit.none
      | _ ->
          try return (Unit.of_string_exn unit)
          with Failure msg -> fail ("Error parsing unit: " ^ msg)
  in
  return { quantity; unit }

let pp fmt { quantity; unit } =
  Format.fprintf fmt "%a" (Unit.pp_quantity ~quantity) unit

module Op = struct
  let (+)
      ({ quantity = q1; unit = unit1 } as a1)
      ({ quantity = q2; unit = unit2 } as a2) =
    (* If one of the operands is nothing, then the result is always defined *)
    if is_nothing a1 then
      a2
    else if is_nothing a2 then
      a1
    else
      let quantity, unit = Unit.add q1 unit1 q2 unit2 in
      { quantity; unit }
end
