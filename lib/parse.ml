open Angstrom

let f_exn parser str =
  let open Angstrom.Buffered in
  let state = parse parser  in
  let state = feed state (`String str) in
  let state = feed state `Eof in
  match state with
  | Partial _ -> assert false
  | Done ({ off; len; _ }, v) ->
      if not (Int.equal len 0) then
        failwith @@ String.concat ""
          [ Printf.sprintf
             "Parser finished with unconsumed = { off = %d; len = %d; _ }"
             off
             len
          ]
      else
        v
  | Fail ({ off; len; _ }, strs, str) ->
      (* ??? *)
      failwith @@ String.concat ""
        ([ Printf.sprintf
             "Parse error: unconsumed = { off = %d; len = %d; _ }\n\
              Error string list:\n"
             off
             len
         ]
         @ List.map
             (fun s -> String.concat "" [ "  "; s; "\n" ])
             strs
         @ [ "Error string:\n  "; str ]
        )
