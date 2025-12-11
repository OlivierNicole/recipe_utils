let usage_msg = "recettes [-verbose] <file1> [<file2>] ..."
let verbose = ref false
let input_files = ref []

let anon_fun filename =
  input_files := filename::!input_files

let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information")]

let () =
  Arg.parse speclist anon_fun usage_msg;

open Recipe

module Ingredient : sig
  type t

  val mk : name:string -> Amount.t -> t

  val amount : t -> Amount.t
  val name : t -> string
  val of_markdown : Omd.doc -> t
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end = struct
  type t = { amount : Amount.t; name : string }

  let mk ~name amount = { name; amount }

  let amount { amount; _ } = amount
  let name { name; _ } = name

  let of_markdown (doc : Omd.doc) =
    let open Omd in
    match doc with
    | [ Paragraph
          (_, Concat (_, [Emph (_, Text (_, amount)); Text (_, name)]))
      ] ->
        { amount = Parse.f_exn Amount.parser amount; name = String.trim name }
    | [] -> failwith "empty ingredient"
    | _ :: _ -> failwith "More than one paragraph"

  let pp fmt { amount; name } =
    Format.fprintf fmt "%a %s" Amount.pp amount name

  let to_string i =
    Format.(pp str_formatter i);
    Format.flush_str_formatter ()
end

module Recipe : sig
  type t

  val ingredients : t -> Ingredient.t list
  val of_markdown : Omd.doc -> t
end = struct
  type t =
    { name : Omd.(attributes inline);
      ingredients : Ingredient.t list;
    }

  let ingredients { ingredients; _ } = ingredients

  let of_markdown (doc : Omd.doc) =
    let open Omd in
    match doc with
    | Heading (_, 1, name) :: rest -> (
        match rest with
        | Paragraph (_, _desc)
          :: Paragraph (_, _tags)
          :: Paragraph (_, _servings_or_weight)
          :: Thematic_break _
          :: List (_, _, _, ingredients)
          :: Thematic_break _
          :: _
          ->
            { name; ingredients = List.map Ingredient.of_markdown ingredients }
        | _ ->
            failwith "unexpected structure"
      )
    | _ ->
      failwith "First element of the recipe should be a 1-level heading"
end

let compare_ingredient_names i1 i2 =
  String.compare (Ingredient.name i1) (Ingredient.name i2)

let add_amounts i1 i2 =
  (* Assume same ingredient name *)
  Ingredient.(mk ~name:(name i1) @@ Amount.Op.(+) (amount i1) (amount i2))

let warn_on_similar_ingredients sorted =
  let threshold = 3 in
  let rec aux = function
    | [] | [ _ ] -> ()
    | i1 :: (i2 :: _ as rest) ->
        let name = Ingredient.name in
        if String.equal (name i1) (name i2) then
          aux rest
        else
          let dist =
            String.edit_distance ~limit:(threshold + 1) (name i1) (name i2)
          in
          if dist <= threshold then
            Format.eprintf
              "WARNING: ingredients %s and %s seem very close, you may want to \
               harmonize names.\n%!"
              (name i1)
              (name i2);
        aux rest
  in
  aux sorted

let sum_ingredients lists =
  let sorted =
    List.concat lists
    |> List.sort compare_ingredient_names
  in
  warn_on_similar_ingredients sorted;
  sorted
  |> List.to_seq
  |> Seq.group
       (fun i1 i2 ->
         String.equal (Ingredient.name i1) (Ingredient.name i2))
  |> Seq.map
       (fun ingredients_various_amounts ->
         let fst, rest = Seq.uncons ingredients_various_amounts |> Option.get in
         Seq.fold_left add_amounts fst rest
       )
  |> List.of_seq

let () =
  let ingredients =
    !input_files
    |> List.mapi (fun i filename ->
        In_channel.with_open_text filename @@ fun file ->
          let doc = Omd.of_channel file in
          let r = Recipe.of_markdown doc in
          let ingredients = Recipe.ingredients r in
          if !verbose then (
            Format.printf "@[<hov>Ingredient list #%d:@,@[<v 2>@," i;
            List.iter (fun i -> Format.printf "%a@," Ingredient.pp i) ingredients;
            Format.printf "@]@]@,");
          ingredients
        )
        |> sum_ingredients
  in
  Format.printf "@[<hov>Aggregated ingredient list:@,@[<v 2>@,";
  List.iter (fun i -> Format.printf "%a@," Ingredient.pp i) ingredients;
  Format.printf "@."
