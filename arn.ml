(* Arn.ml *)

type arn_base =
  A
| U
| C
| G;;

let arn_base_str = fun arn_base ->
  match arn_base with
  | A -> "A"
  | U -> "U"
  | C -> "C"
  | G -> "G";;

type arn_strand = arn_base list;;

let arn_strand_str = fun arn_strand ->
  String.concat "" (List.map arn_base_str arn_strand);;

(* Convert Adn to Arn *)
let adn_to_arn_base = fun adn_base ->
  if (Random.float 1.0) > 0.00000001 then
    match adn_base with
      Adn.A -> A
    | Adn.T -> U
    | Adn.C -> C
    | Adn.G -> G
  else
    match Random.int 4 with
      0 -> A
    | 1 -> U
    | 2 -> C
    | _ -> G;;

(* Convert Arn back to Adn *)
let arn_to_adn_base = fun adn_base ->
  if (Random.float 1.0) > 0.0001 then
    match adn_base with
      A -> Adn.A
    | U -> Adn.T
    | C -> Adn.C
    | G -> Adn.G
  else
    match Random.int 4 with
      0 -> A
    | 1 -> T
    | 2 -> C
    | _ -> G;;

let transcribe = List.map adn_to_arn_base;;

let reverse_transcribe = List.map arn_to_adn_base;;
