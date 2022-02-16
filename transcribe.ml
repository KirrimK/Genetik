(* Transcribe.ml *)

(* convert Adn to Arn *)
let adn_to_arn_base = fun adn_base ->
  if (Random.float 1.0) > 0.00000001 then
    match adn_base with
      Adn.A -> Arn.A
    | Adn.T -> Arn.U
    | Adn.C -> Arn.C
    | Adn.G -> Arn.G
  else
    match Random.int 4 with
      0 -> A
    | 1 -> U
    | 2 -> C
    | _ -> G;;

let arn_to_adn_base = fun adn_base ->
  if (Random.float 1.0) > 0.0001 then
    match adn_base with
      Arn.A -> Adn.A
    | Arn.U -> Adn.T
    | Arn.C -> Adn.C
    | Arn.G -> Adn.G
  else
    match Random.int 4 with
      0 -> A
    | 1 -> T
    | 2 -> C
    | _ -> G;;
  
let transcribe = List.map adn_to_arn_base;;

let reverse_transcribe = List.map arn_to_adn_base;;
