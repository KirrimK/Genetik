(* Arn.ml *)

type arn_base =
  A
| U
| C
| G;;

let arn_pair = fun arn_base ->
  match arn_base with
    A -> U
  | U -> A
  | C -> G
  | G -> C;;

let arn_base_str = fun arn_base ->
  match arn_base with
  | A -> "A"
  | U -> "U"
  | C -> "C"
  | G -> "G";;

type arn_strand = arn_base list;;

let arn_twin = fun arn_strand ->
  List.map arn_pair arn_strand;;

let arn_strand_str = fun arn_strand ->
  String.concat "" (List.map arn_base_str arn_strand);;
