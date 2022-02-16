(* ADN.ml *)

type adn_base =
  A
| T
| C
| G;;

let adn_pair = fun adn_base ->
  match adn_base with
    A -> T
  | T -> A
  | C -> G
  | G -> C;;

let adn_base_str = fun adn_base ->
  match adn_base with
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G";;

let adn_base_from_char = fun char ->
  match char with
    'A'
  | 'a' -> A
  | 'T'
    | 't' -> T
  | 'C'
    | 'c' -> C
  | 'G'
    | 'g' -> G
  | _ -> failwith "incorrect";;
           
type adn_strand = adn_base list;;

let adn_twin = fun adn_strand ->
  List.map adn_pair adn_strand;;

let adn_strand_str = fun adn_strand ->
  String.concat "" (List.map adn_base_str adn_strand);;
