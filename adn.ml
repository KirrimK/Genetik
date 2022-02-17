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

(* Determine which part of the strand is going to cross over *)
let cross_over = fun strand ->
  let a = Random.int (List.length strand) in
  let b = a + Random.int ((List.length strand) - a + 1) in
  let rec local = fun count in_ start middle end_ ->
    match in_ with
      hd::tl -> if count < a then
                  local (count+1) tl (hd::start) middle end_
                else if count < b then
                  local (count+1) tl start (hd::middle) end_
                else
                  local (count+1) tl start middle (hd::end_)
    | [] -> (List.rev start, List.rev middle, List.rev end_) in
  local 0 strand [] [] [];;

(* Create "children" strands from two strands *)
let meiosis = fun strand_a strand_b ->
  let (sa, ma, ea) = cross_over strand_a in
  let (sb, mb, eb) = cross_over strand_b in
  (* replication, for now is just a simple list copy *)
  (*let original_a = strand_a in
  let original_b = strand_b in*)
  let cross_over_a = List.flatten [sa; mb; ea] in
  let cross_over_b = List.flatten [sb; ma; eb] in
  [strand_a; strand_b; cross_over_a; cross_over_b];;

