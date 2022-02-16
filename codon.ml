(* Codon.ml *)

open Arn;;

type codon = arn_base * arn_base * arn_base;;

let codon_str = fun codon ->
  let (a, b, c) = codon in
  String.concat "" [arn_base_str a; arn_base_str b; arn_base_str c];;

let codon_list_str = fun cdls ->
  String.concat " " (List.map codon_str cdls);;

let arn_strand_to_codon_list = fun strand ->
  let rec prime = fun in_ acc ->
    match in_, acc with
      G::tl, U::(A::_) -> tl
    | hd::tl, _ -> prime tl (hd::acc)
    | [], _ -> [] in
  let rec local = fun in_ acc out ->
    match in_ with
      hd::tl -> begin match acc with
                    [a; b] -> local tl [] ((b, a, hd)::out)
                  | _ -> local tl (hd::acc) out end
    | [] -> out in
  local (prime strand []) [] [];;
