(* Amino.ml *)

open Arn;;

let debug = ref false;;

type amino =
  Phe (* Push 0 onto active stack *)
| Leu (* Discard top of active stack *)
| Ile (* Switch active stack *)
| Met (* If was ignoring everything, resume normal functioning *)
| Val (* Swap the two upper elements of active stack *)
| Ser (* If top of active stack is 0, jump to corresponding Pro *)
| Pro (* End of conditional block *)
| Thr (* Start of while top of active stack not zero *)
| Ala (* End of corresponding while block *)
| Tyr (* Ignores everything until Met *)
| Stop (* Stops execution of amino list *)
| His (* Increment top of active stack by one *)
| Gln (* Decrement top of active stack by one *)
| Asn (* Get char as int from stdin on top of active stack, can be deactivated in no-interaction mode *)
| Lys (* Put top of active stack as char on stdout *)
| Asp (* Duplicates top of active stack *)
| Glu (* Sends top of active stack to top of other stack *)
| Cys (* Add second-most top of active stack to top of same stack, if only one element then does nothing *)
| Trp (* Substracts second-most top of active stack to top of same stack, if only one element then does nothing *)
| Arg (* Outputs the top of stack as int on stdout *)
| Gly (* Unimplemented *);;

let amino_str = fun amino ->
  match amino with
    Phe -> "Phe"
  | Leu -> "Leu"
  | Ile -> "Ile"
  | Met -> "Met"
  | Val -> "Val"
  | Ser -> "Ser"
  | Pro -> "Pro"
  | Thr -> "Thr"
  | Ala -> "Ala"
  | Tyr -> "Tyr"
  | Stop -> "Stop"
  | His -> "His"
  | Gln -> "Gln"
  | Asn -> "Asn"
  | Lys -> "Lys"
  | Asp -> "Asp"
  | Glu -> "Glu"
  | Cys -> "Cys"
  | Trp -> "Trp"
  | Arg -> "Arg"
  | Gly -> "Gly";;

let amino_ls_str = fun amino_ls ->
  String.concat " " (List.map amino_str amino_ls);;

let codon_to_amino = fun codon ->
  match codon with
    (U, U, U)
  | (U, U, C) -> Phe
  | (U, U, _)
    | (C, U, _) -> Leu
  | (A, U, G) -> Met
  | (A, U, _) -> Ile
  | (G, U, _) -> Val
  | (U, C, _) -> Ser
  | (C, C, _) -> Pro
  | (A, C, _) -> Thr
  | (G, C, _) -> Ala
  | (U, A, U)
    | (U, A, C) -> Tyr
  | (U, A, _) -> Stop
  | (C, A, U)
    | (C, A, C) -> His
  | (C, A, _) -> Gln
  | (A, A, U)
    | (A, A, C) -> Asn
  | (A, A, _) -> Lys
  | (G, A, A)
    | (G, A, G) -> Glu
  | (G, A, _) -> Asp
  | (U, G, U)
    | (U, G, C) -> Cys
  | (U, G, A) -> Stop
  | (U, G, G) -> Trp
  | (C, G, _) -> Arg
  | (A, G, U)
    | (A, G, C) -> Ser
  | (A, G, _) -> Arg
  | (G, G, _) -> Gly;;

let codon_ls_to_amino_ls = List.map codon_to_amino;;
