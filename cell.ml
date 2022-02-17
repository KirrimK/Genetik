(* Cell.ml *)

open Adn;;
open Arn;;
open Codon;;
open Amino;;

(* If the mode is 4+ (random), determines a normal mode at random *)
let rec det_mode = fun mode ->
  match mode with
    0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | _ -> det_mode (Random.int 4);;

type stack_content =
  Int of int
| Flag_if
| Flag_while_goback
| Flag_while_overfly
| Flag_ignore;;

(* Stack content to string *)
let stct_str = fun stct ->
  match stct with
    Int i -> string_of_int i
  | Flag_if -> "if"
  | Flag_while_goback -> "wgb"
  | Flag_while_overfly -> "wof"
  | Flag_ignore -> "ign";;

let stack_str = fun stack ->
  String.concat " <- " (List.map stct_str stack);;

(* Code to get one char from keypress in terminal
   copied from https://stackoverflow.com/a/13410456 *)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_echo = false } in
  let res = input_char Stdlib.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

(* Runs the code in the provided strand
 step: true: cell will function step-by-step
       false: it will not
 interactive:
       true: the cell should interpret the Asn instruction
       false: the cell should ignore the Asn instruction
 mode: 0 for standard
       1 for standard reversed
       2 for twin
       3 for twin reversed
       _ for random
 adn_strand:
       the adn strand with the code to run *)
let run_cell = fun step interactive mode adn_strand in_char_ls ->
  let run_adn =
    match det_mode mode with
      0 -> adn_strand
    | 1 -> List.rev adn_strand
    | 2 -> adn_twin adn_strand
    | _ -> List.rev (adn_twin adn_strand) in
  let run_arn = transcribe run_adn in
  let run_codons = arn_strand_to_codon_list run_arn in
  let run_aminos = List.rev (codon_ls_to_amino_ls run_codons) in
  match run_aminos with
    [] (* No viable code to execute, cell returns *) ->
     (false, "")
  | _::_ (* Sort-of viable code was found *) ->
     let amls_length = List.length run_aminos in
     let rec eval = fun ptr active other out in_->
       try
         if ptr < 0 || ptr >= amls_length then
           (false, out)
         else
           let () = (* step debug mode *)
             if step then
               let _ = read_line () in
               Printf.printf "(ptr: %d (%s)\nactive: %s\nother: %s)\n" ptr (amino_str (List.nth run_aminos ptr)) (stack_str active) (stack_str other)
             else
               () in
           begin match active with
             Flag_if::tl -> begin match List.nth run_aminos ptr with
                              Pro -> eval (ptr+1) tl other out in_
                            | Met -> eval (ptr+1) (Flag_if::active) other out in_
                            | _ -> eval (ptr+1) active other out in_ end
           | Flag_while_goback::(Flag_while_goback::tl) -> begin match List.nth run_aminos ptr with
                                                             Thr -> eval (ptr-1) tl other out in_
                                                           | _ -> eval (ptr-1) active other out in_ end
           | Flag_while_goback::tl -> begin match List.nth run_aminos ptr with
                                        Thr -> eval ptr tl other out in_
                                      | _ -> eval (ptr-1) active other out in_ end
           | Flag_while_overfly::tl -> begin match List.nth run_aminos ptr with
                                         Ala  -> eval (ptr+1) tl other out in_
                                       | Thr -> eval (ptr+1) (Flag_while_overfly::active) other out in_
                                       | _ -> eval (ptr+1) active other out in_ end
           | Flag_ignore::tl -> begin match List.nth run_aminos ptr with
                                  Met -> eval (ptr+1) tl other out in_
                                | _ -> eval (ptr+1) active other out in_ end
           | _ -> begin match List.nth run_aminos ptr with
                    Phe (* Push 0 onto active stack *) ->
                     eval (ptr+1) (Int(0)::active) other out in_
                  | Leu (* Discard top of active stack *) ->
                     let new_stack = begin match active with
                                       _::tl -> tl
                                     | [] -> [] end in
                     eval (ptr+1) new_stack other out in_
                  | Ile (* Switch active stack *) ->
                     eval (ptr+1) other active out in_
                  | Met (* If was ignoring everything, resume normal functioning *) ->
                     eval (ptr+1) active other out in_
                  | Val (* Swap the two upper elements of active stack *) ->
                     let new_stack = begin match active with
                                       a::(b::tl) -> b::(a::tl)
                                     | other -> other end in
                     eval (ptr+1) new_stack other out in_
                  | Ser (* If top of active stack is 0, jump to corresponding Pro *) ->
                     begin match active with
                       Int(0)::_ -> eval (ptr+1) (Flag_if::active) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Pro (* End of conditional block *) ->
                     eval (ptr+1) active other out in_
                  | Thr (* Start of while top of active stack not zero *) ->
                     begin match active with
                       Int(0)::_ -> eval (ptr+1) (Flag_while_overfly::active) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Ala (* End of corresponding while block *) ->
                     eval (ptr-1) (Flag_while_goback::active) other out in_
                  | Tyr (* Ignores everything until Met *) ->
                     eval (ptr+1) (Flag_ignore::active) other out in_
                  | Stop (* Stops execution of amino list *) ->
                     (true, out)
                  | His (* Increment top of active stack by one *) ->
                     begin match active with
                       Int(i)::tl -> eval (ptr+1) (Int(i+1)::tl) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Gln (* Decrement top of active stack by one *) ->
                     begin match active with
                       Int(i)::tl -> eval (ptr+1) (Int(i-1)::tl) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Asn (* if interactive mode, take a keypress from user as ascii char and push to stack
                           if not, take an ascii char from stdin and push to stack *) ->
                     if interactive then
                       let got_char = get1char () in
                       eval (ptr+1) (Int(Char.code got_char)::active) other out in_
                     else
                       begin match in_ with
                         ch::tl -> eval (ptr+1) (Int(Char.code ch)::active) other out tl
                       | _ -> eval (ptr+1) (Int(0)::active) other out in_ end
                  | Lys (* Put top of active stack as char on stdout *) ->
                     begin match active with
                       Int(i)::_ ->
                        let this_out = if 0 <= i && i <= 255 then
                                         Printf.sprintf "%c" (Char.chr i)
                                       else
                                         Printf.sprintf " " in
                        if interactive then
                          Printf.printf "%s%!" this_out;
                        eval (ptr+1) active other (String.cat out this_out) in_
                       | _ -> eval (ptr+1) active other out in_ end
                  | Asp (* Duplicates top of active stack *) ->
                     begin match active with
                       Int(i)::_ -> eval (ptr+1) (Int(i)::active) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Glu (* Sends top of active stack to top of other stack *) ->
                     begin match active with
                       Int(i)::_ -> eval (ptr+1) active (Int(i)::other) out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Cys (* Add second-most top of active stack to top of same stack,
                         if only one element then does nothing *) ->
                     begin match active with
                       Int(i)::(Int(j)::tl) -> eval (ptr+1) (Int(i+j)::(Int(j)::tl)) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Trp (* Substracts second-most top of active stack to top of same stack,
                         if only one element then does nothing *) ->
                     begin match active with
                       Int(i)::(Int(j)::tl) -> eval (ptr+1) (Int(i-j)::(Int(j)::tl)) other out in_
                     | _ -> eval (ptr+1) active other out in_ end
                  | Arg (* Outputs the top of stack as int on stdout *) ->
                     begin match active with
                       Int(i)::_ ->
                        let this_out = Printf.sprintf "%d" i in
                        if interactive then
                          Printf.printf "%s%!" this_out;
                        eval (ptr+1) active other (String.cat out this_out) in_
                       | _ -> eval (ptr+1) active other out in_ end
                  | Gly (* Unimplemented *) ->
                     eval (ptr+1) active other out in_ end end
       with _ -> (false, out) in
     eval 0 [] [] "" in_char_ls;;

               
