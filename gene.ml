(* Gene.ml *)
open Adn;;
open Cell;;

let usage_msg = "gene.exe [-d] [-i] [-m<s,sr,t,tr>] [<file>]";;
let input_files = ref [];;
let is_repl = ref true;;
let is_step = ref false;;
let run_mode = ref 4;;
let is_interactive = ref false;;
let is_shut = ref false;;

(* TODO: Try to fix problem with shebangs that prevent from using more than one flag,
   and no flag with arguments *)

let has_file = fun filename ->
  input_files := filename::!input_files;
  is_repl := false;;

let speclist =
  [("-d", Arg.Set is_step, "Start the interpreter in step by step / debug mode");
   ("-mn", Arg.Unit (fun () -> run_mode := 0), "Set the cell execution mode to normal");
   ("-mnr", Arg.Unit (fun () -> run_mode := 1), "Set the cell execution mode to normal reverse");
   ("-mt", Arg.Unit (fun () -> run_mode := 2), "Set the cell execution mode to twin");
   ("-mtr", Arg.Unit (fun () -> run_mode := 3), "Set the cell execution mode to twin reverse");
   ("-u", Arg.Set is_interactive, "Allow the interpreter to ask for user input");
   ("-s", Arg.Set is_shut, "No input at all (neither stdin nor user input")];;

(* Add other modes, like population mode, or merge mode *)

let rec exec = fun in_ ->
  try
    let strand = if !is_repl then
                   let () = Printf.printf "\n> " in
                   let char_list = List.of_seq (String.to_seq (read_line ())) in
                   List.map adn_base_from_char char_list
                 else
                   let ic = open_in (List.hd (List.rev !input_files)) in
                   let file = really_input_string ic (in_channel_length ic) in
                   let char_list = List.of_seq (String.to_seq file) in
                   List.fold_left (fun x elt -> try (adn_base_from_char elt)::x with _ -> x) [] (List.rev char_list) in
    let (result, out) = run_cell !is_step !is_interactive !run_mode strand in_ in
    Printf.printf "%s(%s)%s" (if !is_interactive then "\n" else "") (if result then "cell succeeded" else "cell failed") (if not !is_interactive then Printf.sprintf "\n%s" out else "");
    if !is_repl then
      exec in_
    else
      ()
  with _ -> ();;

let mode_str = fun mode ->
  match mode with
    0 -> "s"
  | 1 -> "sr"
  | 2 -> "t"
  | 3 -> "tr"
  | _ -> "random";;

let rec get_content_of_actual_stdin = fun acc ->
  try
    get_content_of_actual_stdin (String.cat acc (really_input_string stdin 1))
  with _ -> acc;;

let () =
  Random.self_init ();
  Arg.parse speclist has_file usage_msg;
  let stdin_str = (if !is_interactive || !is_shut then "" else get_content_of_actual_stdin "") in
  (* let () = Printf.printf "%s" stdin_str in *)
  let in_char_ls = List.of_seq (String.to_seq stdin_str) in
  if !is_repl then
    Printf.printf "Gene v0.2 REPl\n%sInterpreting in mode %s.%s\nPress ^D or ^C to quit." (if !is_step then "Step-by-step mode is on | " else "") (mode_str !run_mode) (if !is_interactive then " | Interactive mode is on" else "");
  exec in_char_ls;;
