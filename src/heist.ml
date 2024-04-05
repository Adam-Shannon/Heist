open Bigraph
open Format
open Error

(* Fundamental storage *)

let state_bg = ref Big.id_eps

module M = Map.Make(String)
let file_parser = Parser.file Lexer.read 


(* Command line parsing *) 
let usage_msg = "./heist.exe <.heat> <bg.json> -o <output>"
let input_files = ref []
let output_json = ref ""
let output_tikz = ref ""
let anon_fun filename =
  input_files := filename :: !input_files
let speclist =
  [
    ("-t", Arg.Set_string output_tikz , "output final state to Tikz");
    ("-js", Arg.Set_string output_json , "output final state to Json")
  ]

let handle_cmd_line xs= 
  try
    if (List.length xs) != 2 then
      raise Invalid_File_Args
    else
      state_bg := Eval.parseBG(List.nth xs 0);
      if Big.is_id (!state_bg) then 
        raise Invalid_File_Args 
      else if (Sys.file_exists (List.nth xs 1)) then
        ([] , Some(!state_bg))
      else
        raise Invalid_File_Args
  with
    Invalid_File_Args -> ([] , None)

let read_file parser fn =
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    let terms = parser lex in
    close_in fh;
    terms
    

(** File writer taken from Bigrapher *)
let write_string s ~name ~path =
  try
    let out_ch = Filename.concat path name |> open_out in
    output_string out_ch s;
    close_out out_ch;
    String.length s
  with 
    Sys_error _ -> raise Invalid_File_Args

(** Json writer from Bigrapher *)
let write_json b ~name ~path =
    try Big.yojson_of_t b |> Yojson.Safe.pretty_to_string |> write_string ~name ~path
    with Sys_error _ -> raise Unchecked_failure

(** tikz writer from Bigrapher *)
let write_tikz b ~name ~path =
    try write_string (Tikz.big_to_tikz b) ~name ~path
    with Sys_error _ -> raise Unchecked_failure

let main() =
  Arg.parse speclist anon_fun usage_msg;
  let s0 = handle_cmd_line (! input_files) in
    let cmds = read_file (file_parser) (List.nth (!input_files) 1) in
      let scoped_calls = List.fold_left Eval.parse s0 cmds in 
        let (_, b') = List.fold_left Eval.evaluate scoped_calls cmds in
          match b' with
            |None -> print_string "Failed at some point \n"
            |Some b_end ->
              print_string "finished\n"; 
              (if (!output_tikz != "") then
                let _ = write_tikz b_end ~name:(!output_tikz) ~path:"./" in
                  print_string "Attempted write to Tikz \n");
              (if (!output_json != "") then
                let _ = write_json b_end ~name:(!output_json) ~path:"./" in
                  print_string "Attempted write to Json \n")
              ;;

let () = main()