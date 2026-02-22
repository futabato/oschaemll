(** REPL（Read-Eval-Print Loop）。

    ライブラリの [Mini_shell.Shell.eval] を呼ぶだけ。 ロジックはすべて lib/ 側にある。 *)

open Mini_shell

let read_line_prompt () : string option =
  print_string "mini> ";
  flush stdout;
  try Some (input_line stdin) with End_of_file -> None

let () =
  print_endline "Mini Shell (OCaml) — type 'exit' to quit";
  let rec loop () =
    match read_line_prompt () with
    | None ->
        print_newline ();
        print_endline "bye!"
    | Some "exit" -> print_endline "bye!"
    | Some "" -> loop ()
    | Some input ->
        (match Shell.eval input with
        | Ok result ->
            if result.Types.last.stdout <> "" then
              print_string result.Types.last.stdout;
            if result.Types.last.stderr <> "" then
              Printf.eprintf "%s" result.Types.last.stderr
        | Error (Types.Parse_error err) ->
            Printf.eprintf "%s\n" (Types.display_parse_error err)
        | Error (Types.Execution_error err) ->
            Printf.eprintf "error: %s\n" err.Types.message);
        flush stdout;
        flush stderr;
        loop ()
  in
  loop ()
