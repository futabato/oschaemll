(** テスト。

    Alcotest を使ったユニットテスト。 パターンマッチで成功/失敗を検証する。 *)

open Mini_shell
open Types

(** ヘルパー: tokenize → parse を一気にやる *)
let parse_input (input : string) : (command_line, parse_error) result =
  match Tokenizer.tokenize input with
  | Error err -> Error err
  | Ok tokens -> Parser.parse tokens

(** Alcotest 用ヘルパー: result を unwrap する *)
let unwrap_ok msg = function Ok v -> v | Error _ -> Alcotest.fail msg

let unwrap_err msg = function Error e -> e | Ok _ -> Alcotest.fail msg

(* ============================================
   字句解析
   ============================================ *)

let test_tokenize_simple () =
  let tokens = unwrap_ok "tokenize" (Tokenizer.tokenize "ls -la /tmp") in
  Alcotest.(check int) "3 tokens" 3 (List.length tokens)

let test_tokenize_pipe () =
  let tokens =
    unwrap_ok "tokenize" (Tokenizer.tokenize "cat file | grep hello")
  in
  let kinds = List.map (fun (t : token) -> t.kind) tokens in
  match kinds with
  | [ TWord _; TWord _; TPipe; TWord _; TWord _ ] -> ()
  | _ -> Alcotest.fail "unexpected token kinds"

let test_tokenize_redirect () =
  let tokens =
    unwrap_ok "tokenize" (Tokenizer.tokenize "echo hello > out.txt")
  in
  match List.nth_opt tokens 2 with
  | Some { kind = TRedirectOut; _ } -> ()
  | _ -> Alcotest.fail "expected redirect out"

let test_tokenize_append () =
  let tokens =
    unwrap_ok "tokenize" (Tokenizer.tokenize "echo hello >> log.txt")
  in
  match List.nth_opt tokens 2 with
  | Some { kind = TRedirectAppend; _ } -> ()
  | _ -> Alcotest.fail "expected redirect append"

let test_tokenize_connectors () =
  let tokens =
    unwrap_ok "tokenize" (Tokenizer.tokenize "cmd1 && cmd2 || cmd3")
  in
  let kinds = List.map (fun (t : token) -> t.kind) tokens in
  match kinds with
  | [ TWord _; TAnd; TWord _; TOr; TWord _ ] -> ()
  | _ -> Alcotest.fail "unexpected connector kinds"

let test_tokenize_single_quote () =
  let tokens = unwrap_ok "tokenize" (Tokenizer.tokenize "echo 'hello world'") in
  match tokens with
  | [ _; { kind = TSingleQuoted (Word "hello world"); _ } ] -> ()
  | _ -> Alcotest.fail "unexpected quoted token"

let test_tokenize_unclosed_quote () =
  let err = unwrap_err "should fail" (Tokenizer.tokenize "echo 'unclosed") in
  Alcotest.(check bool) "unclosed quote" true (err.kind = Unclosed_quote)

let tokenizer_tests =
  [
    Alcotest.test_case "simple command" `Quick test_tokenize_simple;
    Alcotest.test_case "pipe" `Quick test_tokenize_pipe;
    Alcotest.test_case "redirect out" `Quick test_tokenize_redirect;
    Alcotest.test_case "append redirect" `Quick test_tokenize_append;
    Alcotest.test_case "and/or connectors" `Quick test_tokenize_connectors;
    Alcotest.test_case "single quoted" `Quick test_tokenize_single_quote;
    Alcotest.test_case "unclosed quote" `Quick test_tokenize_unclosed_quote;
  ]

(* ============================================
   構文解析
   ============================================ *)

let test_parse_simple () =
  let cmd_line = unwrap_ok "parse" (parse_input "ls -la /tmp") in
  let cmd = cmd_line.first.first in
  Alcotest.(check bool) "program" true (cmd.program = Word "ls");
  Alcotest.(check bool) "args" true (cmd.args = [ Word "-la"; Word "/tmp" ]);
  Alcotest.(check bool) "no rest" true (cmd_line.rest = [])

let test_parse_pipeline () =
  let cmd_line =
    unwrap_ok "parse" (parse_input "cat file | grep hello | wc -l")
  in
  let pl = cmd_line.first in
  Alcotest.(check bool) "first is cat" true (pl.first.program = Word "cat");
  Alcotest.(check int) "2 piped cmds" 2 (List.length pl.rest)

let test_parse_redirect () =
  let cmd_line = unwrap_ok "parse" (parse_input "echo hello > out.txt") in
  let cmd = cmd_line.first.first in
  match cmd.redirects with
  | [ { direction = Output; target = FilePath "out.txt" } ] -> ()
  | _ -> Alcotest.fail "unexpected redirect"

let test_parse_and () =
  let cmd_line = unwrap_ok "parse" (parse_input "make && make install") in
  match cmd_line.rest with
  | [ { connector = And; _ } ] -> ()
  | _ -> Alcotest.fail "expected And connector"

let test_parse_or () =
  let cmd_line =
    unwrap_ok "parse" (parse_input "test -f file || echo missing")
  in
  match cmd_line.rest with
  | [ { connector = Or; _ } ] -> ()
  | _ -> Alcotest.fail "expected Or connector"

let test_parse_sequential () =
  let cmd_line = unwrap_ok "parse" (parse_input "echo a ; echo b ; echo c") in
  Alcotest.(check int) "2 rest" 2 (List.length cmd_line.rest);
  Alcotest.(check bool)
    "all sequential" true
    (List.for_all (fun cp -> cp.connector = Sequential) cmd_line.rest)

let test_parse_mixed () =
  let cmd_line =
    unwrap_ok "parse" (parse_input "cmd1 && cmd2 || cmd3 ; cmd4")
  in
  match cmd_line.rest with
  | [
   { connector = And; _ }; { connector = Or; _ }; { connector = Sequential; _ };
  ] ->
      ()
  | _ -> Alcotest.fail "unexpected connectors"

let test_parse_trailing_connector () =
  let err = unwrap_err "should fail" (parse_input "echo hello &&") in
  Alcotest.(check bool) "trailing" true (err.kind = Trailing_connector)

let test_parse_empty () =
  let err = unwrap_err "should fail" (Parser.parse []) in
  Alcotest.(check bool) "empty" true (err.kind = Empty_pipeline)

let parser_tests =
  [
    Alcotest.test_case "simple command" `Quick test_parse_simple;
    Alcotest.test_case "pipeline" `Quick test_parse_pipeline;
    Alcotest.test_case "redirect out" `Quick test_parse_redirect;
    Alcotest.test_case "and connector" `Quick test_parse_and;
    Alcotest.test_case "or connector" `Quick test_parse_or;
    Alcotest.test_case "sequential" `Quick test_parse_sequential;
    Alcotest.test_case "mixed connectors" `Quick test_parse_mixed;
    Alcotest.test_case "trailing connector" `Quick test_parse_trailing_connector;
    Alcotest.test_case "empty input" `Quick test_parse_empty;
  ]

(* ============================================
   型設計の検証
   ============================================ *)

let test_process_status_exhaustive () =
  (* この関数がコンパイルを通ること自体が型の網羅性テスト *)
  let describe status =
    match status with
    | Exited (ExitCode code) -> Printf.sprintf "exited %d" code
    | Signaled signal -> (
        match signal with
        | SIGINT -> "interrupted"
        | SIGTERM -> "terminated"
        | SIGKILL -> "killed"
        | SIGPIPE -> "broken pipe")
  in
  ignore (describe (Exited (ExitCode 0)));
  ignore (describe (Signaled SIGINT))

let test_connector_exhaustive () =
  let describe conn =
    match conn with Sequential -> ";" | And -> "&&" | Or -> "||"
  in
  ignore (describe Sequential);
  ignore (describe And);
  ignore (describe Or)

let test_is_success () =
  let ok = { status = Exited (ExitCode 0); stdout = ""; stderr = "" } in
  let fail = { status = Exited (ExitCode 1); stdout = ""; stderr = "" } in
  let sig' = { status = Signaled SIGINT; stdout = ""; stderr = "" } in
  Alcotest.(check bool) "exit 0 is success" true (is_success ok);
  Alcotest.(check bool) "exit 1 is not" false (is_success fail);
  Alcotest.(check bool) "signal is not" false (is_success sig')

let type_design_tests =
  [
    Alcotest.test_case "process_status exhaustive" `Quick
      test_process_status_exhaustive;
    Alcotest.test_case "connector exhaustive" `Quick test_connector_exhaustive;
    Alcotest.test_case "is_success" `Quick test_is_success;
  ]

(* ============================================
   統合テスト
   ============================================ *)

let test_eval_echo () =
  let result = unwrap_ok "eval" (Shell.eval "echo hello") in
  Alcotest.(check bool) "success" true (pipeline_success result);
  Alcotest.(check string) "stdout" "hello\n" result.last.stdout

let test_eval_and_success () =
  let result = unwrap_ok "eval" (Shell.eval "echo ok && echo done") in
  Alcotest.(check bool) "success" true (pipeline_success result);
  Alcotest.(check string) "stdout" "done\n" result.last.stdout

let test_eval_and_failure () =
  let result = unwrap_ok "eval" (Shell.eval "false && echo nope") in
  Alcotest.(check bool) "not success" false (pipeline_success result)

let test_eval_or_fallback () =
  let result = unwrap_ok "eval" (Shell.eval "false || echo fallback") in
  Alcotest.(check bool) "success" true (pipeline_success result);
  Alcotest.(check string) "stdout" "fallback\n" result.last.stdout

let test_eval_parse_error () =
  match Shell.eval "echo 'unclosed" with
  | Error (Parse_error err) ->
      Alcotest.(check bool) "unclosed" true (err.kind = Unclosed_quote)
  | _ -> Alcotest.fail "expected parse error"

let test_eval_not_found () =
  match Shell.eval "nonexistent_cmd_xyz" with
  | Error (Execution_error err) ->
      Alcotest.(check bool) "not found" true (err.kind = Command_not_found)
  | _ -> Alcotest.fail "expected execution error"

let integration_tests =
  [
    Alcotest.test_case "echo" `Quick test_eval_echo;
    Alcotest.test_case "&& success" `Quick test_eval_and_success;
    Alcotest.test_case "&& failure" `Quick test_eval_and_failure;
    Alcotest.test_case "|| fallback" `Quick test_eval_or_fallback;
    Alcotest.test_case "parse error" `Quick test_eval_parse_error;
    Alcotest.test_case "command not found" `Quick test_eval_not_found;
  ]

(* ============================================
   メイン
   ============================================ *)

let () =
  Alcotest.run "mini_shell"
    [
      ("tokenizer", tokenizer_tests);
      ("parser", parser_tests);
      ("type_design", type_design_tests);
      ("integration", integration_tests);
    ]
