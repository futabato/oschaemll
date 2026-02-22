(** シェルのオーケストレーター。

    Tokenizer → Parser → Executor を合成して、 文字列 → 実行結果 への変換を提供する。

    型が示すパイプライン:
    {[
      string → token list → command_line → pipeline_result
    ]} *)

open Types

let eval (input : string) : (pipeline_result, shell_error) result =
  match Tokenizer.tokenize input with
  | Error err -> Error (Parse_error err)
  | Ok tokens -> (
      match Parser.parse tokens with
      | Error err -> Error (Parse_error err)
      | Ok command_line -> (
          match Executor.execute_command_line command_line with
          | Error err -> Error (Execution_error err)
          | Ok result -> Ok result))
