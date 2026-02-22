(** 簡易シェルの型定義。

    OCamlのバリアント型（代数的データ型）でコマンドの構文構造を表現する。 この型定義を読めば、シェルが何を扱えるかがすべてわかる。

    設計方針:
    - 不正な状態はそもそも型で表現できない
    - パターンマッチの網羅性チェックが漏れを防ぐ
    - 型定義がそのままドキュメントになる *)

(** {1 プリミティブな意味型} *)

type word = Word of string
type file_path = FilePath of string
type exit_code = ExitCode of int

(** {1 トークン: 字句解析の出力} *)

type token_kind =
  | TWord of word
  | TPipe
  | TRedirectIn
  | TRedirectOut
  | TRedirectAppend
  | TSemicolon
  | TAnd
  | TOr
  | TSingleQuoted of word
  | TDoubleQuoted of word

type token = { kind : token_kind; position : int }

(** {1 リダイレクト} *)

type redirect_direction = Input | Output | Append
type redirect = { direction : redirect_direction; target : file_path }

(** {1 コマンド} *)

type simple_command = {
  program : word;
  args : word list;
  redirects : redirect list;
}

(** {1 パイプライン}

    非空リストを型で保証: first + rest の構造により 空のパイプラインは表現不能。 *)

type pipeline = { first : simple_command; rest : simple_command list }

(** {1 複合コマンド} *)

type connector =
  | Sequential  (** A ; B *)
  | And  (** A && B *)
  | Or  (** A || B *)

type connected_pipeline = { connector : connector; pipeline : pipeline }

type command_line = { first : pipeline; rest : connected_pipeline list }
(** 入力1行の全体を表すトップレベルの型。

    - 空のコマンドラインは表現不能（firstが必須）
    - 接続子が宙に浮くことも不可能（必ずパイプラインとペア） *)

(** {1 実行結果} *)

type signal_kind = SIGINT | SIGTERM | SIGKILL | SIGPIPE
type process_status = Exited of exit_code | Signaled of signal_kind

type command_result = {
  status : process_status;
  stdout : string;
  stderr : string;
}

type pipeline_result = { results : command_result list; last : command_result }

(** {1 エラー型} *)

type parse_error_kind =
  | Unexpected_token
  | Unclosed_quote
  | Empty_pipeline
  | Trailing_connector
  | Redirect_without_target

type parse_error = {
  kind : parse_error_kind;
  message : string;
  position : int;
  source : string;
}

type execution_error_kind =
  | Command_not_found
  | Permission_denied
  | IO_error
  | Redirect_failed

type execution_error = {
  kind : execution_error_kind;
  message : string;
  command : simple_command;
}

type shell_error =
  | Parse_error of parse_error
  | Execution_error of execution_error

(** {1 ヘルパー関数} *)

let is_success (result : command_result) : bool =
  match result.status with
  | Exited (ExitCode 0) -> true
  | Exited _ -> false
  | Signaled _ -> false

let pipeline_success (result : pipeline_result) : bool = is_success result.last

let display_parse_error (err : parse_error) : string =
  let marker = String.make err.position ' ' ^ "^" in
  Printf.sprintf "parse error: %s\n  %s\n  %s" err.message err.source marker
