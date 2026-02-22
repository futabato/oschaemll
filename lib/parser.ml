(** 構文解析: token list → command_line

    フラットなトークン列を、型で定義されたASTに変換する。

    - パイプライン: simple_command ('|' simple_command)*
    - コマンドライン: pipeline (connector pipeline)*
    - リダイレクト: ('<' | '>' | '>>') filepath

    エラーは [parse_error] の [result] 型で返す。 *)

open Types

(** トークンからワードを抽出する（コマンド名・引数として使えるもの） *)
let word_of_token (t : token) : word option =
  match t.kind with
  | TWord w -> Some w
  | TSingleQuoted w -> Some w
  | TDoubleQuoted w -> Some w
  | TPipe -> None
  | TRedirectIn -> None
  | TRedirectOut -> None
  | TRedirectAppend -> None
  | TSemicolon -> None
  | TAnd -> None
  | TOr -> None

(** トークンがリダイレクト演算子かどうか *)
let redirect_of_token (t : token) : redirect_direction option =
  match t.kind with
  | TRedirectIn -> Some Input
  | TRedirectOut -> Some Output
  | TRedirectAppend -> Some Append
  | TWord _ -> None
  | TSingleQuoted _ -> None
  | TDoubleQuoted _ -> None
  | TPipe -> None
  | TSemicolon -> None
  | TAnd -> None
  | TOr -> None

(** トークンが接続子（;, &&, ||）かどうか *)
let connector_of_token (t : token) : connector option =
  match t.kind with
  | TSemicolon -> Some Sequential
  | TAnd -> Some And
  | TOr -> Some Or
  | TWord _ -> None
  | TSingleQuoted _ -> None
  | TDoubleQuoted _ -> None
  | TPipe -> None
  | TRedirectIn -> None
  | TRedirectOut -> None
  | TRedirectAppend -> None

(** simple_command をパースする。 トークン列の先頭からワードとリダイレクトを消費し、 (コマンド, 残りのトークン) を返す。 *)
let parse_simple_command (tokens : token list) :
    (simple_command * token list, parse_error) result =
  let rec collect tokens words redirects =
    match tokens with
    | [] -> Ok (List.rev words, List.rev redirects, [])
    | t :: rest -> (
        match redirect_of_token t with
        | Some direction -> (
            match rest with
            | [] ->
                Error
                  {
                    kind = Redirect_without_target;
                    message = "expected file path after redirect";
                    position = t.position;
                    source = "";
                  }
            | target :: rest2 -> (
                match word_of_token target with
                | Some (Word s) ->
                    collect rest2 words
                      ({ direction; target = FilePath s } :: redirects)
                | None ->
                    Error
                      {
                        kind = Redirect_without_target;
                        message = "expected file path after redirect";
                        position = target.position;
                        source = "";
                      }))
        | None -> (
            match word_of_token t with
            | Some w -> collect rest (w :: words) redirects
            | None -> Ok (List.rev words, List.rev redirects, tokens)))
  in
  match tokens with
  | [] ->
      Error
        {
          kind = Empty_pipeline;
          message = "expected command";
          position = 0;
          source = "";
        }
  | t :: _ -> (
      match word_of_token t with
      | None ->
          Error
            {
              kind = Unexpected_token;
              message = "expected command";
              position = t.position;
              source = "";
            }
      | Some _ -> (
          match collect tokens [] [] with
          | Error e -> Error e
          | Ok ([], _, _) ->
              Error
                {
                  kind = Empty_pipeline;
                  message = "expected command";
                  position = 0;
                  source = "";
                }
          | Ok (program :: args, redirects, remaining) ->
              Ok ({ program; args; redirects }, remaining)))

(** pipeline をパースする: simple_command ('|' simple_command)* *)
let parse_pipeline (tokens : token list) :
    (pipeline * token list, parse_error) result =
  match parse_simple_command tokens with
  | Error e -> Error e
  | Ok (first, remaining) ->
      let rec collect_rest (tokens : token list) rest_cmds =
        match tokens with
        | { kind = TPipe; _ } :: rest -> (
            match parse_simple_command rest with
            | Error e -> Error e
            | Ok (cmd, remaining) -> collect_rest remaining (cmd :: rest_cmds))
        | _ -> Ok (({ first; rest = List.rev rest_cmds } : pipeline), tokens)
      in
      collect_rest remaining []

(** command_line をパースする: pipeline (connector pipeline)* *)
let parse (tokens : token list) : (command_line, parse_error) result =
  match tokens with
  | [] ->
      Error
        {
          kind = Empty_pipeline;
          message = "empty input";
          position = 0;
          source = "";
        }
  | _ -> (
      match parse_pipeline tokens with
      | Error e -> Error e
      | Ok (first_pl, remaining) -> (
          let rec collect_rest (tokens : token list) connected =
            match tokens with
            | [] -> Ok (List.rev connected)
            | t :: rest -> (
                match connector_of_token t with
                | Some conn -> (
                    match rest with
                    | [] ->
                        Error
                          {
                            kind = Trailing_connector;
                            message = "trailing connector";
                            position = t.position;
                            source = "";
                          }
                    | _ -> (
                        match parse_pipeline rest with
                        | Error e -> Error e
                        | Ok (pl, remaining) ->
                            collect_rest remaining
                              ({ connector = conn; pipeline = pl } :: connected)
                        ))
                | None ->
                    Error
                      {
                        kind = Unexpected_token;
                        message = "unexpected token";
                        position = t.position;
                        source = "";
                      })
          in
          match collect_rest remaining [] with
          | Error e -> Error e
          | Ok rest -> Ok { first = first_pl; rest }))
