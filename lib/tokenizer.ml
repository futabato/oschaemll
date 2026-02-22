(** 字句解析: string → token list

    入力文字列を1文字ずつ走査し、トークン列に変換する。 クォート内の文字列はそのまま保持する。

    エラーは [parse_error] の [result] 型で返す（例外は使わない）。 *)

open Types

(** 特殊文字（トークン区切り）かどうか *)
let is_special = function
  | ' ' | '\t' | '|' | '<' | '>' | ';' | '&' | '\'' | '"' -> true
  | _ -> false

(** 入力文字列をトークン列に変換する *)
let tokenize (input : string) : (token list, parse_error) result =
  let len = String.length input in
  let pos = ref 0 in
  let tokens = ref [] in
  let emit kind p = tokens := { kind; position = p } :: !tokens in

  (* 空白をスキップ *)
  let skip_spaces () =
    while !pos < len && (input.[!pos] = ' ' || input.[!pos] = '\t') do
      incr pos
    done
  in

  (* クォートされた文字列を読む *)
  let read_quoted (quote : char) : (word * int, parse_error) result =
    let start = !pos in
    incr pos;
    let buf = Buffer.create 16 in
    while !pos < len && input.[!pos] <> quote do
      Buffer.add_char buf input.[!pos];
      incr pos
    done;
    if !pos >= len then
      Error
        {
          kind = Unclosed_quote;
          message = "unclosed quote";
          position = start;
          source = input;
        }
    else begin
      incr pos;
      Ok (Word (Buffer.contents buf), start)
    end
  in

  (* 通常のワードを読む *)
  let read_word () =
    let start = !pos in
    let buf = Buffer.create 16 in
    while !pos < len && not (is_special input.[!pos]) do
      Buffer.add_char buf input.[!pos];
      incr pos
    done;
    (Word (Buffer.contents buf), start)
  in

  let rec loop () =
    skip_spaces ();
    if !pos >= len then Ok (List.rev !tokens)
    else
      match input.[!pos] with
      | '|' ->
          let start = !pos in
          incr pos;
          if !pos < len && input.[!pos] = '|' then begin
            incr pos;
            emit TOr start
          end
          else emit TPipe start;
          loop ()
      | '>' ->
          let start = !pos in
          incr pos;
          if !pos < len && input.[!pos] = '>' then begin
            incr pos;
            emit TRedirectAppend start
          end
          else emit TRedirectOut start;
          loop ()
      | '<' ->
          let start = !pos in
          incr pos;
          emit TRedirectIn start;
          loop ()
      | ';' ->
          let start = !pos in
          incr pos;
          emit TSemicolon start;
          loop ()
      | '&' ->
          let start = !pos in
          incr pos;
          if !pos < len && input.[!pos] = '&' then begin
            incr pos;
            emit TAnd start
          end
          else emit (TWord (Word "&")) start;
          loop ()
      | '\'' -> (
          match read_quoted '\'' with
          | Error e -> Error e
          | Ok (w, start) ->
              emit (TSingleQuoted w) start;
              loop ())
      | '"' -> (
          match read_quoted '"' with
          | Error e -> Error e
          | Ok (w, start) ->
              emit (TDoubleQuoted w) start;
              loop ())
      | _ ->
          let w, start = read_word () in
          emit (TWord w) start;
          loop ()
  in
  loop ()
