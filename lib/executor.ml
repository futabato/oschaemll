(** コマンドの実行: command_line → pipeline_result

    副作用（プロセス起動、I/O）はこのモジュールに閉じ込める。 他のモジュール (tokenizer, parser) は純粋関数のみ。

    エラーは [execution_error] の [result] 型で返す。 *)

open Types

(** ファイルディスクリプタの内容をすべて読み取る *)
let read_all_fd (fd : Unix.file_descr) : string =
  let buf = Buffer.create 256 in
  let tmp = Bytes.create 4096 in
  let rec drain () =
    match Unix.read fd tmp 0 4096 with
    | 0 -> Buffer.contents buf
    | n ->
        Buffer.add_subbytes buf tmp 0 n;
        drain ()
  in
  drain ()

(** Unix.process_status → Types.process_status *)
let convert_status (status : Unix.process_status) : process_status =
  match status with
  | Unix.WEXITED code -> Exited (ExitCode code)
  | Unix.WSIGNALED n ->
      if n = Sys.sigint then Signaled SIGINT
      else if n = Sys.sigterm then Signaled SIGTERM
      else if n = Sys.sigkill then Signaled SIGKILL
      else if n = Sys.sigpipe then Signaled SIGPIPE
      else Signaled SIGTERM
  | Unix.WSTOPPED _ -> Signaled SIGTERM

(** fd を安全に閉じる（二重クローズでも例外を出さない） *)
let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

(** リダイレクト用の fd を開く *)
let open_redirect (r : redirect) : Unix.file_descr =
  let (FilePath path) = r.target in
  match r.direction with
  | Input -> Unix.openfile path [ O_RDONLY ] 0
  | Output -> Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
  | Append -> Unix.openfile path [ O_WRONLY; O_CREAT; O_APPEND ] 0o644

(** コマンドの argv を組み立てる *)
let argv_of_command (cmd : simple_command) : string * string array =
  let (Word prog) = cmd.program in
  let args = List.map (fun (Word w) -> w) cmd.args in
  (prog, Array.of_list (prog :: args))

(** リダイレクトを適用し、実際の stdin/stdout と追加で開いた fd を返す *)
let apply_redirects (cmd : simple_command) (default_in : Unix.file_descr)
    (default_out : Unix.file_descr) :
    Unix.file_descr * Unix.file_descr * Unix.file_descr list =
  let actual_in = ref default_in in
  let actual_out = ref default_out in
  let opened = ref [] in
  List.iter
    (fun r ->
      let fd = open_redirect r in
      opened := fd :: !opened;
      match r.direction with
      | Input -> actual_in := fd
      | Output -> actual_out := fd
      | Append -> actual_out := fd)
    cmd.redirects;
  (!actual_in, !actual_out, !opened)

(** 単一コマンドを実行し stdout/stderr をキャプチャする *)
let execute_capturing (cmd : simple_command) (stdin_fd : Unix.file_descr) :
    (command_result, execution_error) result =
  let prog, argv = argv_of_command cmd in
  try
    let out_r, out_w = Unix.pipe ~cloexec:true () in
    let err_r, err_w = Unix.pipe ~cloexec:true () in
    let actual_in, actual_out, extra = apply_redirects cmd stdin_fd out_w in
    let pid =
      try Unix.create_process prog argv actual_in actual_out err_w
      with exn ->
        List.iter close_fd [ out_r; out_w; err_r; err_w ];
        List.iter close_fd extra;
        raise exn
    in
    close_fd out_w;
    close_fd err_w;
    List.iter close_fd extra;
    let stdout_str = read_all_fd out_r in
    close_fd out_r;
    let stderr_str = read_all_fd err_r in
    close_fd err_r;
    let _, status = Unix.waitpid [] pid in
    Ok
      {
        status = convert_status status;
        stdout = stdout_str;
        stderr = stderr_str;
      }
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      Error
        {
          kind = Command_not_found;
          message = Printf.sprintf "command not found: %s" prog;
          command = cmd;
        }
  | Unix.Unix_error (Unix.EACCES, _, _) ->
      Error
        {
          kind = Permission_denied;
          message = Printf.sprintf "permission denied: %s" prog;
          command = cmd;
        }
  | Unix.Unix_error (err, _, _) ->
      Error { kind = IO_error; message = Unix.error_message err; command = cmd }

(** パイプラインを実行する

    単一コマンドの場合は直接実行。 複数コマンドの場合はパイプで接続して並行実行し、 最後のコマンドの stdout/stderr をキャプチャする。 *)
let execute_pipeline (pl : pipeline) : (pipeline_result, execution_error) result
    =
  match pl.rest with
  | [] -> (
      match execute_capturing pl.first Unix.stdin with
      | Error e -> Error e
      | Ok r -> Ok { results = [ r ]; last = r })
  | rest_cmds -> (
      let all = Array.of_list (pl.first :: rest_cmds) in
      let n = Array.length all in
      let pipes = Array.init (n - 1) (fun _ -> Unix.pipe ~cloexec:true ()) in
      let cap_r, cap_w = Unix.pipe ~cloexec:true () in
      let cerr_r, cerr_w = Unix.pipe ~cloexec:true () in
      let pids = Array.make n 0 in
      let error = ref None in
      for i = 0 to n - 1 do
        if Option.is_none !error then begin
          let prog, argv = argv_of_command all.(i) in
          let cmd_in = if i = 0 then Unix.stdin else fst pipes.(i - 1) in
          let cmd_out = if i = n - 1 then cap_w else snd pipes.(i) in
          let cmd_err = if i = n - 1 then cerr_w else Unix.stderr in
          try
            pids.(i) <- Unix.create_process prog argv cmd_in cmd_out cmd_err
          with
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              error :=
                Some
                  {
                    kind = Command_not_found;
                    message = Printf.sprintf "command not found: %s" prog;
                    command = all.(i);
                  }
          | Unix.Unix_error (Unix.EACCES, _, _) ->
              error :=
                Some
                  {
                    kind = Permission_denied;
                    message = Printf.sprintf "permission denied: %s" prog;
                    command = all.(i);
                  }
          | Unix.Unix_error (err, _, _) ->
              error :=
                Some
                  {
                    kind = IO_error;
                    message = Unix.error_message err;
                    command = all.(i);
                  }
        end
      done;
      (* 親プロセス側でパイプ fd をすべて閉じる *)
      Array.iter
        (fun (r, w) ->
          close_fd r;
          close_fd w)
        pipes;
      close_fd cap_w;
      close_fd cerr_w;
      match !error with
      | Some e ->
          close_fd cap_r;
          close_fd cerr_r;
          Array.iter
            (fun pid ->
              if pid > 0 then begin
                (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
                try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ()
              end)
            pids;
          Error e
      | None ->
          let stdout_str = read_all_fd cap_r in
          close_fd cap_r;
          let stderr_str = read_all_fd cerr_r in
          close_fd cerr_r;
          let results =
            Array.to_list
              (Array.mapi
                 (fun i pid ->
                   let _, status = Unix.waitpid [] pid in
                   if i = n - 1 then
                     {
                       status = convert_status status;
                       stdout = stdout_str;
                       stderr = stderr_str;
                     }
                   else
                     {
                       status = convert_status status;
                       stdout = "";
                       stderr = "";
                     })
                 pids)
          in
          let last = List.nth results (n - 1) in
          Ok { results; last })

(** command_line を実行する。

    接続子の意味に従ってパイプラインを順に実行する:
    - [Sequential] (;) : 常に次を実行
    - [And] (&&) : 前が成功した場合のみ次を実行
    - [Or] (||) : 前が失敗した場合のみ次を実行 *)
let execute_command_line (cl : command_line) :
    (pipeline_result, execution_error) result =
  match execute_pipeline cl.first with
  | Error e -> Error e
  | Ok first_result ->
      let rec eval_rest (prev : pipeline_result)
          (rest : connected_pipeline list) :
          (pipeline_result, execution_error) result =
        match rest with
        | [] -> Ok prev
        | { connector; pipeline } :: tail ->
            let should_run =
              match connector with
              | Sequential -> true
              | And -> pipeline_success prev
              | Or -> not (pipeline_success prev)
            in
            if should_run then
              match execute_pipeline pipeline with
              | Error e -> Error e
              | Ok result -> eval_rest result tail
            else eval_rest prev tail
      in
      eval_rest first_result cl.rest
