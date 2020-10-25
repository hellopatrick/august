let read () =
  Stdio.print_string "> " ;
  Stdio.Out_channel.flush Stdio.stdout ;
  Stdio.In_channel.input_line Stdio.stdin

let redirect = Unix.dup2

let exec argv =
  let cmd = List.hd argv in
  let args = Array.of_list argv in
  try Unix.execvp cmd args
  with Unix.Unix_error (Unix.ENOENT, _, cmd) ->
    Stdio.printf "Unknown cmd: %s\n" cmd ;
    Stdio.Out_channel.flush Stdio.stdout ;
    exit 2

let process cmds =
  let rec aux cmds r =
    match cmds with
    | [] -> ()
    | [cmd] -> redirect r Unix.stdin ; exec cmd
    | cmd :: rest -> (
        let r', w' = Unix.pipe () in
        match Unix.fork () with
        | 0 ->
            Unix.close r' ;
            redirect r Unix.stdin ;
            redirect w' Unix.stdout ;
            exec cmd
        | _ -> Unix.close w' ; Unix.close r ; aux rest r' ) in
  aux cmds Unix.stdin

let fork_run cmds =
  match Unix.fork () with
  | 0 -> process cmds
  | _ ->
      let _ = Unix.wait () in
      ()

let eval line =
  let open August in
  let cmd = line |> Lexing.from_string |> Parser.start Lexer.read in
  match cmd with Some cmd -> fork_run cmd | None -> ()

let rec run () =
  match read () with
  | Some line ->
      let _ = eval line in
      run ()
  | None -> ()

let () = run ()
