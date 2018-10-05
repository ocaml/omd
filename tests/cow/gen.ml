let all = ref []

let f md =
  let base = Filename.chop_extension md in
  let html = base ^ ".html" in
  all := base :: !all;
  Printf.printf
{|(rule
 (targets %s.out)
 (deps %s)
 (action (with-stdout-to %%{targets} (run omd %%{deps}))))

(alias
 (name %s)
 (action (diff %s %s.out)))

|} html md base html html

let () =
  Arg.parse [] f "";
  Printf.printf
{|(alias
 (name runtest)
 (deps %s))
|} (String.concat "\n       " (List.rev_map (fun s -> Printf.sprintf "(alias %s)" s) !all))
