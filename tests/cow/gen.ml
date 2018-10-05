let f md =
  let html = Filename.chop_extension md ^ ".html" in
  Printf.printf
{|(rule
  (targets %s.out)
  (deps %s)
  (action (with-stdout-to %%{targets} (run omd %%{deps}))))

(alias
  (name runtest)
  (action (diff %s %s.out)))

|} html md html html

let () =
  Arg.parse [] f ""
