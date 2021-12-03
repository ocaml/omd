let log f =
  if Sys.getenv_opt "DEBUG" |> Option.is_some then
    f Printf.eprintf
  else
    ()
