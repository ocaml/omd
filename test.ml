let () =
  if Array.length Sys.argv < 1 then (
    Printf.eprintf "usage: %s <dirname>\n" (Filename.basename Sys.argv.(0));
    exit 1;
  )

let process file =
  let html = (Filename.chop_extension file) ^ ".html" in
  if not (Sys.file_exists html) then (
    Printf.eprintf "File %s does not exist.\n" html;
    exit 2;
  );
  let t1 = Md.of_file html in
  let t2 = Md.of_string (Md.to_string t1) in
  Md.assert_equal t1 t2

let () =
  let dir = Sys.argv.(1) in
  let files = Sys.readdir dir in
  let files = Array.to_list files in
  let files = List.map (fun f -> Filename.concat dir f) files in
  let md_files = List.filter (fun f -> Filename.check_suffix f ".md") files in
  List.iter process md_files

