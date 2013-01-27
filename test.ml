let dir =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    Filename.concat (Sys.getcwd ()) "lib_tests/md"

let process file =
  let html = (Filename.chop_extension file) ^ ".html" in
  if not (Sys.file_exists html) then (
    Printf.eprintf "File %s does not exist.\n" html;
    exit 2;
  );
  let t1 = Cow.Md.of_file html in
  let t2 = Cow.Md.of_string (Cow.Md.to_string t1) in
  Cow.Md.assert_equal t1 t2

let () =
  let files = Sys.readdir dir in
  let files = Array.to_list files in
  let files = List.map (fun f -> Filename.concat dir f) files in
  let md_files = List.filter (fun f -> Filename.check_suffix f ".md") files in
  List.iter process md_files

