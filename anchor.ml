let re = Str.regexp "\\([(\\/]\\)\\* \\(.*\\) (caml2html: \\([a-zA-Z0-9_]+\\)) \\*\\([)\\/]\\)"

let rec loop previous_lines =
  try
    let current_line = read_line () in
    let new_line =
      try
        ignore (Str.search_forward re current_line 0);
        let comment = Str.matched_group 2 current_line in
        let anchor = Str.matched_group 3 current_line in
        Printf.printf "<a name=\"%s\"></a>" anchor;
        Str.global_replace re "\\1* \\2 *\\4" current_line
      with Not_found -> current_line in
    if List.length previous_lines < 3 then
      loop (previous_lines @ [new_line])
    else begin
      print_endline (List.hd previous_lines);
      loop (List.tl previous_lines @ [new_line])
    end
  with End_of_file -> List.iter print_endline previous_lines

let _ = loop []
