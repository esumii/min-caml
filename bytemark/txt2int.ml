while true do
  let c = input_char stdin in
  Printf.printf "%d\n" (int_of_char c);
  flush stdout
done
