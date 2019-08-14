open Bigarray

type pixmap = bytes * int

let get (img, width) i j k =
  Char.code (Bytes.get img (((j * width) + i) * 3 + k))

let set (img, width) i j k v =
  Bytes.set img (((j * width) + i) * 3 + k) (Char.unsafe_chr v)

let setp (img, width) i j r g b =
  let p = ((j * width) + i) * 3 in
  Bytes.set img p (Char.unsafe_chr r);
  Bytes.set img (p + 1) (Char.unsafe_chr g);
  Bytes.set img (p + 2) (Char.unsafe_chr b)

let init ~width ~height = (Bytes.create (height * width * 3), width)

let width (s, width) = width
let height (s, width) = Bytes.length s / width / 3

let dump file (img, width) =
  let sz = Bytes.length img in
  let height = sz / 3 / width in
  let f = open_out_bin file in
  output_string f "P6\n# PL Club\n";
  Printf.fprintf f "%d %d\n255\n" width height;
  output_bytes f img;
  close_out f

let load file =
  let f = open_in_bin file in
  assert (input_line f = "P6");
  let s = input_line f in
  let i = ref 0 in
  while s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
  let width = int_of_string (String.sub s 0 !i) in
  let height =
    int_of_string (String.sub s (!i + 1) (String.length s - !i - 1)) in
  assert (input_line f = "255");
  let (s, _) as img = init width height in
  really_input f s 0 (Bytes.length s);
  close_in f;
  img
