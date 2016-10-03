type pos = int * int (* for error message: (line number, column number) *)
[@@deriving show]

type t = string  (* �ѿ���̾�� (caml2html: id_t) *)
[@@deriving show]

type l = L of string (* �ȥåץ��٥��ؿ��䥰�����Х������Υ��٥� (caml2html: id_l) *)
[@@deriving show]

let dmy_pos = (-1, -1)

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let rec id_of_typ = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Float -> "d"
  | Type.Fun _ -> "f"
  | Type.Tuple _ -> "t"
  | Type.Array _ -> "a" 
  | Type.Var _ -> assert false
let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_typ typ) !counter
