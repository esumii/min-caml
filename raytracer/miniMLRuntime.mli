(* monomorphic runtime primitives *)

external (=) : int -> int -> bool = "%equal"
external (<>) : int -> int -> bool = "%notequal"
external (<) : int -> int -> bool = "%lessthan"
external (>) : int -> int -> bool = "%greaterthan"
external (<=) : int -> int -> bool = "%lessequal"
external (>=) : int -> int -> bool = "%greaterequal"

external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"

external fequal : float -> float -> bool = "%equal"
external fless : float -> float -> bool = "%lessthan"

val fispos : float -> bool
val fisneg : float -> bool
val fiszero : float -> bool

external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"

external xor : bool -> bool -> bool = "%notequal"
external not : bool -> bool = "%boolnot"

(* runtime routines which should be provided by user-written library *)
external fabs : float -> float = "%absfloat"
external fneg : float -> float = "%negfloat"
val fsqr : float -> float
val fhalf : float -> float
external sqrt : float -> float = "sqrt_float" "sqrt" "float"
external floor : float -> float = "floor_float" "floor" "float"

external int_of_float : float -> int = "%intoffloat"
external float_of_int : int -> float = "%floatofint"

external cos : float -> float = "cos_float" "cos" "float"
external sin : float -> float = "sin_float" "sin" "float"
external atan : float -> float = "atan_float" "atan" "float"

val read_float : unit -> float
val read_int : unit -> int

external create_array : int -> 'a -> 'a array = "caml_make_vect"

val print_char : int -> unit
val print_int : int -> unit
