(** Constructive / computable real numbers. *)
type t

val pp : t Fmt.t
val debug_to_string : t -> string
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t

(** Shift to the left. Equivalent to multiplication by a power of 2. The second argument
    can be negative. *)
val shift_left : t -> int32 -> t

(** Shift to the right. Equivalent to division by a power of 2. The second argument can be
    negative. *)
val shift_right : t -> int32 -> t

val negate : t -> t
val sqrt : t -> t
val select : t -> t -> t -> t
val max : t -> t -> t
val min : t -> t -> t
val abs : t -> t
val exp : t -> t
val ln : t -> t
val sin : t -> t
val cos : t -> t
val tan : t -> t
val asin : t -> t
val acos : t -> t
val atan : t -> t
val of_bigint : Z.t -> t
val of_int : int -> t
val of_int32 : int32 -> t
val of_float : float -> t
val signum : t -> int32
val bigint_value : t -> Z.t
val int_value : t -> int
val float_value : t -> float
val eval_to_string : ?digits:int32 -> ?radix:int32 -> t -> string
val compare_absolute : t -> t -> absolute_tolerance:int32 -> int
val compare : t -> t -> relative_tolerance:int32 -> absolute_tolerance:int32 -> int
val compare_known_unequal : t -> t -> int
val one : t
val minus_one : t
val two : t
val three : t
val pi : t
val half_pi : t
val e : t

(** A number that may not have been completely evaluated but is assumed to be an integer,
    * so is never evaluated beyond the decimal point. *)
val assume_int : t -> t

(** A read-only view of a term's structure, for walking the term graph (e.g.
    to visualize it). Constructor names mirror the internal representation;
    the [Prescaled*] operations wrap arguments already reduced to a
    convergence-friendly range. *)
type view =
  | IntV of Z.t
  | AssumedIntV of t
  | AddV of t * t
  | ShiftedV of t * int32
  | NegV of t
  | SelectV of t * t * t
  | MultV of t * t
  | InvV of t
  | PrescaledExpV of t
  | PrescaledCosV of t
  | PrescaledLnV of t
  | PrescaledAsinV of t
  | SqrtV of t
  | PiV

val view : t -> view

(** The term's cached approximation, if it has been evaluated:
    [Some (max_appr, min_prec)] means the value is within one unit of
    [max_appr * 2^min_prec]. *)
val approximation : t -> (Z.t * int32) option
