(** Convenience module for writing expressions within predefined theories.  *)

open Sygus

(* Terminals *)

(** Create a term containing a variable given a variable name. *)
val var : symbol -> sygus_term

(** Create a sort given a sort name. *)
val sort : symbol -> sygus_sort

(** Create an integer constant term given an int. *)
val int : int -> sygus_term

(** Create a decimal number term given a float. *)
val real : float -> sygus_term

(** Create a hex constant given its string representation. *)
val hex : string -> sygus_term

(** Create a hex constant given a desired width (in bits) and its integer value. Requires width to be a multiple of 4 *)
val hex_of_int : int -> int -> sygus_term

(* If-then-else *)

(** Create a conditional given the condition, then-branch and else-branch expressions.  *)
val ite : sygus_term -> sygus_term -> sygus_term -> sygus_term

(* Arithmetic *)

(** Numeral division.  *)
val ( / ) : sygus_term -> sygus_term -> sygus_term

(** Numeral addition. *)
val ( + ) : sygus_term -> sygus_term -> sygus_term

(** Numeral Substraction.  *)
val ( - ) : sygus_term -> sygus_term -> sygus_term

(** Numeral multiplication. *)
val ( * ) : sygus_term -> sygus_term -> sygus_term

(** Max function (non-standard, needs to be declared first in a SyGuS file.)  *)
val max : sygus_term -> sygus_term -> sygus_term

(** Min function (non-standard, needs to be declared first in a SyGuS file.)  *)
val min : sygus_term -> sygus_term -> sygus_term

(** Modulo operation.  *)
val modulo : sygus_term -> sygus_term -> sygus_term

(** Abs operations.  *)
val abs : sygus_term -> sygus_term

(** Unary minus.  *)
val neg : sygus_term -> sygus_term

(* Bool *)

(** The false value.  *)
val mk_false : sygus_term

(** The true value in Sygus.  *)
val mk_true : sygus_term

(** Conjunction.  *)
val ( && ) : sygus_term -> sygus_term -> sygus_term

(** Disjunction. *)
val ( || ) : sygus_term -> sygus_term -> sygus_term

(** Negation.  *)
val not : sygus_term -> sygus_term

(* Comparisons *)
val ( > ) : sygus_term -> sygus_term -> sygus_term
val ( >= ) : sygus_term -> sygus_term -> sygus_term
val ( < ) : sygus_term -> sygus_term -> sygus_term
val ( <= ) : sygus_term -> sygus_term -> sygus_term
val ( = ) : sygus_term -> sygus_term -> sygus_term

(* BitVector *)

(** zero extend a bitvector *)
val zero_extend : index -> sygus_term -> sygus_term

(** sign extend a bitvector *)
val sign_extend : index -> sygus_term -> sygus_term

(** concatenate two bitvectors *)
val concat : sygus_term -> sygus_term -> sygus_term

(** extract the range i to j from bitvector *)
val extract : index -> index -> sygus_term -> sygus_term

(** logical not *)
val bvnot : sygus_term -> sygus_term

(** negate bitvector *)
val bvneg : sygus_term -> sygus_term

(** and two bitvectors together *)
val bvand : sygus_term -> sygus_term -> sygus_term

(** or two bitvectors together *)
val bvor : sygus_term -> sygus_term -> sygus_term

(** add two bitvectors together *)
val bvadd : sygus_term -> sygus_term -> sygus_term

(** multiply one bitvector with another *)
val bvmul : sygus_term -> sygus_term -> sygus_term

(** divide one bitvector with anoter *)
val bvudiv : sygus_term -> sygus_term -> sygus_term

(** get the remainder of one bitvector module another bitvector *)
val bvurem : sygus_term -> sygus_term -> sygus_term

(** signed division of two bitvectors *)
val bvsdiv : sygus_term -> sygus_term -> sygus_term

(** signed remainder of two bitvectors *)
val bvsrem : sygus_term -> sygus_term -> sygus_term

(** left shift one bitvector by another bitvector *)
val bvshl : sygus_term -> sygus_term -> sygus_term

(** shift one bitvector logically right by another bitvector *)
val bvlshr : sygus_term -> sygus_term -> sygus_term

(** shift one bitvector arithmetically by another bitvector *)
val bvashr : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by unsigned less than *)
val bvult : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by unsigned less than or equal *)
val bvule : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by unsigned greater than *)
val bvugt : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by unsigned greater than or equal *)
val bvuge : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by signed less than *)
val bvslt : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by signed less than or equal *)
val bvsle : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by signed greater than *)
val bvsgt : sygus_term -> sygus_term -> sygus_term

(** compare two bitvectors by signed greater than or equal *)
val bvsge : sygus_term -> sygus_term -> sygus_term

(** rotate a bitvector left by index bits *)
val bvrotl : index -> sygus_term -> sygus_term

(** rotate a bitvector right by index bits*)
val bvrotr : index -> sygus_term -> sygus_term

(** Non-standard, but useful bit manipulation macros. *)

(** count-leading-zeros macro. requires the width of the bitvector as first argument. *)
val clz : int -> sygus_term -> sygus_term

(** count-leading ones macro, essentially clz (~x). requires the width of the bitvector as first argument *)
val clo : int -> sygus_term -> sygus_term

(** count leading sign bits macro, essentially a clz or clo operation, depending on the MSB. requires the width of the bitvector as first argument*)
val cls : int -> sygus_term -> sygus_term

(** population count macro. requires the width of the bitvector as first argument and returns the result as a bitvector with that width *)
val popcnt : int -> sygus_term -> sygus_term

(** bit reversal macro. requires the width of the bitvector as first argument (which needs to be even) *)
val bvrev : int -> sygus_term -> sygus_term

(* Grammar *)

(** [gconst x] is the sygus grammar term [(Constant x)] *)
val gconst : sygus_sort -> sygus_gsterm

(** [gterm x] is the sygus grammar term [x] *)
val gterm : sygus_term -> sygus_gsterm

(** [gvar x] is the sygus grammar term [(Variable x)]*)
val gvar : sygus_sort -> sygus_gsterm

(** [glblock n s productions] defines the grammar productions with name [n]
    and sort [s] with the productions [productions].
  *)
val gblock : string -> sygus_sort -> sygus_gsterm list -> grammar_def

(* Predefined Sorts *)

(** The standard sort for integers.  *)
val int_sort : sygus_sort

(** The standard sort for booleans.  *)
val bool_sort : sygus_sort

(** The standard sort for reals. *)
val real_sort : sygus_sort

(** The standard sort for BitVectors, which take the width as argument *)
val bv_sort : int -> sygus_sort

(** get the width of a bitvector sort *)
val width_of_bv : sygus_sort -> int