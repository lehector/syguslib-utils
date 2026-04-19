(** utility functions that are not defined in Stdlib *)

(** iterate a function n times on a starting input x. that is: f^n(x) = f(f(f(f...f(x)))) *)
val f_iter : ('a -> 'a) -> int -> 'a -> 'a

(** generate a list of size n with a function that takes each index and returns the element at that index *)
val list_by_index : (int -> 'a) -> int -> 'a list