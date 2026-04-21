open Sygus
open Base

(* ============================================================================================= *)
(*                                      SHORT FORM EXPRESSIONS                                   *)
(* ============================================================================================= *)

(* Terminals *)
let var x = mk_t_id (mk_id_simple x)
let sort x = mk_sort (mk_id_simple x)
let int x = mk_t_lit (mk_lit_num x)
let real f = mk_t_lit (mk_lit_dec f)
let hex x = mk_t_lit (mk_lit_hex x)

(* If-then-else *)
let ite a b c = mk_t_app (mk_id_simple "ite") [ a; b; c ]

(* Arithmetic *)
let ( / ) e1 e2 = mk_t_app (mk_id_simple "div") [ e1; e2 ]
let ( + ) e1 e2 = mk_t_app (mk_id_simple "+") [ e1; e2 ]
let ( - ) e1 e2 = mk_t_app (mk_id_simple "-") [ e1; e2 ]
let ( * ) e1 e2 = mk_t_app (mk_id_simple "*") [ e1; e2 ]
let max e1 e2 = mk_t_app (mk_id_simple "max") [ e1; e2 ]
let min e1 e2 = mk_t_app (mk_id_simple "min") [ e1; e2 ]
let modulo e1 e2 = mk_t_app (mk_id_simple "mod") [ e1; e2 ]
let abs e1 = mk_t_app (mk_id_simple "abs") [ e1 ]
let neg e1 = mk_t_app (mk_id_simple "-") [ e1 ]

(* Bool *)
let mk_false = mk_simple_id "false"
let mk_true = mk_simple_id "true"
let ( && ) e1 e2 = mk_t_app (mk_id_simple "and") [ e1; e2 ]
let ( || ) e1 e2 = mk_t_app (mk_id_simple "or") [ e1; e2 ]
let not e1 = mk_t_app (mk_id_simple "not") [ e1 ]

(* BitVectors *)
let zero_extend i e1 = mk_t_app (mk_id_indexed "zero_extend" [i]) [e1]
let sign_extend i e1 = mk_t_app (mk_id_indexed "sign_extend" [i]) [e1]
let concat e1 e2 = mk_t_app (mk_id_simple "concat") [e1; e2]
let extract i j e1 = mk_t_app (mk_id_indexed "extract" [i; j]) [e1]
let bvnot e1 = mk_t_app (mk_id_simple "bvnot") [e1]
let bvneg e1 = mk_t_app (mk_id_simple "bvneg") [e1]
let bvand e1 e2 = mk_t_app (mk_id_simple "bvand") [e1; e2]
let bvor e1 e2 = mk_t_app (mk_id_simple "bvor") [e1; e2]
let bvadd e1 e2 = mk_t_app (mk_id_simple "bvadd") [e1; e2]
let bvmul e1 e2 = mk_t_app (mk_id_simple "bvmul") [e1; e2]
let bvudiv e1 e2 = mk_t_app (mk_id_simple "bvudiv") [e1; e2]
let bvurem e1 e2 = mk_t_app (mk_id_simple "bvurem") [e1; e2]
let bvsdiv e1 e2 = mk_t_app (mk_id_simple "bvsdiv") [e1; e2]
let bvsrem e1 e2 = mk_t_app (mk_id_simple "bvsrem") [e1; e2]
let bvshl e1 e2 = mk_t_app (mk_id_simple "bvshl") [e1; e2]
let bvlshr e1 e2 = mk_t_app (mk_id_simple "bvlshr") [e1; e2]
let bvashr e1 e2 = mk_t_app (mk_id_simple "bvashr") [e1; e2]
let bvult e1 e2 = mk_t_app (mk_id_simple "bvult") [e1; e2]
let bvule e1 e2 = mk_t_app (mk_id_simple "bvule") [e1; e2]
let bvugt e1 e2 = mk_t_app (mk_id_simple "bvugt") [e1; e2]
let bvuge e1 e2 = mk_t_app (mk_id_simple "bvuge") [e1; e2]
let bvslt e1 e2 = mk_t_app (mk_id_simple "bvslt") [e1; e2]
let bvsle e1 e2 = mk_t_app (mk_id_simple "bvsle") [e1; e2]
let bvsgt e1 e2 = mk_t_app (mk_id_simple "bvsgt") [e1; e2]
let bvsge e1 e2 = mk_t_app (mk_id_simple "bvsge") [e1; e2]
let bvrotr i e1 = mk_t_app (mk_id_indexed "rotate_right" [i]) [e1]
let bvrotl i e1 = mk_t_app (mk_id_indexed "rotate_left" [i]) [e1]

let clz (width: int) x = if width < 1 then raise (Invalid_argument "width needs to be at least 1") else
  let gen_hex i = List.init width ~f:(fun i' -> (Stdlib.(=) i i')) in
  let f = (fun (i', expr) -> if Stdlib.(=) i' 0 then (i', width |> mk_num) else ((Stdlib.(-) i' 1), (ite (bvuge x (gen_hex (Stdlib.(-) (Stdlib.(-) width 1) i') |> mk_bin)) (i' |> mk_num) expr))) in
  let out = Fn.apply_n_times ~n:width f (width, mk_num width) |> snd in
  ite (bvuge x (gen_hex (Stdlib.(-) width 1) |> mk_bin)) (mk_bin (List.init width ~f:(fun i -> Stdlib.(=) i 0))) out

let clo width x = clz width (bvnot x)

let cls width x = ite (bvsle x (mk_bin (List.init width ~f:(fun i -> Stdlib.(=) i (Stdlib.(-) width 1))))) (clo width x) (clz width x)

let popcnt width x = if width < 1 then raise (Invalid_argument "width needs to be at least 1") else
  Fn.apply_n_times ~n:width (fun (i, expr) -> (Stdlib.(+) i 1, bvadd expr (zero_extend (mk_index_num (Stdlib.(-) width 1)) (extract (mk_index_num i) (mk_index_num i) x)))) (0, mk_num 0) |> snd

let bvrev width x = if Stdlib.(=) (width % 2) 1 then (raise (Invalid_argument "width needs to be even")) else
  Fn.apply_n_times ~n:width (fun (i, expr) -> ((Stdlib.(-) i 1), (if (Stdlib.(=) i width) then expr else concat (extract (mk_index_num (Stdlib.(-) i 1)) (mk_index_num (Stdlib.(-) i 1)) x) expr)))
    (width, extract (mk_index_num (Stdlib.(-) width 1)) (mk_index_num (Stdlib.(-) width 1)) x) |> snd

(* Comparisons *)
let ( > ) e1 e2 = mk_t_app (mk_id_simple ">") [ e1; e2 ]
let ( >= ) e1 e2 = mk_t_app (mk_id_simple ">=") [ e1; e2 ]
let ( < ) e1 e2 = mk_t_app (mk_id_simple ">") [ e1; e2 ]
let ( <= ) e1 e2 = mk_t_app (mk_id_simple "<=") [ e1; e2 ]
let ( = ) e1 e2 = mk_t_app (mk_id_simple "=") [ e1; e2 ]

(* Grmmar *)
let gconst sort = mk_g_constant sort
let gterm t = mk_g_term t
let gvar sort = mk_g_var sort

let gblock (name : symbol) (sort : sygus_sort) (tl : sygus_gsterm list) : grammar_def =
  [ (dummy_loc, name, sort), tl ]
;;

(* Predefined Sorts *)
let int_sort = sort "Int"
let bool_sort = sort "Bool"
let real_sort = sort "Real"
let bv_sort width = mk_sort (mk_id_indexed (mk_symbol "BitVec") [mk_index_num width])
let width_of_bv = function
  | SId (_, IdIndexed (_, sym, [index])) -> if Stdlib.(=) sym "BitVec" then (match index with
    | INum (_ , width) -> width
    | _ -> raise (Invalid_argument "expected index to be an integer")) else raise (Invalid_argument "symbol was not 'BitVector'")
  | _ -> raise (Invalid_argument "symbol was not 'BitVector'")