open Syguslib
open Solvers
open Sygus
open Expressions
open Serializer
open Sexplib.Sexp

module TestLog : Logger = struct
  let error f = Fmt.(pf stdout "%a@." f ())
  let debug f = Fmt.(pf stdout "%a@." f ())
  let verb f = Fmt.(pf stdout "%a@." f ())
  let log_file = "unknown"
  let verbose = true
  let log_queries = false
end

module Config : SolverSystemConfig = struct
  let cvc_binary_path () = "cvc4"
  let dryadsynth_binary_path () = "dryadsynth"
  let eusolver_binary_path () = "eusolver"
  let using_cvc5 () = false
end

(* Test the asynchronous solver using Lwt. *)
module LSolver = LwtSolver (NoStat) (TestLog) (Config)

let test_lwt_solver input_file =
  Fmt.(pf stdout "SOLVE TEST %s ..@." input_file);
  let program = Parser.sexp_parse input_file in
  let opt_task, u = LSolver.solve_commands ?solver_kind:(Some CVC) program in
  Lwt.wakeup u 0;
  match Lwt_main.run opt_task with
  | Some (RSuccess ((s, _, _, _) :: _)) ->
    Fmt.(pf stdout "Solution to: %s.@." s);
    Fmt.(pf stdout "OK@.")
  | Some _ | None -> Fmt.(pf stdout "FAILED@.")
;;

test_lwt_solver "../../../test/inputs/positive/alist_sum.sl";
test_lwt_solver "../../../test/inputs/positive/bench_msshom2b1774.sl";
test_lwt_solver "../../../test/inputs/positive/bench_msshom931e5a.sl"

(* Test the synchronous solver. *)
module SSolver = SyncSolver (NoStat) (TestLog) (Config)

let test_sync_solver input_file =
  Fmt.(pf stdout "SOLVE TEST %s ..@." input_file);
  let program = Parser.sexp_parse input_file in
  match SSolver.solve_commands ~solver_kind:CVC program with
  | RSuccess ((s, _, _, _) :: _) ->
    Fmt.(pf stdout "Solution to: %s.@." s);
    Fmt.(pf stdout "OK@.")
  | _ -> Fmt.(pf stdout "FAILED@.")
;;

test_sync_solver "../../../test/inputs/positive/alist_sum.sl";
test_sync_solver "../../../test/inputs/positive/bench_msshom2b1774.sl";
test_sync_solver "../../../test/inputs/positive/bench_msshom931e5a.sl"

(** Test function for any bitvector macro.
  We test a macro {!f} by trying to synthesise a constant {!x} s.t.
  {!f x = e}, where {!e} is some expected value.
  So for instance, if we test the clz macro with expected value 2, one possible solution could be #b00100000
  Since different macros obviously have different semantics, we pass the resulting {!type:Sygus.sygus_term} to a verifier function
  that then evalutes if the result is correct.
*)
let macro_test width macro_name macro_fun expected verifier =
  let macro_term = macro_fun width (mk_t_id (mk_id_simple "x")) in
  let macro_define_fun = mk_c_define_fun macro_name [mk_sorted_var "x" (bv_sort width)] (bv_sort width) macro_term in
  let constant_grammar = Some ([mk_sorted_var "S" (bv_sort width), [mk_g_constant (bv_sort width)]]) in
  let constant_synth_fun = mk_c_synth_fun ~g:constant_grammar "synth_const" [] (bv_sort width) in
  let constr = mk_c_constraint (hex_of_int width expected = mk_t_app (mk_id_simple macro_name) [mk_t_id (mk_id_simple "synth_const")]) in
  let program = [mk_c_set_logic "BV"; macro_define_fun; constant_synth_fun; constr; mk_c_check_synth ()] in
  Fmt.(pf stdout "MACRO TEST WITH %s and expected result %i..@." macro_name expected);
  match SSolver.solve_commands ~solver_kind:CVC program with
  | RSuccess ((s, _, _, x) :: _) ->
    Fmt.(pf stdout "Solution to: %s %s.@." s (to_string_hum (sexp_of_sygus_term x)));
    if verifier width x expected then Fmt.(pf stdout "OK@.") else Fmt.(pf stdout "ERROR@.")
  | _ -> Fmt.(pf stdout "FAILED@.")
;;

let clz_verifier _ term expected = match term with
  | SyLit (_, LitBin (_, l)) -> let lz = Base.List.take_while l ~f:(fun x -> Stdlib.(not) x) |> List.length in if Stdlib.(=) lz expected then true
  else (Fmt.(pf stdout "expected %i, got %i ..@." expected lz); false)
  | _ -> Fmt.(pf stdout "expected binary constant, got %s ..@." (to_string_hum (sexp_of_sygus_term term))); false;;

let clo_verifier _ term expected = match term with
  | SyLit (_, LitBin (_, l)) -> let lo = Base.List.take_while l ~f:(fun x -> x) |> List.length in if Stdlib.(=) lo expected then true
  else (Fmt.(pf stdout "expected %i, got %i ..@." expected lo); false)
  | _ -> Fmt.(pf stdout "expected binary constant, got %s ..@." (to_string_hum (sexp_of_sygus_term term))); false;;

let cls_verifier width term expected = match term with
  | SyLit (_, LitBin (_, l)) -> if List.hd l then clo_verifier width term expected else clz_verifier width term expected
  | _ -> Fmt.(pf stdout "expected binary constant, got %s ..@." (to_string_hum (sexp_of_sygus_term term))); false;;

let popcnt_verifier _ term expected = match term with
  | SyLit (_, LitBin (_, l)) -> let cnt = Base.List.count l ~f:(fun x -> x) in if Stdlib.(=) cnt expected then true
  else (Fmt.(pf stdout "expected %i, got %i ..@." expected cnt); false)
  | _ -> Fmt.(pf stdout "expected binary constant, got %s ..@." (to_string_hum (sexp_of_sygus_term term))); false;;

let rev_verifier width term expected = 
  let as_bitlist = List.init width (fun i -> Stdlib.(=) (Stdlib.(/) expected (Base.Int.pow 2 i) mod 2) 1) in
  let pp_bit = Fmt.using (fun b -> if b then '1' else '0') Fmt.char in
  match term with 
  | SyLit (_, LitBin (_, l)) -> if (Stdlib.(=) as_bitlist l) then true 
    else (Fmt.(pf stdout "expected #b%a, got #b%a ..@." (Fmt.list ~sep:Fmt.nop pp_bit) as_bitlist (Fmt.list ~sep:Fmt.nop pp_bit) l); false)
  | _ -> Fmt.(pf stdout "expected binary constant, got %s ..@." (to_string_hum (sexp_of_sygus_term term))); false;;

List.init 8 (fun i -> macro_test 8 "clz8" clz i clz_verifier);;
List.init 8 (fun i -> macro_test 8 "clo8" clo i clo_verifier);;
List.init 8 (fun i -> macro_test 8 "cls8" cls i cls_verifier);;
List.init 8 (fun i -> macro_test 8 "popcnt8" popcnt i popcnt_verifier);;
List.init 8 (fun i -> macro_test 8 "rev8" bvrev i rev_verifier);;
