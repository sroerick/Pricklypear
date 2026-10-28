#!/bin/ksh
# treecalc acceptance vectors. Requires PP_URL/PP_USER/PP_PASS and pp-eval.
set -u
PP_EVAL=${PP_EVAL:-pp-eval}
pass=0; fail=0

chk() { # chk NAME EXPECTED EXPR
  name=$1; want=$2; expr=$3
  out=$($PP_EVAL -j "$expr" 2>/dev/null)
  got=$(printf "%s" "$out" | sed -n 's/.*"value":"\(.*\)".*/\1/p' | tr -d '\\')
  if [ "$got" = "$want" ]; then
    pass=$((pass+1)); echo "PASS $name"
  else
    fail=$((fail+1)); echo "FAIL $name: want [$want] got [$got] raw [$out]"
  fi
}

V='(cdr (car (cdr (cdr R))))'   # tc/eval result -> value tree
OM='(let ((M (tc/app (tc/app tc/s tc/i) tc/i))) (tc/app M M))'

chk k_leaf_leaf "0" \
 "(let ((R (tc/eval (tc/from-text \"t t t t\")))) (tc/to-ternary $V))"
chk rule1_extra_args "10" \
 "(let ((R (tc/eval (tc/from-text \"t t t t t\")))) (tc/to-ternary $V))"
chk identity_self_zero_steps "0" \
 "(let ((R (tc/eval (tc/from-ternary \"21100\")))) (cdr (car (cdr R))))"
chk ternary_roundtrip_stem "10" "(tc/to-ternary (tc/from-ternary \"10\"))"
chk ternary_roundtrip_fork "2100" "(tc/to-ternary (tc/from-ternary \"2100\"))"
chk skk_is_identity "1110" \
 "(let ((R (tc/eval-arg (tc/app (tc/app tc/s tc/k) tc/k) (tc/nat 3)))) (tc/to-ternary $V))"
chk k_a_b_is_a "110" \
 "(let ((R (tc/eval-arg (tc/app tc/k (tc/nat 2)) (tc/nat 1)))) (tc/to-ternary $V))"
chk iszero_0_true "10" \
 "(let ((R (tc/eval (tc/is-zero (tc/nat 0))))) (tc/to-ternary $V))"
chk iszero_3_false "2021100" \
 "(let ((R (tc/eval (tc/is-zero (tc/nat 3))))) (tc/to-ternary $V))"
chk rule_3a_leaf_case "10" \
 "(let ((R (tc/eval (tc/app (tc/triage (tc/nat 1) (tc/nat 2) (tc/nat 3)) (tc/from-ternary \"0\"))))) (tc/to-ternary $V))"
chk rule_3b_stem_case "0" \
 "(let ((R (tc/eval (tc/app (tc/triage (tc/nat 1) tc/i (tc/nat 3)) (tc/from-ternary \"10\"))))) (tc/to-ternary $V))"
chk rule_3c_fork_case "2210012100" \
 "(let ((R (tc/eval (tc/app (tc/triage (tc/nat 1) (tc/nat 2) (tc/nat 4)) (tc/from-ternary \"21100\"))))) (tc/to-ternary $V))"
chk step_reports_rule_1 "1" \
 "(cdr (car (tc/step (tc/from-text \"t t t t\"))))"
chk trace_single_step "1" \
 "(length (tc/trace (tc/app (tc/app tc/k (tc/nat 1)) (tc/nat 2))))"
chk omega_out_of_steps_at_default "[\"out-of-steps\",4000]" \
 "(let ((R (tc/eval $OM))) (list (cdr (car R)) (cdr (car (cdr R)))))"
chk omega_out_of_steps_exactly_300 "[\"out-of-steps\",300]" \
 "(let ((R (tc/eval-limit $OM 300))) (list (cdr (car R)) (cdr (car (cdr R)))))"
chk minbin_roundtrip_leaf "1" "(tc/to-minbin (tc/from-minbin \"1\"))"
chk minbin_roundtrip_identity "001010111" "(tc/to-minbin (tc/from-minbin \"001010111\"))"
chk minbin_roundtrip_ttt "00111" "(tc/to-minbin (tc/from-minbin \"00111\"))"
chk minbin_decode_identity "21100" "(tc/to-ternary (tc/from-minbin \"001010111\"))"
chk minbin_decode_ttt "200" "(tc/to-ternary (tc/from-minbin \"00111\"))"
chk fromtext_roundtrip "t (t t) t" "(tc/to-text (tc/from-text \"(t (t t)) t\"))"
chk size_identity_5 "5" "(tc/size (tc/from-ternary \"21100\"))"
chk s4_compiled_k_behaves_as_k "110" \
 "(let ((R (tc/eval-arg (tc/app (tc/from-ternary \"212120020021100\") (tc/nat 2)) (tc/nat 1)))) (tc/to-ternary $V))"
chk s4_compiled_I_selfapp_terminates "21100" \
 "(let ((R (tc/eval (tc/app (tc/from-ternary \"21100\") (tc/from-ternary \"21100\"))))) (tc/to-ternary $V))"
chk s4_compiled_M_selfapp_diverges_300 "[\"out-of-steps\",300]" \
 "(let ((R (tc/eval-limit (tc/app (tc/from-ternary \"212110021100\") (tc/from-ternary \"212110021100\")) 300))) (list (cdr (car R)) (cdr (car (cdr R)))))"

echo "----"
echo "pass=$pass fail=$fail"
[ "$fail" = 0 ]
