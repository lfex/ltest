(defmodule ltest-beams-tests
  (behaviour ltest-unit))

(include-lib "include/ltest-macros.lfe")

(deftest app-ebin-dir
  (is-equal "_build/arthur/lib/dent/ebin/*.beam"
            (ltest-beams:app-ebin-dir 'arthur "dent")))

(deftest paths-no-args
  (let* ((results (ltest-beams:paths))
         (sorted (lists:sort (lists:map #'filename:basename/1 results)))
         (expected '("ltest-basic-tests.beam"
                     "ltest-beams-tests.beam"
                     "ltest-beams.beam"
                     "ltest-cancelled-tests.beam"
                     "ltest-color.beam"
                     "ltest-const.beam"
                     "ltest-fixture-tests.beam"
                     "ltest-fixturecase-tests.beam"
                     "ltest-formatter.beam"
                     "ltest-generated-tests.beam"
                     "ltest-integration.beam"
                     "ltest-listener.beam"
                     "ltest-named-tests.beam"
                     "ltest-runner.beam"
                     "ltest-system.beam"
                     "ltest-testset-tests.beam"
                     "ltest-unit.beam"
                     "ltest-util.beam"
                     "ltest.beam")))
    (is-equal expected sorted)))

(deftest paths-app
  (let* ((results (ltest-beams:paths 'lfe))
         (sorted (lists:sort (lists:map #'filename:basename/1 results)))
         (expected '("cl.beam"
                     "clj.beam"
                     "lfe.beam"
                     "lfe_abstract_code.beam"
                     "lfe_bits.beam"
                     "lfe_codegen.beam"
                     "lfe_comp.beam"
                     "lfe_doc.beam"
                     "lfe_edlin_expand.beam"
                     "lfe_env.beam"
                     "lfe_eval.beam"
                     "lfe_gen.beam"
                     "lfe_init.beam"
                     "lfe_internal.beam"
                     "lfe_io.beam"
                     "lfe_io_format.beam"
                     "lfe_io_pretty.beam"
                     "lfe_io_write.beam"
                     "lfe_lib.beam"
                     "lfe_lint.beam"
                     "lfe_macro.beam"
                     "lfe_macro_export.beam"
                     "lfe_macro_include.beam"
                     "lfe_macro_record.beam"
                     "lfe_ms.beam"
                     "lfe_parse.beam"
                     "lfe_pmod.beam"
                     "lfe_qlc.beam"
                     "lfe_scan.beam"
                     "lfe_shell.beam"
                     "lfe_trans.beam"
                     "lfe_types.beam"
                     "lfescript.beam")))
    (is-equal expected sorted)))

(deftest paths-apps
  (let* ((results (ltest-beams:paths '(ltest lfe)))
         (sorted (lists:sort (lists:map #'filename:basename/1 results)))
         (expected '("cl.beam"
                     "clj.beam"
                     "lfe.beam"
                     "lfe_abstract_code.beam"
                     "lfe_bits.beam"
                     "lfe_codegen.beam"
                     "lfe_comp.beam"
                     "lfe_doc.beam"
                     "lfe_edlin_expand.beam"
                     "lfe_env.beam"
                     "lfe_eval.beam"
                     "lfe_gen.beam"
                     "lfe_init.beam"
                     "lfe_internal.beam"
                     "lfe_io.beam"
                     "lfe_io_format.beam"
                     "lfe_io_pretty.beam"
                     "lfe_io_write.beam"
                     "lfe_lib.beam"
                     "lfe_lint.beam"
                     "lfe_macro.beam"
                     "lfe_macro_export.beam"
                     "lfe_macro_include.beam"
                     "lfe_macro_record.beam"
                     "lfe_ms.beam"
                     "lfe_parse.beam"
                     "lfe_pmod.beam"
                     "lfe_qlc.beam"
                     "lfe_scan.beam"
                     "lfe_shell.beam"
                     "lfe_trans.beam"
                     "lfe_types.beam"
                     "lfescript.beam"
                     "ltest-basic-tests.beam"
                     "ltest-beams-tests.beam"
                     "ltest-beams.beam"
                     "ltest-cancelled-tests.beam"
                     "ltest-color.beam"
                     "ltest-const.beam"
                     "ltest-fixture-tests.beam"
                     "ltest-fixturecase-tests.beam"
                     "ltest-formatter.beam"
                     "ltest-generated-tests.beam"
                     "ltest-integration.beam"
                     "ltest-listener.beam"
                     "ltest-named-tests.beam"
                     "ltest-runner.beam"
                     "ltest-system.beam"
                     "ltest-testset-tests.beam"
                     "ltest-unit.beam"
                     "ltest-util.beam"
                     "ltest.beam")))
    (is-equal expected sorted)))
