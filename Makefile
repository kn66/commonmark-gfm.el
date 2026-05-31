EMACS ?= emacs
BYTE_COMPILE_DIR ?= /tmp/commonmark-gfm-byte-compile
SPEC ?= test/spec-smoke.json
GFM_SPEC ?= test/gfm-smoke.json
GFM_FULL_SPEC ?= /tmp/cmark-gfm-spec.txt

BATCH = $(EMACS) --batch -Q \
	--eval "(setq load-prefer-newer t)" \
	--eval "(setq native-comp-jit-compilation nil)" \
	--eval "(setq native-comp-enable-subr-trampolines nil)" \
	-L .

SOURCES = commonmark-gfm-ast.el \
	commonmark-gfm-inline.el \
	commonmark-gfm-block.el \
	commonmark-gfm-html.el \
	commonmark-gfm.el \
	commonmark-gfm-spec.el

TESTS = test/commonmark-gfm-test.el

.PHONY: check test spec gfm-spec gfm-full-spec compile clean-elc

check: test spec gfm-spec compile

test:
	$(BATCH) -l $(TESTS) -f ert-run-tests-batch-and-exit

spec:
	$(BATCH) -l commonmark-gfm-spec \
		--eval "(let* ((result (commonmark-gfm-spec-run-file \"$(SPEC)\")) (total (plist-get result :total)) (passed (plist-get result :passed)) (failed (length (plist-get result :failed)))) (princ (format \"commonmark-gfm spec: %d/%d examples passed, %d failed\n\" passed total failed)) (unless (= passed total) (kill-emacs 1)))"

gfm-spec:
	$(BATCH) -l commonmark-gfm-spec \
		--eval "(let* ((result (commonmark-gfm-spec-run-file \"$(GFM_SPEC)\")) (total (plist-get result :total)) (passed (plist-get result :passed)) (failed (length (plist-get result :failed)))) (princ (format \"commonmark-gfm gfm spec: %d/%d examples passed, %d failed\n\" passed total failed)) (unless (= passed total) (kill-emacs 1)))"

gfm-full-spec:
	$(BATCH) -l commonmark-gfm-spec \
		--eval "(let* ((result (commonmark-gfm-spec-run-file \"$(GFM_FULL_SPEC)\" #'commonmark-gfm-spec-gfm-options)) (total (plist-get result :total)) (passed (plist-get result :passed)) (failed (length (plist-get result :failed)))) (princ (format \"commonmark-gfm full gfm spec: %d/%d examples passed, %d failed\n\" passed total failed)))"

compile:
	$(BATCH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		--eval "(make-directory \"$(BYTE_COMPILE_DIR)\" t)" \
		--eval "(setq byte-compile-dest-file-function (lambda (file) (expand-file-name (concat (file-name-nondirectory file) \"c\") \"$(BYTE_COMPILE_DIR)\")))" \
		-f batch-byte-compile $(SOURCES) $(TESTS)

clean-elc:
	rm -f *.elc test/*.elc
	rm -rf -- "$(BYTE_COMPILE_DIR)"
