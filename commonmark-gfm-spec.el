;;; commonmark-gfm-spec.el --- Spec test helpers for commonmark-gfm -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT bridge for CommonMark/GFM JSON spec examples.

;;; Code:

(require 'ert)
(require 'json)
(require 'subr-x)
(require 'commonmark-gfm)

(defun commonmark-gfm-spec-read-json (file)
  "Read CommonMark/GFM JSON spec examples from FILE."
  (let ((json-array-type 'list)
        (json-object-type 'alist)
        (json-key-type 'symbol))
    (json-read-file file)))

(defun commonmark-gfm-spec--replace-tabs (text)
  "Replace spec visible tab markers in TEXT with literal tabs."
  (replace-regexp-in-string "→" "\t" text t t))

(defun commonmark-gfm-spec-read-text (file)
  "Read CommonMark/GFM side-by-side examples from spec text FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n"))
          (section "Unknown")
          (example-number 0)
          examples)
      (while lines
        (let ((line (pop lines)))
          (cond
           ((string-match "\\`#+[ \t]+\\(.*\\)\\'" line)
            (setq section (match-string 1 line)))
           ((string-match "\\`\\(`\\{3,\\}\\)[ \t]+example\\(?:[ \t]+\\(.*\\)\\)?[ \t]*\\'" line)
            (let ((fence (match-string 1 line))
                  (example-section (or (match-string 2 line) section))
                  markdown
                  html
                  (target 'markdown)
                  done)
              (while (and lines (not done))
                (let ((example-line (pop lines)))
                  (cond
                   ((string= example-line fence)
                    (setq done t))
                   ((string= example-line ".")
                    (setq target 'html))
                   ((eq target 'markdown)
                    (push (concat example-line "\n") markdown))
                   (t
                    (push (concat example-line "\n") html)))))
              (setq example-number (1+ example-number))
              (push `((example . ,example-number)
                      (section . ,(string-trim example-section))
                      (markdown . ,(commonmark-gfm-spec--replace-tabs
                                    (apply #'concat (nreverse markdown))))
                      (html . ,(commonmark-gfm-spec--replace-tabs
                                (apply #'concat (nreverse html)))))
                    examples))))))
      (nreverse examples))))

(defun commonmark-gfm-spec-read-file (file)
  "Read CommonMark/GFM examples from JSON or side-by-side spec FILE."
  (if (string-suffix-p ".json" file t)
      (commonmark-gfm-spec-read-json file)
    (commonmark-gfm-spec-read-text file)))

(defconst commonmark-gfm-spec-gfm-commonmark-sections
  '("Autolinks" "HTML blocks")
  "Spec sections checked in CommonMark mode for cmark-gfm spec text.")

(defun commonmark-gfm-spec-gfm-options (example)
  "Return render options for EXAMPLE from cmark-gfm spec text.
Examples that conflict with always-on GFM extensions are checked with GFM
disabled.  Other examples use the default GFM-enabled mode."
  (when (member (alist-get 'section example)
                commonmark-gfm-spec-gfm-commonmark-sections)
    '(:gfm nil)))

(defun commonmark-gfm-spec--slug (text)
  "Return a small test-name slug for TEXT."
  (let* ((text (downcase (or text "unknown")))
         (text (replace-regexp-in-string "[^a-z0-9]+" "-" text)))
    (string-trim text "-" "-")))

(defun commonmark-gfm-spec--test-symbol (suite section example)
  "Return an ERT test symbol for SUITE SECTION and EXAMPLE."
  (intern
   (format "commonmark-gfm-spec/%s/%04d-%s"
           (commonmark-gfm-spec--slug (format "%s" suite))
           example
           (commonmark-gfm-spec--slug section))))

(defun commonmark-gfm-spec-run-file (file &optional options)
  "Run examples from spec JSON FILE and return a result plist.
The result has `:total', `:passed', and `:failed' keys.  `:failed' contains
the original example alists that did not match expected HTML.
OPTIONS is passed to `commonmark-gfm-render-to-html'.  If OPTIONS is a
function, it is called with each example and should return the options for
that example."
  (let ((total 0)
        (passed 0)
        failed)
    (dolist (example (commonmark-gfm-spec-read-file file))
      (setq total (1+ total))
      (let ((markdown (alist-get 'markdown example))
            (expected (alist-get 'html example))
            (example-options (if (functionp options)
                                 (funcall options example)
                               options)))
        (if (string= expected
                     (commonmark-gfm-render-to-html
                      markdown
                      example-options))
            (setq passed (1+ passed))
          (push example failed))))
    (list :total total
          :passed passed
          :failed (nreverse failed))))

;;;###autoload
(defun commonmark-gfm-spec-report-file (file)
  "Report CommonMark/GFM JSON spec results for FILE."
  (interactive "fSpec file: ")
  (let* ((result (commonmark-gfm-spec-run-file file))
         (total (plist-get result :total))
         (passed (plist-get result :passed))
         (failed (length (plist-get result :failed))))
    (message "commonmark-gfm: %d/%d examples passed, %d failed"
             passed total failed)
    result))

;;;###autoload
(defun commonmark-gfm-spec-define-tests (file &optional suite)
  "Define ERT tests for CommonMark/GFM examples from FILE.

FILE should be a JSON file in the format published by the CommonMark project,
or the side-by-side spec text format used by cmark-gfm.
SUITE is used only in generated test names."
  (interactive "fSpec file: ")
  (let ((suite (or suite 'commonmark)))
    (dolist (example (commonmark-gfm-spec-read-file file))
      (let* ((number (alist-get 'example example))
             (section (alist-get 'section example))
             (markdown (alist-get 'markdown example))
             (html (alist-get 'html example))
             (name (commonmark-gfm-spec--test-symbol suite section number)))
        (eval
         `(ert-deftest ,name ()
            ,(format "Spec example %s from %s." number file)
            (should (string= ,html
                             (commonmark-gfm-render-to-html ,markdown)))))))))

(provide 'commonmark-gfm-spec)

;;; commonmark-gfm-spec.el ends here
