;;; commonmark-gfm-block.el --- Block parsing for commonmark-gfm -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Block parser.  This is still an incomplete CommonMark/GFM implementation,
;; but it follows the block/inline split and keeps each block feature isolated
;; enough to replace with stricter spec-compatible code.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'commonmark-gfm-ast)
(require 'commonmark-gfm-inline)

(defconst commonmark-gfm-block--html-block-tags
  '("address" "article" "aside" "base" "basefont" "blockquote" "body"
    "caption" "center" "col" "colgroup" "dd" "details" "dialog" "dir" "div"
    "dl" "dt" "fieldset" "figcaption" "figure" "footer" "form" "frame"
    "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hr" "html"
    "iframe" "legend" "li" "link" "main" "menu" "menuitem" "nav" "noframes"
    "ol" "optgroup" "option" "p" "param" "search" "section" "summary"
    "table" "tbody" "td" "tfoot" "th" "thead" "title" "tr" "track" "ul")
  "HTML block tags from the CommonMark HTML block start condition.")

(defconst commonmark-gfm-block--gfm-html-block-tags
  '("noembed" "plaintext" "xmp")
  "Additional GFM tagfilter tags that can start HTML blocks.")

(defconst commonmark-gfm-block--line-number-property
  'commonmark-gfm-line-number
  "Text property used to keep original source line numbers on line strings.")

(defconst commonmark-gfm-block--lazy-continuation-property
  'commonmark-gfm-lazy-continuation
  "Text property used for block quote lazy continuation lines.")

(defun commonmark-gfm-block--expand-tabs (line)
  "Expand leading tabs in LINE using CommonMark's four-column tab stops."
  (let ((column 0)
        (in-leading-indent t)
        chars)
    (mapc
     (lambda (char)
       (if (and in-leading-indent (= char ?\t))
           (let ((spaces (- 4 (% column 4))))
             (dotimes (_ spaces)
               (push ?\s chars))
             (setq column (+ column spaces)))
         (push char chars)
         (unless (memq char '(?\s ?\t))
           (setq in-leading-indent nil))
         (setq column (1+ column))))
     line)
    (apply #'string (nreverse chars))))

(defun commonmark-gfm-block--line-with-number (line number)
  "Return LINE tagged with original source line NUMBER."
  (let ((line (copy-sequence line)))
    (unless (string-empty-p line)
      (add-text-properties
       0 (length line)
       (list commonmark-gfm-block--line-number-property number)
       line))
    line))

(defun commonmark-gfm-block--line-number (line)
  "Return original source line number for LINE, or nil."
  (and (> (length line) 0)
       (get-text-property 0 commonmark-gfm-block--line-number-property line)))

(defun commonmark-gfm-block--with-source-line (text source)
  "Return TEXT tagged with SOURCE's original source line number."
  (if-let ((number (commonmark-gfm-block--line-number source)))
      (commonmark-gfm-block--line-with-number text number)
    text))

(defun commonmark-gfm-block--lazy-continuation-line-p (line)
  "Return non-nil when LINE came from a lazy block quote continuation."
  (and (> (length line) 0)
       (get-text-property 0 commonmark-gfm-block--lazy-continuation-property
                          line)))

(defun commonmark-gfm-block--mark-lazy-continuation (line)
  "Return LINE tagged as a lazy block quote continuation."
  (let ((line (copy-sequence line)))
    (unless (string-empty-p line)
      (add-text-properties
       0 (length line)
       (list commonmark-gfm-block--lazy-continuation-property t)
       line))
    line))

(defun commonmark-gfm-block--sourcepos-for-lines (lines)
  "Return a source position for LINES, or nil.
The shape is ((START-LINE START-COLUMN) (END-LINE END-COLUMN))."
  (let ((start-line (cl-find-if #'commonmark-gfm-block--line-number lines))
        (end-line (cl-find-if #'commonmark-gfm-block--line-number
                              (reverse lines))))
    (when (and start-line end-line)
      `((,(commonmark-gfm-block--line-number start-line) 1)
        (,(commonmark-gfm-block--line-number end-line)
         ,(max 1 (length end-line)))))))

(defun commonmark-gfm-block--sourcepos-for-vector-range (lines start end)
  "Return source position for LINES between START inclusive and END exclusive."
  (commonmark-gfm-block--sourcepos-for-lines
   (cl-loop for index from start below end
            collect (aref lines index))))

(defun commonmark-gfm-block--inline-parse-at (text line column)
  "Parse inline TEXT starting at one-based source LINE and COLUMN."
  (let ((text (substring-no-properties text)))
    (if line
        (commonmark-gfm-inline-parse-with-sourcepos text line (or column 1))
      (commonmark-gfm-inline-parse text))))

(defun commonmark-gfm-block--inline-parse-lines (lines)
  "Parse inline content formed by joining LINES with newlines."
  (let* ((text (mapconcat #'identity lines "\n"))
         (first-line (cl-find-if #'commonmark-gfm-block--line-number lines))
         (line-number (and first-line
                           (commonmark-gfm-block--line-number first-line))))
    (commonmark-gfm-block--inline-parse-at text line-number 1)))

(defun commonmark-gfm-block--split-lines (text)
  "Split TEXT into logical input lines without trailing newlines."
  (let ((lines (split-string text "\n"))
        (number 1)
        result)
    (when (and (string-suffix-p "\n" text)
               lines
               (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    (dolist (line lines (nreverse result))
      (push (commonmark-gfm-block--line-with-number
             (commonmark-gfm-block--expand-tabs
              (if (string-suffix-p "\r" line)
                  (substring line 0 -1)
                line))
             number)
            result)
      (setq number (1+ number)))))

(defun commonmark-gfm-block--reference-opener (line)
  "Return (NORMALIZED-LABEL . REST) when LINE starts a reference definition."
  (when (string-match
         "\\`[ \t]\\{0,3\\}\\[\\(\\(?:\\\\.\\|[^]\n]\\)+\\)\\]:[ \t]*\\(.*\\)\\'"
         line)
    (let* ((label (match-string 1 line))
           (rest (match-string 2 line))
           (normalized (commonmark-gfm-inline-normalize-reference-label
                        label)))
      (unless (or (string-empty-p normalized)
                  (string-match-p "\\(?:\\`\\|[^\\]\\)\\[" label))
        (cons normalized rest)))))

(defun commonmark-gfm-block--reference-opener-at (lines index)
  "Return (NORMALIZED-LABEL REST NEXT-INDEX) for a reference opener at INDEX."
  (let ((line (nth index lines)))
    (or
     (when-let ((opener (commonmark-gfm-block--reference-opener line)))
       (list (car opener) (cdr opener) (1+ index)))
     (when (string-match "\\`[ \t]\\{0,3\\}\\[\\(.*\\)\\'" line)
       (let ((label-parts (list (match-string 1 line)))
             (line-count (length lines))
             (cursor (1+ index))
             found)
         (while (and (< cursor line-count) (not found))
           (let ((candidate (nth cursor lines)))
             (if (commonmark-gfm-block--blank-line-p candidate)
                 (setq cursor line-count)
               (if (string-match "\\`\\(.*\\)\\]:[ \t]*\\(.*\\)\\'"
                                 candidate)
                   (setq label-parts
                         (append label-parts
                                 (list (match-string 1 candidate)))
                         found (list (match-string 2 candidate)
                                     (1+ cursor)))
                 (setq label-parts (append label-parts
                                           (list candidate)))))
             (setq cursor (1+ cursor))))
         (when found
           (let ((normalized
                  (commonmark-gfm-inline-normalize-reference-label
                   (mapconcat #'identity label-parts "\n"))))
             (unless (string-empty-p normalized)
               (list normalized (car found) (cadr found))))))))))

(defun commonmark-gfm-block--reference-complete-title (text)
  "Return stripped title for TEXT when it is a complete title, or nil."
  (commonmark-gfm-inline--strip-title text))

(defun commonmark-gfm-block--reference-title-close-char (text)
  "Return title close delimiter when TEXT starts a title, or nil."
  (let ((text (string-trim-left text)))
    (and (> (length text) 0)
         (commonmark-gfm-inline--title-close-char (aref text 0)))))

(defun commonmark-gfm-block--reference-title-at (lines index initial)
  "Parse a reference title from INITIAL and following LINES.
INDEX is the next unread line.  Return (TITLE . NEXT-INDEX), or nil."
  (let* ((initial (string-trim-left initial))
         (close (commonmark-gfm-block--reference-title-close-char initial))
         (complete (commonmark-gfm-block--reference-complete-title initial)))
    (cond
     (complete
      (cons complete index))
     ((not close)
      nil)
     (t
      (let ((title (substring initial 1))
            (line-count (length lines))
            done)
        (while (and (< index line-count) (not done))
          (let ((line (nth index lines)))
            (if (commonmark-gfm-block--blank-line-p line)
                (setq index line-count)
              (let ((right (string-trim-right line)))
                (setq title (concat title "\n" right))
                (setq index (1+ index))
                (when (and (> (length right) 0)
                           (= (aref right (1- (length right))) close))
                  (setq title (substring title 0 -1)
                        done t))))))
        (when done
          (cons title index)))))))

(defun commonmark-gfm-block--reference-destination-at (lines index rest)
  "Parse a reference destination from REST or following LINES.
Return (DESTINATION TITLE-SPEC NEXT-INDEX), or nil."
  (let ((line-count (length lines))
        (spec (string-trim rest)))
    (when (string-empty-p spec)
      (when (< index line-count)
        (let ((line (nth index lines)))
          (unless (commonmark-gfm-block--blank-line-p line)
            (setq spec (string-trim line)
                  index (1+ index))))))
    (pcase-let ((`(,destination . ,title-spec)
                 (commonmark-gfm-inline--split-destination-title spec)))
      (when destination
        (list destination title-spec index)))))

(defun commonmark-gfm-block--reference-title-line (line)
  "Return title attributes when LINE is a standalone reference title."
  (when (<= (commonmark-gfm-block--leading-spaces line) 3)
    (commonmark-gfm-inline--title-attr (string-trim line))))

(defun commonmark-gfm-block--reference-definition-at (lines index)
  "Return parsed reference definition at INDEX in LINES.
The return value is (DEFINITION . NEXT-INDEX), where DEFINITION is
\(NORMALIZED-LABEL . ATTRS)."
  (let ((opener (commonmark-gfm-block--reference-opener-at lines index)))
    (when opener
      (let* ((label (car opener))
             (rest (cadr opener))
             (next-index (caddr opener))
             (destination-at
              (commonmark-gfm-block--reference-destination-at
               lines next-index rest)))
        (when destination-at
          (pcase-let ((`(,destination ,title-spec ,after-destination)
                       destination-at))
            (let ((attrs `((destination . ,(commonmark-gfm-inline--decode-character-references
                                            (commonmark-gfm-inline--unescape-string
                                             destination)))))
                  (final-index after-destination)
                  title-at)
              (cond
               ((not (string-empty-p title-spec))
                (setq title-at
                      (commonmark-gfm-block--reference-title-at
                       lines after-destination title-spec))
                (when title-at
                  (setq attrs (append attrs
                                      `((title . ,(commonmark-gfm-inline--decode-character-references
                                                   (commonmark-gfm-inline--unescape-string
                                                    (car title-at))))))
                        final-index (cdr title-at))))
               ((< after-destination (length lines))
                (setq title-at
                      (commonmark-gfm-block--reference-title-at
                       lines
                       (1+ after-destination)
                       (nth after-destination lines)))
                (when title-at
                  (setq attrs (append attrs
                                      `((title . ,(commonmark-gfm-inline--decode-character-references
                                                   (commonmark-gfm-inline--unescape-string
                                                    (car title-at))))))
                        final-index (cdr title-at)))))
              (when (or (string-empty-p title-spec) title-at)
                (cons (cons label attrs) final-index)))))))))

(defun commonmark-gfm-block--line-reference-definition (line)
  "Return a reference definition from a single LINE, or nil."
  (car-safe (commonmark-gfm-block--reference-definition-at (list line) 0)))

(defun commonmark-gfm-block--container-line-reference-definition (line)
  "Return a reference definition nested directly in LINE, or nil."
  (when-let ((quoted-line (commonmark-gfm-block--blockquote-line line)))
    (commonmark-gfm-block--line-reference-definition quoted-line)))

(defun commonmark-gfm-block--reference-definition-can-start-p (body)
  "Return non-nil when a reference definition can start before BODY."
  (or (null body)
      (commonmark-gfm-block--blank-line-p (car body))
      (commonmark-gfm-block--block-start-p (car body))))

(defun commonmark-gfm-block--collect-reference-definitions (lines)
  "Collect top-level link reference definitions from LINES.
Return (REFERENCES . BODY-LINES).  The first definition for a normalized label
wins, matching CommonMark's reference definition precedence."
  (let ((index 0)
        (line-count (length lines))
        references
        body)
    (while (< index line-count)
      (let* ((line (nth index lines))
             (fence (commonmark-gfm-block--fence-opener line))
             (definition-at
              (and (not (commonmark-gfm-block--indented-code-line-p line))
                   (commonmark-gfm-block--reference-definition-can-start-p
                    body)
                   (commonmark-gfm-block--reference-definition-at
                    lines index)))
             (container-definition
              (and (not definition-at)
                   (commonmark-gfm-block--container-line-reference-definition
                    line))))
        (cond
         (fence
          (pcase-let ((`(,char ,length ,_info ,_indent) fence))
            (push line body)
            (setq index (1+ index))
            (while (and (< index line-count)
                        (not (commonmark-gfm-block--fence-closer-p
                              (nth index lines) char length)))
              (push (nth index lines) body)
              (setq index (1+ index)))
            (when (< index line-count)
              (push (nth index lines) body)
              (setq index (1+ index)))))
         (definition-at
          (let ((definition (car definition-at)))
            (unless (assoc (car definition) references)
              (push definition references))
            (setq index (cdr definition-at))))
         (container-definition
          (unless (assoc (car container-definition) references)
            (push container-definition references))
          (push line body)
          (setq index (1+ index)))
         (t
          (push line body)
          (setq index (1+ index))))))
    (cons (nreverse references) (nreverse body))))

(defun commonmark-gfm-block--blank-line-p (line)
  "Return non-nil when LINE is blank."
  (string-match-p "\\`[ \t]*\\'" line))

(defun commonmark-gfm-block--leading-spaces (line)
  "Return the number of leading spaces in LINE."
  (if (string-match "\\` *" line)
      (length (match-string 0 line))
    0))

(defun commonmark-gfm-block--html-block-tags-for-mode ()
  "Return HTML block tags for the active Markdown mode."
  (if commonmark-gfm-enable-gfm
      (append commonmark-gfm-block--gfm-html-block-tags
              commonmark-gfm-block--html-block-tags)
    commonmark-gfm-block--html-block-tags))

(defun commonmark-gfm-block--expand-tabs-from-column (text column)
  "Expand tabs in TEXT using COLUMN as the zero-based starting column."
  (let (chars)
    (mapc
     (lambda (char)
       (if (= char ?\t)
           (let ((spaces (- 4 (% column 4))))
             (dotimes (_ spaces)
               (push ?\s chars))
             (setq column (+ column spaces)))
         (push char chars)
         (setq column (1+ column))))
     text)
    (apply #'string (nreverse chars))))

(defun commonmark-gfm-block--trim-marker-prefix (line)
  "Return LINE without up to three leading spaces."
  (let ((spaces (commonmark-gfm-block--leading-spaces line)))
    (if (<= spaces 3)
        (substring line spaces)
      line)))

(defun commonmark-gfm-block--strip-atx-heading-tail (text)
  "Strip an optional ATX heading closing sequence from TEXT."
  (let ((trimmed (string-trim-right text)))
    (if (string-match "\\(?:\\`\\|[ \t]\\)#+[ \t]*\\'" trimmed)
        (string-trim-right (substring trimmed 0 (match-beginning 0)))
      trimmed)))

(defun commonmark-gfm-block--atx-heading (line)
  "Return (LEVEL . CONTENT) if LINE is an ATX heading."
  (when (string-match "\\`[ \t]\\{0,3\\}\\(#\\{1,6\\}\\)\\(?:[ \t]+\\|\\'\\)\\(.*\\)\\'" line)
    (cons (length (match-string 1 line))
          (commonmark-gfm-block--strip-atx-heading-tail
           (match-string 2 line)))))

(defun commonmark-gfm-block--atx-heading-content-column (line)
  "Return the one-based source column where ATX heading content starts."
  (if (string-match "\\`[ \t]\\{0,3\\}#\\{1,6\\}\\(?:[ \t]+\\|\\'\\)" line)
      (1+ (match-end 0))
    1))

(defun commonmark-gfm-block--setext-heading (line)
  "Return heading level when LINE is a setext heading underline."
  (let ((spaces (commonmark-gfm-block--leading-spaces line)))
    (when (and (<= spaces 3)
               (not (commonmark-gfm-block--lazy-continuation-line-p line))
               (string-match "\\` *\\(=+\\|-+\\)[ \t]*\\'" line))
      (if (= (aref (match-string 1 line) 0) ?=) 1 2))))

(defun commonmark-gfm-block--thematic-break-p (line)
  "Return non-nil when LINE is a thematic break."
  (let ((spaces (commonmark-gfm-block--leading-spaces line)))
    (when (<= spaces 3)
      (let* ((body (substring line spaces))
             (compact (replace-regexp-in-string "[ \t]" "" body)))
        (and (>= (length compact) 3)
             (memq (aref compact 0) '(?* ?- ?_))
             (cl-every (lambda (char) (= char (aref compact 0)))
                       compact))))))

(defun commonmark-gfm-block--fence-opener (line)
  "Return (CHAR LENGTH INFO INDENT) when LINE opens a fenced code block."
  (when (string-match "\\`[ \t]\\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\(.*\\)\\'" line)
    (let* ((indent (match-beginning 1))
           (marker (match-string 1 line))
           (char (aref marker 0))
           (info (match-string 2 line)))
      (unless (and (= char ?`) (string-match-p "`" info))
        (list char
              (length marker)
              (commonmark-gfm-inline--decode-character-references
               (commonmark-gfm-inline--unescape-string
                (string-trim info)))
              indent)))))

(defun commonmark-gfm-block--fence-closer-p (line char length)
  "Return non-nil when LINE closes a fence of CHAR and LENGTH."
  (string-match-p
   (format "\\`[ \t]\\{0,3\\}%s%s*[ \t]*\\'"
           (regexp-quote (make-string length char))
           (regexp-quote (char-to-string char)))
   line))

(defun commonmark-gfm-block--fence-state-after-line (state line)
  "Return fenced-code STATE after reading LINE."
  (if state
      (pcase-let ((`(,char ,length) state))
        (if (commonmark-gfm-block--fence-closer-p line char length)
            nil
          state))
    (when-let ((fence (commonmark-gfm-block--fence-opener line)))
      (list (car fence) (cadr fence)))))

(defun commonmark-gfm-block--indented-code-line-p (line)
  "Return non-nil when LINE starts an indented code line."
  (or (string-prefix-p "\t" line)
      (string-prefix-p "    " line)))

(defun commonmark-gfm-block--strip-code-indent (line)
  "Strip one Markdown code indentation level from LINE."
  (cond
   ((string-prefix-p "\t" line) (substring line 1))
   ((string-prefix-p "    " line) (substring line 4))
   (t line)))

(defun commonmark-gfm-block--strip-up-to-indent (line indent)
  "Strip up to INDENT leading spaces from LINE."
  (substring line (min indent (commonmark-gfm-block--leading-spaces line))))

(defun commonmark-gfm-block--html-block-start-p (line)
  "Return non-nil when LINE starts an HTML block handled by this parser."
  (let ((trimmed (commonmark-gfm-block--trim-marker-prefix line))
        (case-fold-search t))
    (or (string-prefix-p "<!--" trimmed)
        (string-prefix-p "<?" trimmed)
        (string-prefix-p "<!" trimmed)
        (string-prefix-p "<![CDATA[" trimmed)
        (string-match-p "\\`</?\\(?:script\\|pre\\|style\\|textarea\\)\\(?:[ \t>]\\|\\'\\)" trimmed)
        (string-match-p
         (format "\\`</?\\(?:%s\\)\\(?:[ \t>/]\\|\\'\\)"
                 (regexp-opt
                  (commonmark-gfm-block--html-block-tags-for-mode)))
         trimmed)
        (let ((end (commonmark-gfm-inline--html-inline-end trimmed 0)))
          (and end (= end (length trimmed)))))))

(defun commonmark-gfm-block--html-end-p (line start-line)
  "Return non-nil when LINE ends an HTML block started by START-LINE."
  (let ((trimmed (commonmark-gfm-block--trim-marker-prefix start-line))
        (case-fold-search t))
    (cond
     ((string-prefix-p "<!--" trimmed)
      (string-match-p "-->" line))
     ((string-prefix-p "<?" trimmed)
      (string-match-p "\\?>" line))
     ((string-prefix-p "<![CDATA[" trimmed)
      (string-match-p "]]>" line))
     ((string-prefix-p "<!" trimmed)
      (string-match-p ">" line))
     ((string-match "\\`</?\\(script\\|pre\\|style\\|textarea\\)\\(?:[ \t>]\\|\\'\\)" trimmed)
      (string-match-p
       (format "</%s[ \t]*>" (match-string 1 trimmed))
       line))
     (t
      (commonmark-gfm-block--blank-line-p line)))))

(defun commonmark-gfm-block--html-blank-terminated-p (start-line)
  "Return non-nil when START-LINE begins a blank-terminated HTML block."
  (let ((trimmed (commonmark-gfm-block--trim-marker-prefix start-line))
        (case-fold-search t))
    (not
     (or (string-prefix-p "<!--" trimmed)
         (string-prefix-p "<?" trimmed)
         (string-prefix-p "<!" trimmed)
         (string-prefix-p "<![CDATA[" trimmed)
         (string-match-p
          "\\`</?\\(?:script\\|pre\\|style\\|textarea\\)\\(?:[ \t>]\\|\\'\\)"
          trimmed)))))

(defun commonmark-gfm-block--html-paragraph-interrupt-p (line)
  "Return non-nil when LINE's HTML block may interrupt a paragraph."
  (let ((trimmed (commonmark-gfm-block--trim-marker-prefix line))
        (case-fold-search t))
    (or (string-prefix-p "<!--" trimmed)
        (string-prefix-p "<?" trimmed)
        (string-prefix-p "<!" trimmed)
        (string-prefix-p "<![CDATA[" trimmed)
        (string-match-p
         "\\`<\\(?:script\\|pre\\|style\\|textarea\\)\\(?:[ \t>]\\|\\'\\)"
         trimmed)
        (string-match-p
         (format "\\`<\\(?:%s\\)\\(?:[ \t>/]\\|\\'\\)"
                 (regexp-opt
                  (commonmark-gfm-block--html-block-tags-for-mode)))
         trimmed)
        (string-match-p
         (format "\\`</\\(?:%s\\)\\(?:[ \t>]\\|\\'\\)"
                 (regexp-opt
                  (cl-remove-if
                   (lambda (tag)
                     (member tag '("pre" "script" "style" "textarea")))
                   (commonmark-gfm-block--html-block-tags-for-mode))))
         trimmed))))

(defun commonmark-gfm-block--blockquote-line (line)
  "Return LINE without a blockquote marker, or nil if none is present."
  (let ((spaces (commonmark-gfm-block--leading-spaces line)))
    (when (and (<= spaces 3)
               (< spaces (length line))
               (= (aref line spaces) ?>))
      (let ((start (1+ spaces))
            (column (1+ spaces))
            (prefix ""))
        (cond
         ((and (< start (length line))
               (= (aref line start) ?\s))
          (setq start (1+ start)
                column (1+ column)))
         ((and (< start (length line))
               (= (aref line start) ?\t))
          (let ((spaces-to-tab (- 4 (% column 4))))
            (setq prefix (make-string (max 0 (1- spaces-to-tab)) ?\s)
                  start (1+ start)
                  column (+ column spaces-to-tab)))))
        (commonmark-gfm-block--with-source-line
         (concat prefix
                 (commonmark-gfm-block--expand-tabs-from-column
                  (substring line start)
                  column))
         line)))))

(defun commonmark-gfm-block--list-padding (text pos column)
  "Return (PADDING CONTENT) for list marker whitespace in TEXT.
POS is the index just after the marker, and COLUMN is the zero-based source
column at POS."
  (let ((index pos)
        (current-column column)
        (len (length text)))
    (while (and (< index len)
                (memq (aref text index) '(?\s ?\t)))
      (if (= (aref text index) ?\t)
          (setq current-column (+ current-column
                                  (- 4 (% current-column 4))))
        (setq current-column (1+ current-column)))
      (setq index (1+ index)))
    (let* ((columns (- current-column column))
           (padding (cond
                     ((= columns 0) 1)
                     ((<= columns 4) columns)
                     (t 1)))
           (leftover (if (> columns 4) (1- columns) 0)))
      (list padding
            (concat (make-string leftover ?\s)
                    (substring text index))))))

(defun commonmark-gfm-block--list-marker (line)
  "Return list marker data for LINE, or nil.
The return value is an alist with `type', `marker', `start', `content',
`content-indent', and `indent' keys."
  (let ((spaces (commonmark-gfm-block--leading-spaces line)))
    (when (<= spaces 3)
      (let ((rest (substring line spaces)))
        (cond
         ((string-match "\\`\\([-+*]\\)\\(?:\\([ \t]+\\)\\(.*\\)\\|\\'\\)" rest)
          (let* ((marker (match-string 1 rest))
                 (padding-content
                  (commonmark-gfm-block--list-padding rest 1 (1+ spaces)))
                 (padding (car padding-content))
                 (content (cadr padding-content)))
            `((type . bullet)
              (marker . ,marker)
              (start . nil)
              (indent . ,spaces)
              (content-indent . ,(+ spaces 1 padding))
              (content . ,content))))
         ((string-match "\\`\\([0-9]\\{1,9\\}\\)\\([.)]\\)\\(?:\\([ \t]+\\)\\(.*\\)\\|\\'\\)" rest)
          (let* ((number-text (match-string 1 rest))
                 (number (string-to-number number-text))
                 (marker (match-string 2 rest))
                 (marker-width (1+ (length number-text)))
                 (padding-content
                  (commonmark-gfm-block--list-padding
                   rest marker-width (+ spaces marker-width)))
                 (padding (car padding-content))
                 (content (cadr padding-content)))
            `((type . ordered)
              (marker . ,marker)
              (start . ,number)
              (indent . ,spaces)
              (content-indent . ,(+ spaces marker-width padding))
              (content . ,content)))))))))

(defun commonmark-gfm-block--same-list-marker-p (first candidate)
  "Return non-nil when CANDIDATE belongs to the same list as FIRST."
  (and candidate
       (eq (alist-get 'type first) (alist-get 'type candidate))
       (string= (alist-get 'marker first) (alist-get 'marker candidate))))

(defun commonmark-gfm-block--sibling-list-marker-p (first candidate)
  "Return non-nil when CANDIDATE is a sibling list marker for FIRST."
  (and (commonmark-gfm-block--same-list-marker-p first candidate)
       (< (alist-get 'indent candidate)
          (alist-get 'content-indent first))))

(defun commonmark-gfm-block--strip-continuation-indent (line indent)
  "Strip INDENT spaces from LINE when present."
  (if (>= (commonmark-gfm-block--leading-spaces line) indent)
      (substring line indent)
    line))

(defun commonmark-gfm-block--list-item-has-content-p (lines)
  "Return non-nil when LINES contain nonblank item content."
  (cl-some (lambda (line)
             (not (commonmark-gfm-block--blank-line-p line)))
           lines))

(defun commonmark-gfm-block--list-item-has-nested-list-p (lines)
  "Return non-nil when LINES already contain a nested list marker."
  (cl-some #'commonmark-gfm-block--list-marker lines))

(defun commonmark-gfm-block--next-nonblank-index (lines index line-count)
  "Return the next nonblank index in LINES at or after INDEX, or nil."
  (while (and (< index line-count)
              (commonmark-gfm-block--blank-line-p (aref lines index)))
    (setq index (1+ index)))
  (and (< index line-count) index))

(defun commonmark-gfm-block--table-split-row (line)
  "Split a GFM table row LINE into cell strings."
  (let ((pos 0)
        (len (length line))
        (cell nil)
        cells)
    (while (< pos len)
      (let ((char (aref line pos)))
        (cond
         ((and (= char ?\\) (< (1+ pos) len))
          (if (= (aref line (1+ pos)) ?|)
              (progn
                (setq pos (1+ pos))
                (push ?| cell))
            (push char cell)))
         ((= char ?|)
          (push (string-trim (apply #'string (nreverse cell))) cells)
          (setq cell nil))
         (t
          (push char cell)))
        (setq pos (1+ pos))))
    (push (string-trim (apply #'string (nreverse cell))) cells)
    (setq cells (nreverse cells))
    (when (and cells (string-empty-p (car cells)))
      (setq cells (cdr cells)))
    (when (and cells (string-empty-p (car (last cells))))
      (setq cells (butlast cells)))
    cells))

(defun commonmark-gfm-block--table-alignments (line)
  "Return table column alignments for delimiter LINE, or nil."
  (let ((cells (commonmark-gfm-block--table-split-row line))
        alignments)
    (when (and cells
               (cl-every
                (lambda (cell)
                  (string-match-p "\\`:?-+:?\\'" (string-trim cell)))
                cells))
      (dolist (cell cells)
        (let ((cell (string-trim cell)))
          (push (cond
                 ((and (string-prefix-p ":" cell)
                       (string-suffix-p ":" cell))
                  'center)
                 ((string-prefix-p ":" cell) 'left)
                 ((string-suffix-p ":" cell) 'right)
                 (t nil))
                alignments)))
      (nreverse alignments))))

(defun commonmark-gfm-block--make-table-row (line alignments header)
  "Return a table row node from LINE using ALIGNMENTS.
When HEADER is non-nil, mark cells as header cells."
  (let* ((raw-cells (commonmark-gfm-block--table-split-row line))
         (width (length alignments))
         cells)
    (dotimes (index width)
      (let ((text (or (nth index raw-cells) ""))
            (align (nth index alignments)))
        (push (commonmark-gfm-make-node
               'table-cell
               :attrs `((align . ,align)
                        (header . ,header))
               :children (commonmark-gfm-inline-parse text))
              cells)))
    (commonmark-gfm-make-node 'table-row
                              :attrs `((header . ,header))
                              :children (nreverse cells))))

(defun commonmark-gfm-block--block-start-p (line)
  "Return non-nil when LINE can start a new block."
  (or (commonmark-gfm-block--blank-line-p line)
      (commonmark-gfm-block--atx-heading line)
      (commonmark-gfm-block--fence-opener line)
      (commonmark-gfm-block--thematic-break-p line)
      (commonmark-gfm-block--html-block-start-p line)
      (commonmark-gfm-block--blockquote-line line)
      (commonmark-gfm-block--list-marker line)
      (commonmark-gfm-block--indented-code-line-p line)))

(defun commonmark-gfm-block--paragraph-interrupting-block-start-p (line)
  "Return non-nil when LINE can interrupt an existing paragraph."
  (let ((list-marker (commonmark-gfm-block--list-marker line)))
    (or (commonmark-gfm-block--atx-heading line)
        (commonmark-gfm-block--fence-opener line)
        (commonmark-gfm-block--thematic-break-p line)
        (commonmark-gfm-block--html-paragraph-interrupt-p line)
        (commonmark-gfm-block--blockquote-line line)
        (and list-marker
             (or (eq (alist-get 'type list-marker) 'bullet)
                 (= (or (alist-get 'start list-marker) 0) 1))
             (not (string-empty-p
                   (string-trim (alist-get 'content list-marker))))))))

(defun commonmark-gfm-block--lines-literal (lines)
  "Return code block literal for LINES."
  (if lines
      (concat (mapconcat #'identity lines "\n") "\n")
    ""))

(defun commonmark-gfm-block--trim-trailing-blank-lines (lines)
  "Return LINES without trailing blank lines."
  (let ((lines (copy-sequence lines)))
    (while (and lines
                (commonmark-gfm-block--blank-line-p (car (last lines))))
      (setq lines (butlast lines)))
    lines))

(defun commonmark-gfm-block--paragraph-inline-line (line final)
  "Return LINE normalized for paragraph inline parsing.
FINAL is accepted for the caller's loop shape; trailing whitespace is handled
by the inline parser so code spans can retain their literal spaces."
  (ignore final)
  (string-trim-left line))

(defun commonmark-gfm-block--paragraph-inline-lines (lines)
  "Return LINES normalized for paragraph inline parsing."
  (cl-loop for tail on lines
           collect (commonmark-gfm-block--paragraph-inline-line
                    (car tail)
                    (null (cdr tail)))))

(defun commonmark-gfm-block--paragraph-node (lines)
  "Return a paragraph node for LINES."
  (commonmark-gfm-make-node
   'paragraph
   :sourcepos (commonmark-gfm-block--sourcepos-for-lines lines)
   :children (commonmark-gfm-block--inline-parse-lines
              (commonmark-gfm-block--paragraph-inline-lines lines))))

(defun commonmark-gfm-block--heading-node (heading line)
  "Return a heading node for HEADING.
HEADING is a cons cell of (LEVEL . CONTENT)."
  (commonmark-gfm-make-node
   'heading
   :attrs `((level . ,(car heading)))
   :sourcepos (commonmark-gfm-block--sourcepos-for-lines (list line))
   :children (commonmark-gfm-block--inline-parse-at
              (cdr heading)
              (commonmark-gfm-block--line-number line)
              (commonmark-gfm-block--atx-heading-content-column line))))

(defun commonmark-gfm-block--parse-fenced-code (lines index line-count fence)
  "Parse fenced code in LINES starting at INDEX.
LINE-COUNT is the number of input lines.  FENCE is opener data."
  (pcase-let ((start-index index)
              (`(,char ,length ,info ,indent) fence)
              (body nil))
    (setq index (1+ index))
    (while (and (< index line-count)
                (not (commonmark-gfm-block--fence-closer-p
                      (aref lines index) char length)))
      (push (commonmark-gfm-block--strip-up-to-indent
             (aref lines index)
             indent)
            body)
      (setq index (1+ index)))
    (when (< index line-count)
      (setq index (1+ index)))
    (cons (commonmark-gfm-make-node
           'code-block
           :literal (commonmark-gfm-block--lines-literal (nreverse body))
           :sourcepos (commonmark-gfm-block--sourcepos-for-vector-range
                       lines start-index index)
           :attrs `((info . ,info)))
          index)))

(defun commonmark-gfm-block--parse-indented-code (lines index line-count)
  "Parse an indented code block in LINES starting at INDEX."
  (let ((start-index index)
        body)
    (while (and (< index line-count)
                (or (commonmark-gfm-block--blank-line-p (aref lines index))
                    (commonmark-gfm-block--indented-code-line-p
                     (aref lines index))))
      (push (if (commonmark-gfm-block--indented-code-line-p (aref lines index))
                (commonmark-gfm-block--strip-code-indent (aref lines index))
              "")
            body)
      (setq index (1+ index)))
    (cons (commonmark-gfm-make-node
           'code-block
           :literal (commonmark-gfm-block--lines-literal
                     (commonmark-gfm-block--trim-trailing-blank-lines
                      (nreverse body)))
           :sourcepos (commonmark-gfm-block--sourcepos-for-vector-range
                       lines start-index index)
           :attrs '((info . "")))
          index)))

(defun commonmark-gfm-block--parse-html-block (lines index line-count)
  "Parse an HTML block in LINES starting at INDEX."
  (let ((start-index index)
        (start-line (aref lines index))
        body done)
    (while (and (< index line-count) (not done))
      (let ((line (aref lines index)))
        (if (and (> index start-index)
                 (commonmark-gfm-block--html-blank-terminated-p start-line)
                 (commonmark-gfm-block--blank-line-p line))
            (setq done t)
          (push line body)
          (setq done (commonmark-gfm-block--html-end-p line start-line))
          (setq index (1+ index)))))
    (cons (commonmark-gfm-make-node
           'html-block
           :sourcepos (commonmark-gfm-block--sourcepos-for-vector-range
                       lines start-index index)
           :literal (commonmark-gfm-block--lines-literal (nreverse body)))
          index)))

(defun commonmark-gfm-block--parse-blockquote (lines index line-count)
  "Parse a block quote in LINES starting at INDEX."
  (let ((start-index index)
        body
        after-blank
        done)
    (while (and (< index line-count) (not done))
      (let ((line (aref lines index)))
        (cond
         ((commonmark-gfm-block--blockquote-line line)
          (let ((quoted-line (commonmark-gfm-block--blockquote-line line)))
            (push quoted-line body)
            (setq after-blank
                  (commonmark-gfm-block--blank-line-p quoted-line)))
          (setq index (1+ index)))
         ((commonmark-gfm-block--blank-line-p line)
          (setq done t))
         ((and body
               (not after-blank)
               (not (commonmark-gfm-block--blank-line-p (car body)))
               (not (commonmark-gfm-block--indented-code-line-p (car body)))
               (not (commonmark-gfm-block--fence-opener (car body)))
               (not (commonmark-gfm-block--atx-heading (car body)))
               (not (commonmark-gfm-block--thematic-break-p (car body)))
               (or (not (commonmark-gfm-block--block-start-p line))
                   (commonmark-gfm-block--indented-code-line-p line)))
          (push (commonmark-gfm-block--mark-lazy-continuation line) body)
          (setq after-blank nil
                index (1+ index)))
         (t
          (setq done t)))))
    (cons (commonmark-gfm-make-node
           'block-quote
           :sourcepos (commonmark-gfm-block--sourcepos-for-vector-range
                       lines start-index index)
           :children (commonmark-gfm-block--parse-line-list
                      (nreverse body)))
          index)))

(defun commonmark-gfm-block--parse-list-item-lines
    (lines index line-count first-marker)
  "Parse item lines from LINES at INDEX using FIRST-MARKER.
Return (ITEM-LINES LOOSE NEXT-INDEX)."
  (let* ((content-indent (alist-get 'content-indent first-marker))
         (item-lines (list (alist-get 'content first-marker)))
         (loose nil)
         (after-blank nil)
         (active-fence
          (commonmark-gfm-block--fence-state-after-line
           nil
           (car item-lines)))
         done)
    (setq index (1+ index))
    (while (and (< index line-count) (not done))
      (let* ((line (aref lines index))
             (candidate (commonmark-gfm-block--list-marker line)))
        (cond
         ((and active-fence (commonmark-gfm-block--blank-line-p line))
          (push "" item-lines)
          (setq index (1+ index)))
         ((commonmark-gfm-block--blank-line-p line)
          (let* ((next-index
                  (commonmark-gfm-block--next-nonblank-index
                   lines (1+ index) line-count))
                 (next-line (and next-index (aref lines next-index)))
                 (next-marker (and next-line
                                   (commonmark-gfm-block--list-marker
                                    next-line)))
                 (continues
                  (and next-line
                       (or (commonmark-gfm-block--sibling-list-marker-p
                            first-marker next-marker)
                           (and (commonmark-gfm-block--list-item-has-content-p
                                 item-lines)
                                (>= (commonmark-gfm-block--leading-spaces
                                     next-line)
                                    content-indent)))))
                 (nested-list-blank
                  (and next-line
                       (> (commonmark-gfm-block--leading-spaces next-line)
                          content-indent)
                       (commonmark-gfm-block--list-item-has-nested-list-p
                        item-lines))))
            (if continues
                (progn
                  (push "" item-lines)
                  (unless nested-list-blank
                    (setq loose t))
                  (setq
                        after-blank t
                        index (1+ index)))
              (setq done t))))
         ((and (not after-blank)
               (< (commonmark-gfm-block--leading-spaces line)
                  content-indent)
               (commonmark-gfm-block--indented-code-line-p line)
               (commonmark-gfm-block--list-item-has-content-p item-lines))
          (push line item-lines)
          (setq active-fence
                (commonmark-gfm-block--fence-state-after-line
                 active-fence
                 line))
          (setq index (1+ index)))
         ((and (< (commonmark-gfm-block--leading-spaces line)
                  content-indent)
               (commonmark-gfm-block--block-start-p line))
          (setq done t))
         ((commonmark-gfm-block--sibling-list-marker-p first-marker candidate)
          (setq done t))
         ((>= (commonmark-gfm-block--leading-spaces line) content-indent)
          (let ((stripped (commonmark-gfm-block--strip-continuation-indent
                           line content-indent)))
            (push stripped item-lines)
            (setq active-fence
                  (commonmark-gfm-block--fence-state-after-line
                   active-fence
                   stripped)))
          (setq after-blank nil
                index (1+ index)))
         ((not after-blank)
          (push line item-lines)
          (setq active-fence
                (commonmark-gfm-block--fence-state-after-line
                 active-fence
                 line))
          (setq index (1+ index)))
         (t
          (setq done t)))))
    (list (nreverse item-lines) loose index)))

(defun commonmark-gfm-block--task-list-state (lines)
  "Return task list state for LINES and destructively strip marker.
The return value is nil, `unchecked', or `checked'."
  (when commonmark-gfm-enable-gfm
    (let ((first (car lines)))
      (when (and first
                 (string-match "\\`[ \t]*\\[\\([ xX]\\)\\][ \t]+\\(.*\\)\\'"
                               first))
        (setcar lines (match-string 2 first))
        (if (string= (match-string 1 first) " ")
            'unchecked
          'checked)))))

(defun commonmark-gfm-block--parse-list (lines index line-count first-marker)
  "Parse a list in LINES starting at INDEX.
FIRST-MARKER is marker data for the first item."
  (let ((start (alist-get 'start first-marker))
        (type (alist-get 'type first-marker))
        items
        any-loose
        marker
        (previous-marker first-marker))
    (while (and (< index line-count)
                (not (commonmark-gfm-block--thematic-break-p
                      (aref lines index)))
                (setq marker (commonmark-gfm-block--list-marker
                              (aref lines index)))
                (commonmark-gfm-block--sibling-list-marker-p
                 previous-marker marker)
                (commonmark-gfm-block--same-list-marker-p
                 first-marker marker))
      (pcase-let ((item-start index)
                  (`(,item-lines ,loose ,next-index)
                   (commonmark-gfm-block--parse-list-item-lines
                    lines index line-count marker)))
        (let ((task-state (commonmark-gfm-block--task-list-state item-lines)))
          (setq any-loose (or any-loose loose))
          (push (commonmark-gfm-make-node
                 'item
                 :attrs `((tight . ,(not loose))
                          (task-state . ,task-state))
                 :sourcepos (commonmark-gfm-block--sourcepos-for-vector-range
                             lines item-start next-index)
                 :children (commonmark-gfm-block--parse-line-list item-lines))
                items))
        (setq index next-index
              previous-marker marker)))
    (setq items (nreverse items))
    (unless (not any-loose)
      (dolist (item items)
        (commonmark-gfm-node-set-attr item 'tight nil)))
    (cons (commonmark-gfm-make-node
           'list
           :attrs `((type . ,type)
                    (start . ,start)
                    (tight . ,(not any-loose)))
           :sourcepos (and items
                           (list (car (commonmark-gfm-node-sourcepos
                                       (car items)))
                                 (cadr (commonmark-gfm-node-sourcepos
                                        (car (last items))))))
           :children items)
          index)))

(defun commonmark-gfm-block--parse-table (lines index line-count alignments)
  "Parse a GFM table in LINES starting at INDEX with ALIGNMENTS."
  (let ((start-index index)
        (rows (list (commonmark-gfm-block--make-table-row
                     (aref lines index) alignments t))))
    (setq index (+ index 2))
    (while (and (< index line-count)
                (not (commonmark-gfm-block--blank-line-p (aref lines index)))
                (not (commonmark-gfm-block--block-start-p
                      (aref lines index))))
      (push (commonmark-gfm-block--make-table-row
             (aref lines index) alignments nil)
            rows)
      (setq index (1+ index)))
    (cons (commonmark-gfm-make-node
           'table
           :attrs `((alignments . ,alignments))
           :sourcepos (commonmark-gfm-block--sourcepos-for-vector-range
                       lines start-index index)
           :children (nreverse rows))
          index)))

(defun commonmark-gfm-block--parse-paragraph (lines index line-count)
  "Parse a paragraph or setext heading in LINES starting at INDEX."
  (let (body
        node
        done)
    (while (and (< index line-count) (not done))
      (let ((line (aref lines index)))
        (cond
         ((and body (commonmark-gfm-block--setext-heading line))
          (let ((paragraph-lines (nreverse body)))
            (setq node
                  (commonmark-gfm-make-node
                   'heading
                   :attrs `((level . ,(commonmark-gfm-block--setext-heading
                                        line)))
                   :sourcepos (commonmark-gfm-block--sourcepos-for-lines
                               (append paragraph-lines (list line)))
                   :children (commonmark-gfm-block--inline-parse-lines
                              (commonmark-gfm-block--paragraph-inline-lines
                               paragraph-lines)))))
          (setq index (1+ index)
                done t))
         ((or (commonmark-gfm-block--blank-line-p line)
              (and body
                   (commonmark-gfm-block--paragraph-interrupting-block-start-p
                    line)))
          (setq done t))
         (t
          (push line body)
          (setq index (1+ index))))))
    (cons (or node (commonmark-gfm-block--paragraph-node (nreverse body)))
          index)))

(defun commonmark-gfm-block--parse-line-list (line-list)
  "Parse LINE-LIST into a list of block AST nodes."
  (let* ((collected (commonmark-gfm-block--collect-reference-definitions
                     line-list))
         (references (car collected))
         (body-lines (cdr collected))
         (commonmark-gfm-inline-reference-definitions
          (append commonmark-gfm-inline-reference-definitions references))
         (lines (vconcat body-lines))
         (line-count (length lines))
         (index 0)
         children)
    (while (< index line-count)
      (let* ((line (aref lines index))
             (heading (commonmark-gfm-block--atx-heading line))
             (fence (commonmark-gfm-block--fence-opener line))
             (list-marker (commonmark-gfm-block--list-marker line))
             (table-alignments
              (and (< (1+ index) line-count)
                   (commonmark-gfm-block--table-alignments
                    (aref lines (1+ index)))))
             parsed)
        (cond
         ((commonmark-gfm-block--blank-line-p line)
          (setq index (1+ index)))
         ((and commonmark-gfm-enable-gfm
               table-alignments
               (or (string-match-p "|" line)
                   (string-match-p "|" (aref lines (1+ index))))
               (= (length table-alignments)
                  (length (commonmark-gfm-block--table-split-row line))))
          (setq parsed (commonmark-gfm-block--parse-table
                        lines index line-count table-alignments))
          (push (car parsed) children)
          (setq index (cdr parsed)))
         (heading
          (push (commonmark-gfm-block--heading-node heading line) children)
          (setq index (1+ index)))
         ((commonmark-gfm-block--thematic-break-p line)
          (push (commonmark-gfm-make-node
                 'thematic-break
                 :sourcepos (commonmark-gfm-block--sourcepos-for-lines
                             (list line)))
                children)
          (setq index (1+ index)))
         (fence
          (setq parsed (commonmark-gfm-block--parse-fenced-code
                        lines index line-count fence))
          (push (car parsed) children)
          (setq index (cdr parsed)))
         ((commonmark-gfm-block--html-block-start-p line)
          (setq parsed (commonmark-gfm-block--parse-html-block
                        lines index line-count))
          (push (car parsed) children)
          (setq index (cdr parsed)))
         ((commonmark-gfm-block--blockquote-line line)
          (setq parsed (commonmark-gfm-block--parse-blockquote
                        lines index line-count))
          (push (car parsed) children)
          (setq index (cdr parsed)))
         (list-marker
          (setq parsed (commonmark-gfm-block--parse-list
                        lines index line-count list-marker))
          (push (car parsed) children)
          (setq index (cdr parsed)))
         ((commonmark-gfm-block--indented-code-line-p line)
          (setq parsed (commonmark-gfm-block--parse-indented-code
                        lines index line-count))
          (push (car parsed) children)
          (setq index (cdr parsed)))
         (t
          (setq parsed (commonmark-gfm-block--parse-paragraph
                        lines index line-count))
          (push (car parsed) children)
          (setq index (cdr parsed))))))
    (nreverse children)))

;;;###autoload
(defun commonmark-gfm-block-parse (markdown &optional _options)
  "Parse MARKDOWN into a document AST node.

The implementation is incomplete, but the parser emits stable node types for
the CommonMark/GFM blocks that are already implemented."
  (let* ((collected (commonmark-gfm-block--collect-reference-definitions
                     (commonmark-gfm-block--split-lines markdown)))
         (references (car collected))
         (body-lines (cdr collected)))
    (let ((commonmark-gfm-inline-reference-definitions references))
      (commonmark-gfm-make-node
       'document
       :attrs `((references . ,references))
       :sourcepos (commonmark-gfm-block--sourcepos-for-lines
                   (commonmark-gfm-block--split-lines markdown))
       :children (commonmark-gfm-block--parse-line-list body-lines)))))

(provide 'commonmark-gfm-block)

;;; commonmark-gfm-block.el ends here
