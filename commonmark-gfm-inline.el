;;; commonmark-gfm-inline.el --- Inline parsing for commonmark-gfm -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bootstrap inline parser.  This deliberately covers only a small subset
;; while keeping the public shape suitable for a later CommonMark delimiter
;; stack implementation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'commonmark-gfm-ast)

(defvar commonmark-gfm-enable-gfm t
  "Whether GFM-specific inline extensions are enabled.")

(defconst commonmark-gfm-inline--escapable-punctuation
  "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  "ASCII punctuation that can be backslash-escaped in Markdown.")

(defconst commonmark-gfm-inline--named-entities
  (list (cons "amp" "&")
        (cons "lt" "<")
        (cons "gt" ">")
        (cons "quot" "\"")
        (cons "apos" "'")
        (cons "nbsp" (string #xa0))
        (cons "AElig" (string #xc6))
        (cons "ClockwiseContourIntegral" (string #x2232))
        (cons "Dcaron" (string #x10e))
        (cons "DifferentialD" (string #x2146))
        (cons "frac34" (string #xbe))
        (cons "HilbertSpace" (string #x210b))
        (cons "ngE" (concat (string #x2267) (string #x338)))
        (cons "auml" (string #xe4))
        (cons "ouml" (string #xf6))
        (cons "copy" (string #xa9))
        (cons "reg" (string #xae))
        (cons "trade" (string #x2122)))
  "Named character references handled by the bootstrap inline parser.")

(defvar commonmark-gfm-inline-reference-definitions nil
  "Currently active link reference definitions.
Each entry is (NORMALIZED-LABEL . ATTRS), where ATTRS contains at least
`destination' and optionally `title'.")

(defvar commonmark-gfm-inline--sourcepos-enabled nil
  "Whether inline parsing should attach source positions.")

(defvar commonmark-gfm-inline--source-line 1
  "Original source line for the beginning of the current inline text.")

(defvar commonmark-gfm-inline--source-column 1
  "Original source column for the beginning of the current inline text.")

(defvar commonmark-gfm-inline--current-text nil
  "Current inline text used for source position calculations.")

(defun commonmark-gfm-inline--advance-position (line column text start end)
  "Advance LINE and COLUMN over TEXT between START and END.
Return (LINE . COLUMN) for the position after END."
  (let ((index start))
    (while (< index end)
      (if (= (aref text index) ?\n)
          (setq line (1+ line)
                column 1)
        (setq column (1+ column)))
      (setq index (1+ index)))
    (cons line column)))

(defun commonmark-gfm-inline--position-at (text pos)
  "Return source position before TEXT character at POS."
  (commonmark-gfm-inline--advance-position
   commonmark-gfm-inline--source-line
   commonmark-gfm-inline--source-column
   text
   0
   pos))

(defun commonmark-gfm-inline--sourcepos (text start end)
  "Return source position for TEXT from START to END exclusive."
  (when (and commonmark-gfm-inline--sourcepos-enabled (< start end))
    (let* ((start-pos (commonmark-gfm-inline--position-at text start))
           (end-pos (commonmark-gfm-inline--position-at text (1- end))))
      `((,(car start-pos) ,(cdr start-pos))
        (,(car end-pos) ,(cdr end-pos))))))

(defun commonmark-gfm-inline--set-sourcepos (node text start end)
  "Set NODE source position using TEXT START and END, then return NODE."
  (when commonmark-gfm-inline--sourcepos-enabled
    (setf (commonmark-gfm-node-sourcepos node)
          (commonmark-gfm-inline--sourcepos text start end)))
  node)

(defun commonmark-gfm-inline--parse-child (text start end)
  "Parse TEXT substring between START and END with adjusted source position."
  (let* ((start-pos (commonmark-gfm-inline--position-at text start))
         (child-text (substring text start end))
         (commonmark-gfm-inline--source-line (car start-pos))
         (commonmark-gfm-inline--source-column (cdr start-pos))
         (commonmark-gfm-inline--current-text child-text))
    (commonmark-gfm-inline-parse child-text)))

(defun commonmark-gfm-inline-normalize-reference-label (label)
  "Normalize reference LABEL for CommonMark-style lookup."
  (replace-regexp-in-string
   "ß" "ss"
   (downcase
    (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim (or label ""))))))

(defun commonmark-gfm-inline--unescape-string (text)
  "Return TEXT with Markdown backslash escapes resolved."
  (let ((pos 0)
        (len (length text))
        chars)
    (while (< pos len)
      (let ((char (aref text pos)))
        (if (and (= char ?\\)
                 (< (1+ pos) len)
                 (string-match-p
                  (regexp-quote (char-to-string (aref text (1+ pos))))
                  commonmark-gfm-inline--escapable-punctuation))
            (progn
              (push (aref text (1+ pos)) chars)
              (setq pos (+ pos 2)))
          (push char chars)
          (setq pos (1+ pos)))))
    (apply #'string (nreverse chars))))

(defun commonmark-gfm-inline--replacement-character-p (codepoint)
  "Return non-nil when CODEPOINT must decode as U+FFFD."
  (or (<= codepoint 0)
      (and (>= codepoint #xd800) (<= codepoint #xdfff))))

(defun commonmark-gfm-inline--entity-literal (body)
  "Return decoded character reference BODY, or nil."
  (cond
   ((string-prefix-p "#x" (downcase body))
    (let ((codepoint (string-to-number (substring body 2) 16)))
      (cond
       ((> codepoint #x10ffff) nil)
       ((commonmark-gfm-inline--replacement-character-p codepoint)
        (string #xfffd))
       (t
        (char-to-string (decode-char 'ucs codepoint))))))
   ((string-prefix-p "#" body)
    (let ((codepoint (string-to-number (substring body 1) 10)))
      (cond
       ((> codepoint #x10ffff) nil)
       ((commonmark-gfm-inline--replacement-character-p codepoint)
        (string #xfffd))
       (t
        (char-to-string (decode-char 'ucs codepoint))))))
   (t
    (cdr (assoc body commonmark-gfm-inline--named-entities)))))

(defun commonmark-gfm-inline--decode-character-references (text)
  "Decode character references in TEXT."
  (replace-regexp-in-string
   "&\\(#\\(?:[xX][0-9A-Fa-f]+\\|[0-9]+\\)\\|[A-Za-z][A-Za-z0-9]+\\);"
   (lambda (match)
     (or (commonmark-gfm-inline--entity-literal (substring match 1 -1))
         match))
   (or text "")
   t
   t))

(defun commonmark-gfm-inline--starts-with-p (prefix text pos)
  "Return non-nil when PREFIX occurs in TEXT at POS."
  (let ((end (+ pos (length prefix))))
    (and (<= end (length text))
         (string= prefix (substring text pos end)))))

(defun commonmark-gfm-inline--find-substring (needle text start)
  "Return index of NEEDLE in TEXT at or after START, or nil."
  (string-match-p (regexp-quote needle) text start))

(defun commonmark-gfm-inline--push-text (literal nodes &optional start end)
  "Push LITERAL as text onto reversed NODES, merging adjacent text."
  (if (string-empty-p literal)
      nodes
    (if commonmark-gfm-inline--sourcepos-enabled
        (cons (commonmark-gfm-inline--set-sourcepos
               (commonmark-gfm-make-node 'text :literal literal)
               commonmark-gfm-inline--current-text
               (or start 0)
               (or end (+ (or start 0) (length literal))))
              nodes)
      (if (and nodes (eq (commonmark-gfm-node-type (car nodes)) 'text))
        (progn
          (setf (commonmark-gfm-node-literal (car nodes))
                (concat (commonmark-gfm-node-literal (car nodes)) literal))
          nodes)
        (cons (commonmark-gfm-make-node 'text :literal literal) nodes)))))

(defun commonmark-gfm-inline--trim-gfm-autolink (literal)
  "Return (BODY . TRAILING) for a GFM autolink LITERAL."
  (let ((body literal)
        (trailing ""))
    (when (string-match "&[A-Za-z0-9]+;\\'" body)
      (setq trailing (concat (match-string 0 body) trailing)
            body (substring body 0 (match-beginning 0))))
    (while (and (> (length body) 0)
                (memq (aref body (1- (length body)))
                      '(?? ?! ?. ?, ?: ?* ?_ ?~)))
      (setq trailing (concat (substring body -1) trailing))
      (setq body (substring body 0 -1)))
    (while (and (string-suffix-p ")" body)
                (> (cl-count ?\) body)
                   (cl-count ?\( body)))
      (setq trailing (concat ")" trailing))
      (setq body (substring body 0 -1)))
    (cons body trailing)))

(defun commonmark-gfm-inline--gfm-autolink-destination (literal)
  "Return link destination for GFM autolink LITERAL."
  (cond
   ((commonmark-gfm-inline--gfm-email-valid-p literal)
    (concat "mailto:" literal))
   ((string-prefix-p "www." literal)
    (concat "http://" literal))
   (t literal)))

(defun commonmark-gfm-inline--char-before-source
    (text pos source-offset)
  "Return the character before TEXT POS using SOURCE-OFFSET when needed."
  (cond
   ((> pos 0)
    (aref text (1- pos)))
   ((and (> source-offset 0)
         commonmark-gfm-inline--current-text
         (<= source-offset (length commonmark-gfm-inline--current-text)))
    (aref commonmark-gfm-inline--current-text (1- source-offset)))))

(defun commonmark-gfm-inline--gfm-email-start-boundary-p
    (text pos source-offset)
  "Return non-nil when POS can start a GFM email autolink in TEXT."
  (let ((before (commonmark-gfm-inline--char-before-source
                 text pos source-offset)))
    (or (null before)
        (not (string-match-p
              "\\`[A-Za-z0-9._+-]\\'"
              (char-to-string before))))))

(defun commonmark-gfm-inline--gfm-email-valid-p (literal)
  "Return non-nil when LITERAL is a valid GFM email autolink."
  (and (string-match-p
        "\\`[A-Za-z0-9._+-]+@[A-Za-z0-9_-]+\\(?:\\.[A-Za-z0-9_-]+\\)+\\'"
        literal)
       (not (memq (aref literal (1- (length literal))) '(?- ?_)))))

(defun commonmark-gfm-inline--gfm-autolink-node (literal text start)
  "Return a GFM autolink node for LITERAL in TEXT at START."
  (commonmark-gfm-inline--set-sourcepos
   (commonmark-gfm-make-node
    'link
    :attrs `((destination . ,(commonmark-gfm-inline--gfm-autolink-destination
                              literal)))
    :children (list
               (commonmark-gfm-inline--set-sourcepos
                (commonmark-gfm-make-node 'text :literal literal)
                text
                start
                (+ start (length literal)))))
   text
   start
   (+ start (length literal))))

(defun commonmark-gfm-inline--parse-gfm-autolink-at
    (text pos &optional source-offset)
  "Parse a GFM autolink literal in TEXT at POS.
SOURCE-OFFSET is added when setting source positions from a substring."
  (when commonmark-gfm-enable-gfm
    (let ((case-fold-search nil)
          (source-offset (or source-offset 0))
          kind
          raw)
      (cond
       ((and (not (eq (commonmark-gfm-inline--char-before-source
                       text pos source-offset)
                      ?<))
             (or (commonmark-gfm-inline--starts-with-p "http://" text pos)
                 (commonmark-gfm-inline--starts-with-p "https://" text pos)
                 (commonmark-gfm-inline--starts-with-p "ftp://" text pos))
             (string-match
              "\\`\\(?:https?://\\|ftp://\\)[A-Za-z0-9][A-Za-z0-9-]*\\(?:\\.[A-Za-z0-9][A-Za-z0-9-]*\\)+[^[:space:]<]*"
              (substring text pos)))
        (setq kind 'url)
        (setq raw (match-string 0 (substring text pos))))
       ((and (not (eq (commonmark-gfm-inline--char-before-source
                       text pos source-offset)
                      ?<))
             (commonmark-gfm-inline--starts-with-p "www." text pos)
             (string-match
              "\\`www\\.[A-Za-z0-9][A-Za-z0-9-]*\\(?:\\.[A-Za-z0-9][A-Za-z0-9-]*\\)+[^[:space:]<]*"
              (substring text pos)))
        (setq kind 'url)
        (setq raw (match-string 0 (substring text pos))))
       ((and (not (eq (commonmark-gfm-inline--char-before-source
                       text pos source-offset)
                      ?<))
             (commonmark-gfm-inline--gfm-email-start-boundary-p
              text pos source-offset)
             (string-match
              "\\`[A-Za-z0-9._+-]+@[A-Za-z0-9_-]+\\(?:\\.[A-Za-z0-9_-]+\\)+\\.?"
              (substring text pos)))
        (setq kind 'email)
        (setq raw (match-string 0 (substring text pos)))))
      (when raw
        (pcase-let* ((`(,body . ,_trailing)
                      (if (eq kind 'email)
                          (if (string-suffix-p "." raw)
                              (cons (substring raw 0 -1) ".")
                            (cons raw ""))
                        (commonmark-gfm-inline--trim-gfm-autolink raw))))
          (when (and (not (string-empty-p body))
                     (or (not (string-match-p "@" body))
                         (commonmark-gfm-inline--gfm-email-valid-p body)))
            (cons (commonmark-gfm-inline--gfm-autolink-node
                   body
                   commonmark-gfm-inline--current-text
                   (+ source-offset pos))
                  (+ pos (length body)))))))))

(defun commonmark-gfm-inline--push-text-with-gfm-autolinks
    (literal nodes &optional source-start)
  "Push LITERAL text, converting GFM bare autolinks when enabled."
  (if (not commonmark-gfm-enable-gfm)
      (commonmark-gfm-inline--push-text
       literal
       nodes
       source-start
       (and source-start (+ source-start (length literal))))
    (let ((pos 0)
          (source-start (or source-start 0))
          parsed)
      (while (< pos (length literal))
        (setq parsed
              (commonmark-gfm-inline--parse-gfm-autolink-at
               literal pos source-start))
        (if parsed
            (progn
              (push (car parsed) nodes)
              (setq pos (cdr parsed)))
          (let ((start pos))
            (setq pos (1+ pos))
            (while (and (< pos (length literal))
                        (not (commonmark-gfm-inline--parse-gfm-autolink-at
                              literal pos source-start)))
              (setq pos (1+ pos)))
            (setq nodes (commonmark-gfm-inline--push-text
                         (substring literal start pos)
                         nodes
                         (+ source-start start)
                         (+ source-start pos))))))
      (commonmark-gfm-inline--push-text
       (substring literal pos)
       nodes
       (+ source-start pos)
       (+ source-start (length literal))))))

(defun commonmark-gfm-inline--hardbreak-text (literal)
  "Return (HARDBREAK . TEXT) for newline after LITERAL."
  (let ((right-trimmed (string-trim-right literal)))
    (cond
     ((string-suffix-p "\\" right-trimmed)
      (cons t (substring right-trimmed 0 -1)))
     ((string-match "  +\\'" literal)
      (cons t (substring literal 0 (match-beginning 0))))
     (t
      (cons nil right-trimmed)))))

(defun commonmark-gfm-inline--trim-final-text (nodes)
  "Trim trailing whitespace from the final text node in reversed NODES."
  (if (and nodes (eq (commonmark-gfm-node-type (car nodes)) 'text))
      (let ((literal (string-trim-right
                      (commonmark-gfm-node-literal (car nodes)))))
        (if (string-empty-p literal)
            (cdr nodes)
          (setf (commonmark-gfm-node-literal (car nodes)) literal)
          nodes))
    nodes))

(defun commonmark-gfm-inline--prepare-newline (nodes)
  "Return (HARDBREAK . NODES) before inserting a newline node."
  (if (and nodes (eq (commonmark-gfm-node-type (car nodes)) 'text))
      (let* ((result (commonmark-gfm-inline--hardbreak-text
                      (commonmark-gfm-node-literal (car nodes))))
             (hardbreak (car result))
             (literal (cdr result)))
        (if hardbreak
            (progn
              (if (string-empty-p literal)
                  (setq nodes (cdr nodes))
                (setf (commonmark-gfm-node-literal (car nodes)) literal))
              (cons t nodes))
          (if (string-empty-p literal)
              (setq nodes (cdr nodes))
            (setf (commonmark-gfm-node-literal (car nodes)) literal))
          (cons nil nodes)))
    (cons nil nodes)))

(defun commonmark-gfm-inline--backtick-run-length (text pos)
  "Return the length of a backtick run in TEXT at POS."
  (let ((len (length text))
        (end pos))
    (while (and (< end len) (= (aref text end) ?`))
      (setq end (1+ end)))
    (- end pos)))

(defun commonmark-gfm-inline--delimiter-run-length (text pos delimiter)
  "Return the length of a DELIMITER run in TEXT at POS."
  (let ((len (length text))
        (end pos))
    (while (and (< end len) (= (aref text end) delimiter))
      (setq end (1+ end)))
    (- end pos)))

(defun commonmark-gfm-inline--tilde-run-length (text pos)
  "Return the length of a tilde run in TEXT at POS."
  (commonmark-gfm-inline--delimiter-run-length text pos ?~))

(defun commonmark-gfm-inline--whitespace-char-p (char)
  "Return non-nil when CHAR is nil or whitespace."
  (or (null char)
      (memq char '(?\s ?\t ?\n ?\r ?\f))
      (memq (get-char-code-property char 'general-category)
            '(Zs Zl Zp))))

(defun commonmark-gfm-inline--punctuation-char-p (char)
  "Return non-nil when CHAR is punctuation."
  (and char
       (string-match-p "\\`[[:punct:]]\\'" (char-to-string char))))

(defun commonmark-gfm-inline--delimiter-flanking (text pos run-length)
  "Return (LEFT-FLANKING . RIGHT-FLANKING) for delimiter run at POS.
RUN-LENGTH is the delimiter run length."
  (let* ((before (and (> pos 0) (aref text (1- pos))))
         (after-pos (+ pos run-length))
         (after (and (< after-pos (length text)) (aref text after-pos)))
         (before-whitespace (commonmark-gfm-inline--whitespace-char-p before))
         (after-whitespace (commonmark-gfm-inline--whitespace-char-p after))
         (before-punctuation (commonmark-gfm-inline--punctuation-char-p before))
         (after-punctuation (commonmark-gfm-inline--punctuation-char-p after))
         (left-flanking
          (and (not after-whitespace)
               (or (not after-punctuation)
                   before-whitespace
                   before-punctuation)))
         (right-flanking
          (and (not before-whitespace)
               (or (not before-punctuation)
                   after-whitespace
                   after-punctuation))))
    (cons left-flanking right-flanking)))

(defun commonmark-gfm-inline--delimiter-can-open-p
    (text pos run-length delimiter)
  "Return non-nil when delimiter run at POS can open emphasis."
  (pcase-let* ((`(,left-flanking . ,right-flanking)
                (commonmark-gfm-inline--delimiter-flanking
                 text pos run-length))
               (before (and (> pos 0) (aref text (1- pos)))))
    (if (= delimiter ?_)
        (and left-flanking
             (or (not right-flanking)
                 (commonmark-gfm-inline--punctuation-char-p before)))
      left-flanking)))

(defun commonmark-gfm-inline--delimiter-can-close-p
    (text pos run-length delimiter)
  "Return non-nil when delimiter run at POS can close emphasis."
  (pcase-let* ((`(,left-flanking . ,right-flanking)
                (commonmark-gfm-inline--delimiter-flanking
                 text pos run-length))
               (after-pos (+ pos run-length))
               (after (and (< after-pos (length text)) (aref text after-pos))))
    (if (= delimiter ?_)
        (and right-flanking
             (or (not left-flanking)
                 (commonmark-gfm-inline--punctuation-char-p after)))
      right-flanking)))

(defun commonmark-gfm-inline--skip-code-span (text pos)
  "Return position after code span at POS, or nil."
  (when (= (aref text pos) ?`)
    (let* ((run-length (commonmark-gfm-inline--backtick-run-length text pos))
           (delimiter (make-string run-length ?`))
           (body-start (+ pos run-length))
           (body-end (commonmark-gfm-inline--find-substring
                      delimiter text body-start)))
      (and body-end (+ body-end run-length)))))

(defun commonmark-gfm-inline--find-code-span-closer
    (text start run-length)
  "Return position of a closing backtick run of RUN-LENGTH in TEXT."
  (let ((pos start)
        (len (length text))
        found)
    (while (and (< pos len) (not found))
      (if (= (aref text pos) ?`)
          (let ((candidate-length
                 (commonmark-gfm-inline--backtick-run-length text pos)))
            (if (= candidate-length run-length)
                (setq found pos)
              (setq pos (+ pos candidate-length))))
        (setq pos (1+ pos))))
    found))

(defun commonmark-gfm-inline--has-closing-delimiter-p
    (text start delimiter run-length)
  "Return non-nil when TEXT has a closing DELIMITER run after START."
  (let ((pos start)
        (len (length text))
        found)
    (while (and (< pos len) (not found))
      (cond
       ((and (= (aref text pos) ?\\) (< (1+ pos) len))
        (setq pos (+ pos 2)))
       ((= (aref text pos) ?`)
        (setq pos (or (commonmark-gfm-inline--skip-code-span text pos)
                      (1+ pos))))
       ((= (aref text pos) ?<)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-autolink text pos))
                      (1+ pos))))
       ((= (aref text pos) delimiter)
        (let ((candidate-length
               (commonmark-gfm-inline--delimiter-run-length
                text pos delimiter)))
          (if (and (>= candidate-length run-length)
                   (commonmark-gfm-inline--delimiter-can-close-p
                    text pos candidate-length delimiter))
              (setq found t)
            (setq pos (+ pos candidate-length)))))
       (t
        (setq pos (1+ pos)))))
    found))

(defun commonmark-gfm-inline--find-emphasis-closer
    (text start delimiter desired-length)
  "Find an emphasis closer in TEXT from START.
DELIMITER is either `*' or `_'.  DESIRED-LENGTH is 1, 2, or 3."
  (let ((pos start)
        (len (length text))
        nested-openers
        found)
    (while (and (< pos len) (not found))
      (cond
       ((and (= (aref text pos) ?\\) (< (1+ pos) len))
        (setq pos (+ pos 2)))
       ((= (aref text pos) ?`)
        (setq pos (or (commonmark-gfm-inline--skip-code-span text pos)
                      (1+ pos))))
       ((= (aref text pos) ?<)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-autolink text pos))
                      (1+ pos))))
       ((and (memq (aref text pos) '(?* ?_))
             (/= (aref text pos) delimiter)
             (>= (commonmark-gfm-inline--delimiter-run-length
                  text pos (aref text pos))
                 2))
        (let* ((other-delimiter (aref text pos))
               (other-length
                (commonmark-gfm-inline--delimiter-run-length
                 text pos other-delimiter))
               (parsed
                (and (commonmark-gfm-inline--delimiter-can-open-p
                      text pos other-length other-delimiter)
                     (commonmark-gfm-inline--parse-best-emphasis
                      text pos (min 2 other-length)))))
          (setq pos (if parsed (cdr parsed) (+ pos other-length)))))
       ((commonmark-gfm-inline--starts-with-p "![" text pos)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-link-like
                            text pos t))
                      (1+ pos))))
       ((= (aref text pos) ?\[)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-link-like
                            text pos nil))
                      (1+ pos))))
       ((= (aref text pos) delimiter)
        (let ((run-length
               (commonmark-gfm-inline--delimiter-run-length
                text pos delimiter)))
          (let ((can-close (commonmark-gfm-inline--delimiter-can-close-p
                            text pos run-length delimiter))
                (can-open (commonmark-gfm-inline--delimiter-can-open-p
                           text pos run-length delimiter))
                (nested-length (min 2 run-length)))
          (cond
           ((and can-open
                 (< run-length desired-length))
            (let ((parsed (commonmark-gfm-inline--parse-emphasis
                           text pos run-length)))
              (setq pos (if parsed (cdr parsed) (+ pos run-length)))))
           ((and can-open
                 (> run-length desired-length)
                 (not nested-openers)
                 (commonmark-gfm-inline--has-closing-delimiter-p
                  text (+ pos run-length) delimiter nested-length))
            (push nested-length nested-openers)
            (setq pos (+ pos run-length)))
           ((and can-open
                 (> run-length desired-length)
                 (not nested-openers))
            (setq pos (+ pos run-length)))
           ((< run-length desired-length)
            (setq pos (+ pos run-length)))
           ((and can-close nested-openers)
            (let* ((nested-length (car nested-openers))
                   (remaining (- run-length nested-length)))
              (setq nested-openers (cdr nested-openers))
              (if (and (>= remaining desired-length) can-close)
                  (setq found (+ pos nested-length))
                (setq pos (+ pos run-length)))))
           (can-close
            (setq found pos))
           (can-open
            (push nested-length nested-openers)
            (setq
                  pos (+ pos run-length)))
           (t
            (setq pos (+ pos run-length)))))))
       (t
        (setq pos (1+ pos)))))
    found))

(defun commonmark-gfm-inline--emphasis-node (length children)
  "Return emphasis node for delimiter LENGTH and CHILDREN."
  (pcase length
    (1 (commonmark-gfm-make-node 'emph :children children))
    (2 (commonmark-gfm-make-node 'strong :children children))
    (3 (commonmark-gfm-make-node
        'emph
        :children (list (commonmark-gfm-make-node
                         'strong
                         :children children))))))

(defun commonmark-gfm-inline--strong-wrapper (count children text start end)
  "Wrap CHILDREN in COUNT nested strong nodes."
  (let ((children children)
        node)
    (dotimes (_ count node)
      (setq node (commonmark-gfm-inline--set-sourcepos
                  (commonmark-gfm-make-node 'strong :children children)
                  text
                  start
                  end)
            children (list node)))))

(defun commonmark-gfm-inline--emphasis-run-node (length children text start end)
  "Return a node for a balanced delimiter run of LENGTH."
  (if commonmark-gfm-enable-gfm
      (if (= (% length 2) 0)
          (commonmark-gfm-inline--set-sourcepos
           (commonmark-gfm-make-node 'strong :children children)
           text
           start
           end)
        (commonmark-gfm-inline--set-sourcepos
         (commonmark-gfm-make-node
          'emph
          :children (list (commonmark-gfm-inline--set-sourcepos
                           (commonmark-gfm-make-node
                            'strong
                            :children children)
                           text
                           start
                           end)))
         text
         start
         end))
    (if (= (% length 2) 0)
        (commonmark-gfm-inline--strong-wrapper
         (/ length 2) children text start end)
      (commonmark-gfm-inline--set-sourcepos
       (commonmark-gfm-make-node
        'emph
        :children (if (= length 1)
                      children
                    (list (commonmark-gfm-inline--strong-wrapper
                           (/ (1- length) 2) children text start end))))
       text
       start
       end))))

(defun commonmark-gfm-inline--find-emphasis-run-closer
    (text start delimiter run-length)
  "Find a closing DELIMITER run of at least RUN-LENGTH in TEXT."
  (let ((pos start)
        (len (length text))
        found)
    (while (and (< pos len) (not found))
      (cond
       ((and (= (aref text pos) ?\\) (< (1+ pos) len))
        (setq pos (+ pos 2)))
       ((= (aref text pos) ?`)
        (setq pos (or (commonmark-gfm-inline--skip-code-span text pos)
                      (1+ pos))))
       ((= (aref text pos) ?<)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-autolink text pos))
                      (1+ pos))))
       ((commonmark-gfm-inline--starts-with-p "![" text pos)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-link-like
                            text pos t))
                      (1+ pos))))
       ((= (aref text pos) ?\[)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-link-like
                            text pos nil))
                      (1+ pos))))
       ((= (aref text pos) delimiter)
        (let ((candidate-length
               (commonmark-gfm-inline--delimiter-run-length
                text pos delimiter)))
          (if (and (>= candidate-length run-length)
                   (commonmark-gfm-inline--delimiter-can-close-p
                    text pos candidate-length delimiter))
              (setq found pos)
            (setq pos (+ pos candidate-length)))))
       (t
        (setq pos (1+ pos)))))
    found))

(defun commonmark-gfm-inline--parse-emphasis-run (text pos)
  "Parse a balanced delimiter run in TEXT at POS.
This handles long same-size runs such as `****foo****' while the full
delimiter stack parser is still being built."
  (let* ((delimiter (aref text pos))
         (run-length (commonmark-gfm-inline--delimiter-run-length
                      text pos delimiter)))
    (when (and (>= run-length 4)
               (commonmark-gfm-inline--delimiter-can-open-p
                text pos run-length delimiter))
      (let* ((body-start (+ pos run-length))
             (body-end (commonmark-gfm-inline--find-emphasis-run-closer
                        text body-start delimiter run-length)))
        (when body-end
          (cons (commonmark-gfm-inline--emphasis-run-node
                 run-length
                 (commonmark-gfm-inline--parse-child text body-start body-end)
                 text
                 pos
                 (+ body-end run-length))
                (+ body-end run-length)))))))

(defun commonmark-gfm-inline--parse-emphasis (text pos desired-length)
  "Parse emphasis in TEXT at POS using DESIRED-LENGTH delimiters.
Return (NODE . END) on success, or nil."
  (let* ((delimiter (aref text pos))
         (run-length (commonmark-gfm-inline--delimiter-run-length
                      text pos delimiter)))
    (when (and (>= run-length desired-length)
               (commonmark-gfm-inline--delimiter-can-open-p
                text pos run-length delimiter))
      (let ((body-start (+ pos desired-length))
            body-end)
        (setq body-end (commonmark-gfm-inline--find-emphasis-closer
                        text body-start delimiter desired-length))
        (when (and body-end (> body-end body-start))
          (cons (commonmark-gfm-inline--set-sourcepos
                 (commonmark-gfm-inline--emphasis-node
                  desired-length
                  (commonmark-gfm-inline--parse-child
                   text body-start body-end))
                 text
                 pos
                 (+ body-end desired-length))
                (+ body-end desired-length)))))))

(defun commonmark-gfm-inline--node-starts-with-delimiter-p (node delimiter)
  "Return non-nil when NODE's rendered inline content starts with DELIMITER."
  (pcase (commonmark-gfm-node-type node)
    ('text
     (let ((literal (commonmark-gfm-node-literal node)))
       (and (> (length literal) 0)
            (= (aref literal 0) delimiter))))
    ((or 'emph 'strong 'link 'image 'strikethrough)
     (let ((children (commonmark-gfm-node-children node)))
       (and children
            (commonmark-gfm-inline--node-starts-with-delimiter-p
             (car children)
             delimiter))))
    (_ nil)))

(defun commonmark-gfm-inline--emphasis-candidate-valid-p
    (node delimiter run-length desired-length)
  "Return non-nil when NODE is a valid split-run emphasis candidate."
  (not
   (and (> run-length desired-length)
        (let ((children (commonmark-gfm-node-children node)))
          (and children
               (commonmark-gfm-inline--node-starts-with-delimiter-p
                (car children)
                delimiter))))))

(defun commonmark-gfm-inline--parse-best-emphasis (text pos desired-length)
  "Parse the best emphasis candidate at POS.
DESIRED-LENGTH is the length preferred by the caller."
  (let* ((run-length (commonmark-gfm-inline--delimiter-run-length
                      text pos (aref text pos)))
         (delimiter (aref text pos))
         (candidates
          (cl-remove-if-not
           (lambda (length)
             (and (<= length run-length)
                  (<= length 3)
                  (not (and (= delimiter ?*)
                            (= run-length 2)
                            (= length 1)))))
           (delete-dups
            (append (list desired-length)
                    (number-sequence (min 3 run-length) 1 -1)))))
         best)
    (dolist (length candidates)
      (when-let ((parsed (commonmark-gfm-inline--parse-emphasis
                          text pos length)))
        (when (and (commonmark-gfm-inline--emphasis-candidate-valid-p
                    (car parsed) delimiter run-length length)
                   (or (not best)
                       (> (cdr parsed) (cdr (cdr best)))
                       (and (= (cdr parsed) (cdr (cdr best)))
                            (> length (car best)))))
          (setq best (cons length parsed)))))
    (cdr best)))

(defun commonmark-gfm-inline--push-delimiter-fallback (text pos nodes)
  "Push delimiter text at POS onto NODES.
When the delimiter run cannot open emphasis, consume the full run as literal
text.  Otherwise consume one delimiter so a shorter opener in the same run can
still be considered by the main parser."
  (let* ((delimiter (aref text pos))
         (run-length (commonmark-gfm-inline--delimiter-run-length
                      text pos delimiter)))
    (if (commonmark-gfm-inline--delimiter-can-open-p
         text pos run-length delimiter)
        (cons (commonmark-gfm-inline--push-text
               (char-to-string delimiter) nodes pos (1+ pos))
              (1+ pos))
      (cons (commonmark-gfm-inline--push-text
             (make-string run-length delimiter)
             nodes
             pos
             (+ pos run-length))
            (+ pos run-length)))))

(defun commonmark-gfm-inline--parse-emphasis-or-fallback
    (text pos nodes desired-length)
  "Parse emphasis at POS or push literal delimiter text.
Return (NODES . POS), where NODES remains in reverse parse order."
  (let ((parsed (or (commonmark-gfm-inline--parse-emphasis-run text pos)
                    (commonmark-gfm-inline--parse-best-emphasis
                     text pos desired-length))))
    (if parsed
        (cons (cons (car parsed) nodes) (cdr parsed))
      (commonmark-gfm-inline--push-delimiter-fallback text pos nodes))))

(defun commonmark-gfm-inline--normalize-code-span (literal)
  "Normalize code span LITERAL according to the CommonMark outline."
  (let ((text (replace-regexp-in-string "\r\n\\|\n\\|\r" " " literal)))
    (if (and (> (length text) 1)
             (string-prefix-p " " text)
             (string-suffix-p " " text)
             (string-match-p "[^ ]" (substring text 1 -1)))
        (substring text 1 -1)
      text)))

(defun commonmark-gfm-inline--parse-code-span (text pos)
  "Parse a code span in TEXT at POS.
Return (NODE . END) on success, or nil."
  (let* ((run-length (commonmark-gfm-inline--backtick-run-length text pos))
         (body-start (+ pos run-length))
         (body-end (commonmark-gfm-inline--find-code-span-closer
                    text body-start run-length)))
    (when body-end
      (cons (commonmark-gfm-inline--set-sourcepos
             (commonmark-gfm-make-node
              'code
              :literal (commonmark-gfm-inline--normalize-code-span
                        (substring text body-start body-end)))
             text
             pos
             (+ body-end run-length))
            (+ body-end run-length)))))

(defun commonmark-gfm-inline--find-closing-bracket (text start)
  "Return the next unescaped closing bracket in TEXT after START."
  (let ((pos start)
        (len (length text))
        (depth 0)
        found)
    (while (and (< pos len) (not found))
      (cond
       ((and (= (aref text pos) ?\\) (< (1+ pos) len))
        (setq pos (+ pos 2)))
       ((= (aref text pos) ?`)
        (setq pos (or (commonmark-gfm-inline--skip-code-span text pos)
                      (1+ pos))))
       ((= (aref text pos) ?<)
        (setq pos (or (cdr (commonmark-gfm-inline--parse-autolink text pos))
                      (1+ pos))))
       ((= (aref text pos) ?\[)
        (setq depth (1+ depth)
              pos (1+ pos)))
       ((= (aref text pos) ?\])
        (if (> depth 0)
            (setq depth (1- depth)
                  pos (1+ pos))
          (setq found pos)))
       (t
        (setq pos (1+ pos)))))
    found))

(defun commonmark-gfm-inline--nodes-contain-link-p (nodes)
  "Return non-nil when NODES contain a link node."
  (cl-some
   (lambda (node)
     (or (eq (commonmark-gfm-node-type node) 'link)
         (commonmark-gfm-inline--nodes-contain-link-p
          (commonmark-gfm-node-children node))))
   nodes))

(defun commonmark-gfm-inline--label-contains-link-p
    (text label-start label-end)
  "Return non-nil when TEXT label range contains a parsed link."
  (commonmark-gfm-inline--nodes-contain-link-p
   (commonmark-gfm-inline--parse-child text label-start label-end)))

(defun commonmark-gfm-inline--find-closing-paren (text start)
  "Return the next unescaped closing parenthesis in TEXT after START."
  (let ((pos start)
        (len (length text))
        (depth 0)
        found)
    (while (and (< pos len) (not found))
      (cond
       ((and (= (aref text pos) ?\\) (< (1+ pos) len))
        (setq pos (+ pos 2)))
       ((= (aref text pos) ?<)
        (if-let ((end (commonmark-gfm-inline--find-substring ">" text (1+ pos))))
            (setq pos (1+ end))
          (setq pos (1+ pos))))
       ((= (aref text pos) ?\()
        (setq depth (1+ depth)
              pos (1+ pos)))
       ((= (aref text pos) ?\))
        (if (> depth 0)
            (setq depth (1- depth)
                  pos (1+ pos))
          (setq found pos)))
       (t
        (setq pos (1+ pos)))))
    found))

(defun commonmark-gfm-inline--title-close-char (char)
  "Return closing title delimiter for CHAR, or nil."
  (pcase char
    (?\" ?\")
    (?\' ?\')
    (?\( ?\))
    (_ nil)))

(defun commonmark-gfm-inline--contains-unescaped-char-p (text char)
  "Return non-nil when TEXT contains unescaped CHAR."
  (let ((pos 0)
        found)
    (while (and (< pos (length text)) (not found))
      (cond
       ((and (= (aref text pos) ?\\) (< (1+ pos) (length text)))
        (setq pos (+ pos 2)))
       ((= (aref text pos) char)
        (setq found t))
       (t
        (setq pos (1+ pos)))))
    found))

(defun commonmark-gfm-inline--strip-title (title)
  "Strip Markdown title delimiters from TITLE.
Return nil when TITLE is not a complete quoted title."
  (when title
    (let* ((title (string-trim title))
           (len (length title))
           (close (and (> len 0)
                       (commonmark-gfm-inline--title-close-char
                        (aref title 0)))))
      (when (and close
                 (>= len 2)
                 (= close (aref title (1- len)))
                 (not (and (memq close '(?\" ?\'))
                           (commonmark-gfm-inline--contains-unescaped-char-p
                            (substring title 1 -1)
                            close))))
        (commonmark-gfm-inline--unescape-string
         (substring title 1 -1))))))

(defun commonmark-gfm-inline--title-attr (title)
  "Return a title attr list for TITLE, or nil."
  (let ((title (commonmark-gfm-inline--strip-title title)))
    (when title
      `((title . ,(commonmark-gfm-inline--decode-character-references
                   title))))))

(defun commonmark-gfm-inline--split-destination-title (spec)
  "Split SPEC into (DESTINATION . TITLE-SPEC), or nil."
  (let ((spec (string-trim spec)))
    (cond
     ((string-empty-p spec)
      nil)
     ((string-prefix-p "<" spec)
      (let ((end (string-match-p ">" spec 1)))
        (when end
          (let ((destination (substring spec 1 end))
                (rest (substring spec (1+ end))))
            (when (or (string-empty-p rest)
                      (commonmark-gfm-inline--whitespace-char-p
                       (aref rest 0)))
              (unless (or (string-match-p "[\n\r]" destination)
                          (and (> end 1)
                               (= (aref spec (1- end)) ?\\)))
                (cons destination (string-trim rest))))))))
     ((string-match "\\`\\([^ \t\n]+\\)\\(?:[ \t\n]+\\(.*\\)\\)?\\'" spec)
      (cons (match-string 1 spec)
            (string-trim (or (match-string 2 spec) "")))))))

(defun commonmark-gfm-inline--parse-destination-title (spec)
  "Parse link destination and optional title from SPEC.
Return an alist containing `destination' and optionally `title', or nil when
SPEC is not a valid destination/title pair."
  (if (string-empty-p (string-trim spec))
      '((destination . ""))
    (pcase-let ((`(,destination . ,title-spec)
                 (commonmark-gfm-inline--split-destination-title spec)))
      (when destination
        (let ((title (unless (string-empty-p title-spec)
                       (commonmark-gfm-inline--title-attr title-spec))))
          (when (or (string-empty-p title-spec) title)
            `((destination . ,(commonmark-gfm-inline--decode-character-references
                               (commonmark-gfm-inline--unescape-string
                                destination)))
              ,@title)))))))

(defun commonmark-gfm-inline--reference-definition (label)
  "Return reference definition attrs for LABEL, or nil."
  (cdr (assoc (commonmark-gfm-inline-normalize-reference-label label)
              commonmark-gfm-inline-reference-definitions)))

(defun commonmark-gfm-inline--make-link-node
    (type label attrs &optional text start end label-start label-end)
  "Return a TYPE link-like node for LABEL and ATTRS."
  (commonmark-gfm-inline--set-sourcepos
   (commonmark-gfm-make-node
    type
    :children (if (and text label-start label-end)
                  (commonmark-gfm-inline--parse-child text label-start label-end)
                (commonmark-gfm-inline-parse label))
    :attrs (copy-tree attrs))
   (or text commonmark-gfm-inline--current-text)
   (or start 0)
   (or end 0)))

(defun commonmark-gfm-inline--parse-reference-link-like
    (text label label-start label-end type)
  "Parse a reference TYPE in TEXT after LABEL ending at LABEL-END."
  (let ((pos (1+ label-end))
        (node-start (- label-start (if (eq type 'image) 2 1)))
        reference-label
        end
        attrs)
    (cond
     ;; Full or collapsed reference: [text][label] or [text][].
     ((and (< pos (length text))
           (= (aref text pos) ?\[))
      (setq end (commonmark-gfm-inline--find-closing-bracket text (1+ pos)))
      (when end
        (setq reference-label (substring text (1+ pos) end))
        (when (string-empty-p reference-label)
          (setq reference-label label))
        (setq attrs (commonmark-gfm-inline--reference-definition
                     reference-label))
        (when attrs
          (cons (commonmark-gfm-inline--make-link-node
                 type label attrs text node-start
                 (1+ end) label-start label-end)
                (1+ end)))))
     ;; Shortcut reference: [label].
     (t
      (setq attrs (commonmark-gfm-inline--reference-definition label))
      (when attrs
        (cons (commonmark-gfm-inline--make-link-node
               type label attrs text node-start
               pos label-start label-end)
              pos))))))

(defun commonmark-gfm-inline--parse-link-like (text pos image)
  "Parse a link or image in TEXT at POS.
When IMAGE is non-nil, parse image syntax.  Return (NODE . END) or nil."
  (let* ((label-start (+ pos (if image 2 1)))
         (label-end (commonmark-gfm-inline--find-closing-bracket text label-start))
         (open-paren (and label-end (1+ label-end))))
    (or
     (when (and label-end
                (or image
                    (not (commonmark-gfm-inline--label-contains-link-p
                          text label-start label-end)))
                (< open-paren (length text))
                (= (aref text open-paren) ?\())
       (let ((close-paren (commonmark-gfm-inline--find-closing-paren
                           text (1+ open-paren))))
         (when close-paren
           (let* ((spec (substring text (1+ open-paren) close-paren))
                  (attrs (commonmark-gfm-inline--parse-destination-title spec))
                  (type (if image 'image 'link)))
             (when attrs
               (cons (commonmark-gfm-inline--set-sourcepos
                      (commonmark-gfm-make-node
                       type
                       :children (commonmark-gfm-inline--parse-child
                                  text label-start label-end)
                       :attrs attrs)
                      text
                      pos
                      (1+ close-paren))
                     (1+ close-paren)))))))
     (when (and label-end
                (or image
                    (not (commonmark-gfm-inline--label-contains-link-p
                          text label-start label-end))))
       (commonmark-gfm-inline--parse-reference-link-like
        text
        (substring text label-start label-end)
        label-start
        label-end
        (if image 'image 'link))))))

(defun commonmark-gfm-inline--parse-delimited (text pos delimiter type)
  "Parse DELIMITER-delimited inline TYPE in TEXT at POS."
  (let* ((body-start (+ pos (length delimiter)))
         (body-end (commonmark-gfm-inline--find-substring delimiter text body-start)))
    (when body-end
      (cons (commonmark-gfm-inline--set-sourcepos
             (commonmark-gfm-make-node
              type
              :children (commonmark-gfm-inline--parse-child
                         text body-start body-end))
             text
             pos
            (+ body-end (length delimiter)))
           (+ body-end (length delimiter))))))

(defun commonmark-gfm-inline--parse-strikethrough (text pos)
  "Parse GFM strikethrough at POS.
GFM accepts matching pairs of one or two tildes.  Runs of three or more
tildes are literal text."
  (when commonmark-gfm-enable-gfm
    (let* ((run-length (commonmark-gfm-inline--tilde-run-length text pos))
           (delimiter (and (<= run-length 2)
                           (make-string run-length ?~))))
      (when delimiter
        (commonmark-gfm-inline--parse-delimited
         text pos delimiter 'strikethrough)))))

(defun commonmark-gfm-inline--find-html-tag-end (text pos)
  "Return the end position for an HTML tag at POS, or nil."
  (let ((index (1+ pos))
        quote
        found)
    (while (and (< index (length text)) (not found))
      (let ((char (aref text index)))
        (cond
         (quote
          (when (= char quote)
            (setq quote nil)))
         ((memq char '(?\" ?\'))
          (setq quote char))
         ((= char ?>)
          (setq found (1+ index)))))
      (setq index (1+ index)))
    found))

(defun commonmark-gfm-inline--html-open-tag-p (literal)
  "Return non-nil when LITERAL is a CommonMark inline HTML open tag."
  (string-match-p
   (concat "\\`<[A-Za-z][A-Za-z0-9-]*"
           "\\(?:[ \t\n]+[A-Za-z_:][A-Za-z0-9_.:-]*"
           "\\(?:[ \t\n]*=[ \t\n]*"
           "\\(?:[^ \t\n\"'=<>`]+\\|'[^']*'\\|\"[^\"]*\"\\)"
           "\\)?\\)*[ \t\n]*/?>\\'")
   literal))

(defun commonmark-gfm-inline--html-close-tag-p (literal)
  "Return non-nil when LITERAL is a CommonMark inline HTML close tag."
  (string-match-p "\\`</[A-Za-z][A-Za-z0-9-]*[ \t\n]*>\\'" literal))

(defun commonmark-gfm-inline--html-inline-end (text pos)
  "Return the end position for inline HTML at POS, or nil."
  (cond
   ((commonmark-gfm-inline--starts-with-p "<!--" text pos)
    (cond
     ((commonmark-gfm-inline--starts-with-p "<!-->" text pos)
      (+ pos 5))
     ((commonmark-gfm-inline--starts-with-p "<!--->" text pos)
      (+ pos 6))
     (t
      (when-let ((end (commonmark-gfm-inline--find-substring
                       "-->" text (+ pos 4))))
        (+ end 3)))))
   ((commonmark-gfm-inline--starts-with-p "<?" text pos)
    (when-let ((end (commonmark-gfm-inline--find-substring "?>" text (+ pos 2))))
      (+ end 2)))
   ((commonmark-gfm-inline--starts-with-p "<![CDATA[" text pos)
    (when-let ((end (commonmark-gfm-inline--find-substring "]]>" text (+ pos 9))))
      (+ end 3)))
   ((and (< (+ pos 2) (length text))
         (= (aref text (1+ pos)) ?!)
         (let ((char (aref text (+ pos 2))))
           (and (>= char ?A) (<= char ?Z))))
    (when-let ((end (commonmark-gfm-inline--find-substring ">" text (+ pos 2))))
      (1+ end)))
   ((and (< (1+ pos) (length text))
         (or (= (aref text (1+ pos)) ?/)
             (and (let ((char (aref text (1+ pos))))
                    (or (and (>= char ?A) (<= char ?Z))
                        (and (>= char ?a) (<= char ?z)))))))
    (when-let ((end (commonmark-gfm-inline--find-html-tag-end text pos)))
      (let ((literal (substring text pos end)))
        (and (or (commonmark-gfm-inline--html-open-tag-p literal)
                 (commonmark-gfm-inline--html-close-tag-p literal))
             end))))))

(defun commonmark-gfm-inline--parse-autolink (text pos)
  "Parse an autolink or inline HTML fragment in TEXT at POS."
  (let ((end (and (< (1+ pos) (length text))
                  (string-match-p ">" text (1+ pos)))))
    (when end
      (let ((body (substring text (1+ pos) end)))
        (cond
         ((string-match-p "\\`[A-Za-z][A-Za-z0-9.+-]\\{1,31\\}:[^<>[:space:]]*\\'"
                          body)
          (cons (commonmark-gfm-inline--set-sourcepos
                 (commonmark-gfm-make-node
                  'link
                  :attrs `((destination . ,body))
                  :children (list (commonmark-gfm-inline--set-sourcepos
                                   (commonmark-gfm-make-node
                                    'text
                                    :literal body)
                                   text
                                   (1+ pos)
                                   end)))
                 text
                 pos
                 (1+ end))
                (1+ end)))
         ((string-match-p
           "\\`[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+\\.[A-Za-z][A-Za-z0-9-]*\\'"
           body)
          (cons (commonmark-gfm-inline--set-sourcepos
                 (commonmark-gfm-make-node
                  'link
                  :attrs `((destination . ,(concat "mailto:" body)))
                  :children (list (commonmark-gfm-inline--set-sourcepos
                                   (commonmark-gfm-make-node
                                    'text
                                    :literal body)
                                   text
                                   (1+ pos)
                                   end)))
                 text
                 pos
                 (1+ end))
                (1+ end)))
         ((commonmark-gfm-inline--html-inline-end text pos)
          (let* ((html-end (commonmark-gfm-inline--html-inline-end text pos))
                 (literal (substring text pos html-end)))
          (cons (commonmark-gfm-inline--set-sourcepos
                 (commonmark-gfm-make-node 'html-inline :literal literal)
                 text
                 pos
                 html-end)
                html-end))))))))

(defun commonmark-gfm-inline--parse-entity (text pos)
  "Parse a character reference in TEXT at POS.
Return (LITERAL . END) on success, or nil."
  (when (and (string-match "&\\(#\\(?:[xX][0-9A-Fa-f]+\\|[0-9]+\\)\\|[A-Za-z][A-Za-z0-9]+\\);" text pos)
             (= (match-beginning 0) pos))
    (let ((body (match-string 1 text))
          (end (match-end 0))
          literal)
      (setq literal (commonmark-gfm-inline--entity-literal body))
      (when literal
        (cons literal end)))))

;;;###autoload
(defun commonmark-gfm-inline-parse (text &optional references)
  "Parse inline Markdown TEXT and return a list of AST nodes.

This is a bootstrap parser.  It is intentionally incomplete and will be
replaced by a CommonMark delimiter stack parser as compatibility work
progresses.  REFERENCES is an optional link reference definition alist."
  (let ((commonmark-gfm-inline-reference-definitions
         (or references commonmark-gfm-inline-reference-definitions))
        (commonmark-gfm-inline--current-text
         (or commonmark-gfm-inline--current-text text))
        (pos 0)
        (len (length text))
        nodes)
    (while (< pos len)
      (let ((char (aref text pos))
            parsed)
        (setq parsed (commonmark-gfm-inline--parse-gfm-autolink-at
                      text pos))
        (cond
         (parsed
          (push (car parsed) nodes)
          (setq pos (cdr parsed)))
         ((and (= char ?\\)
               (< (1+ pos) len)
               (string-match-p
                (regexp-quote (char-to-string (aref text (1+ pos))))
                commonmark-gfm-inline--escapable-punctuation))
          (setq nodes (commonmark-gfm-inline--push-text
                       (char-to-string (aref text (1+ pos)))
                       nodes
                       pos
                       (+ pos 2)))
          (setq pos (+ pos 2)))
         ((= char ?\n)
          (let ((newline (commonmark-gfm-inline--prepare-newline nodes)))
            (setq nodes (cdr newline))
            (push (commonmark-gfm-inline--set-sourcepos
                   (commonmark-gfm-make-node
                    (if (car newline) 'linebreak 'softbreak))
                   text
                   pos
                   (1+ pos))
                  nodes))
          (setq pos (1+ pos)))
        ((= char ?`)
          (setq parsed (commonmark-gfm-inline--parse-code-span text pos))
          (if parsed
              (progn
                (push (car parsed) nodes)
                (setq pos (cdr parsed)))
            (let ((run-length
                   (commonmark-gfm-inline--backtick-run-length text pos)))
              (setq nodes (commonmark-gfm-inline--push-text
                           (make-string run-length ?`)
                           nodes
                           pos
                           (+ pos run-length)))
              (setq pos (+ pos run-length)))))
         ((commonmark-gfm-inline--starts-with-p "![" text pos)
          (setq parsed (commonmark-gfm-inline--parse-link-like text pos t))
          (if parsed
              (progn
                (push (car parsed) nodes)
                (setq pos (cdr parsed)))
            (setq nodes (commonmark-gfm-inline--push-text
                         "!" nodes pos (1+ pos)))
            (setq pos (1+ pos))))
         ((= char ?\[)
          (setq parsed (commonmark-gfm-inline--parse-link-like text pos nil))
          (if parsed
              (progn
                (push (car parsed) nodes)
                (setq pos (cdr parsed)))
            (setq nodes (commonmark-gfm-inline--push-text
                         "[" nodes pos (1+ pos)))
            (setq pos (1+ pos))))
         ((commonmark-gfm-inline--starts-with-p "***" text pos)
          (pcase-let ((`(,new-nodes . ,new-pos)
                       (commonmark-gfm-inline--parse-emphasis-or-fallback
                        text pos nodes 3)))
            (setq nodes new-nodes
                  pos new-pos)))
         ((commonmark-gfm-inline--starts-with-p "___" text pos)
          (pcase-let ((`(,new-nodes . ,new-pos)
                       (commonmark-gfm-inline--parse-emphasis-or-fallback
                        text pos nodes 3)))
            (setq nodes new-nodes
                  pos new-pos)))
         ((commonmark-gfm-inline--starts-with-p "**" text pos)
          (pcase-let ((`(,new-nodes . ,new-pos)
                       (commonmark-gfm-inline--parse-emphasis-or-fallback
                        text pos nodes 2)))
            (setq nodes new-nodes
                  pos new-pos)))
         ((commonmark-gfm-inline--starts-with-p "__" text pos)
          (pcase-let ((`(,new-nodes . ,new-pos)
                       (commonmark-gfm-inline--parse-emphasis-or-fallback
                        text pos nodes 2)))
            (setq nodes new-nodes
                  pos new-pos)))
         ((= char ?~)
          (setq parsed (commonmark-gfm-inline--parse-strikethrough text pos))
          (if parsed
              (progn
                (push (car parsed) nodes)
                (setq pos (cdr parsed)))
            (let ((run-length (commonmark-gfm-inline--tilde-run-length
                               text pos)))
              (setq nodes (commonmark-gfm-inline--push-text
                           (make-string run-length ?~)
                           nodes
                           pos
                           (+ pos run-length)))
              (setq pos (+ pos run-length)))))
         ((= char ?*)
          (pcase-let ((`(,new-nodes . ,new-pos)
                       (commonmark-gfm-inline--parse-emphasis-or-fallback
                        text pos nodes 1)))
            (setq nodes new-nodes
                  pos new-pos)))
         ((= char ?_)
          (pcase-let ((`(,new-nodes . ,new-pos)
                       (commonmark-gfm-inline--parse-emphasis-or-fallback
                        text pos nodes 1)))
            (setq nodes new-nodes
                  pos new-pos)))
         ((= char ?<)
          (setq parsed (commonmark-gfm-inline--parse-autolink text pos))
          (if parsed
              (progn
                (push (car parsed) nodes)
                (setq pos (cdr parsed)))
            (setq nodes (commonmark-gfm-inline--push-text
                         "<" nodes pos (1+ pos)))
            (setq pos (1+ pos))))
         ((and (= char ?&)
               (setq parsed (commonmark-gfm-inline--parse-entity text pos)))
          (setq nodes (commonmark-gfm-inline--push-text
                       (car parsed) nodes pos (cdr parsed)))
          (setq pos (cdr parsed)))
         (t
          (let ((start pos))
            (while (and (< pos len)
                        (not (or (memq (aref text pos)
                                       '(?\\ ?\n ?` ?! ?\[ ?* ?_ ?~ ?<))
                                 (and (= (aref text pos) ?&)
                                      (commonmark-gfm-inline--parse-entity
                                       text pos)))))
              (setq pos (1+ pos)))
            (if (= start pos)
                (progn
                  (setq nodes (commonmark-gfm-inline--push-text
                               (char-to-string (aref text pos))
                               nodes
                               pos
                               (1+ pos)))
                  (setq pos (1+ pos)))
              (setq nodes (commonmark-gfm-inline--push-text-with-gfm-autolinks
                           (substring text start pos) nodes start))))))))
    (nreverse (commonmark-gfm-inline--trim-final-text nodes))))

;;;###autoload
(defun commonmark-gfm-inline-parse-with-sourcepos
    (text start-line start-column &optional references)
  "Parse inline Markdown TEXT with source positions.
START-LINE and START-COLUMN are one-based positions for the start of TEXT.
REFERENCES is an optional link reference definition alist."
  (let ((commonmark-gfm-inline--sourcepos-enabled t)
        (commonmark-gfm-inline--source-line start-line)
        (commonmark-gfm-inline--source-column start-column)
        (commonmark-gfm-inline--current-text text))
    (commonmark-gfm-inline-parse text references)))

(provide 'commonmark-gfm-inline)

;;; commonmark-gfm-inline.el ends here
