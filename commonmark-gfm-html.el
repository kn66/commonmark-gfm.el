;;; commonmark-gfm-html.el --- HTML renderer for commonmark-gfm -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; HTML rendering for the package AST.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'commonmark-gfm-ast)

(defvar commonmark-gfm-enable-gfm t
  "Whether GFM-specific HTML rendering extensions are enabled.")

(defvar commonmark-gfm-html-include-default-css nil
  "Whether `commonmark-gfm-html-render' includes the default CSS.")

(defvar commonmark-gfm-html-user-css nil
  "Additional CSS included by `commonmark-gfm-html-render'.")

(defvar commonmark-gfm-html-include-mermaid-script nil
  "Whether `commonmark-gfm-html-render' includes Mermaid.js initialization.")

(defvar commonmark-gfm-html-mermaid-script-url
  "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.esm.min.mjs"
  "Mermaid.js ES module URL used by `commonmark-gfm-html-render'.")

(defvar commonmark-gfm-html--parent-inline-type nil
  "Inline node type currently rendering this node's children.")

(defconst commonmark-gfm-html-default-css
  "body {
  color: #1f2328;
  background: #ffffff;
  font-family: system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif;
  line-height: 1.6;
  max-width: 860px;
  margin: 0 auto;
  padding: 32px 20px;
}

h1, h2, h3, h4, h5, h6 {
  line-height: 1.25;
  margin: 1.6em 0 0.65em;
}

h1, h2 {
  border-bottom: 1px solid #d0d7de;
  padding-bottom: 0.3em;
}

p, blockquote, ul, ol, table, pre {
  margin: 0 0 1em;
}

a {
  color: #0969da;
}

blockquote {
  color: #57606a;
  border-left: 4px solid #d0d7de;
  padding-left: 1em;
}

code {
  background: #f6f8fa;
  border-radius: 4px;
  font-family: ui-monospace, SFMono-Regular, Consolas, \"Liberation Mono\", monospace;
  font-size: 0.92em;
  padding: 0.12em 0.35em;
}

pre {
  background: #f6f8fa;
  border-radius: 6px;
  overflow: auto;
  padding: 16px;
}

pre code {
  background: transparent;
  border-radius: 0;
  display: block;
  padding: 0;
}

table {
  border-collapse: collapse;
  display: block;
  overflow-x: auto;
  width: max-content;
  max-width: 100%;
}

th, td {
  border: 1px solid #d0d7de;
  padding: 6px 13px;
}

th {
  background: #f6f8fa;
  font-weight: 600;
}

tr:nth-child(even) {
  background: #f6f8fa;
}

input[type=\"checkbox\"] {
  margin-right: 0.35em;
}

.mermaid {
  margin: 1.25em 0;
  text-align: center;
}
"
  "Default CSS optionally included in rendered HTML.")

(defconst commonmark-gfm-html--tagfilter-tags
  '("title" "textarea" "style" "xmp" "iframe" "noembed" "noframes" "script"
    "plaintext")
  "Raw HTML tags filtered by the GFM tagfilter extension.")

(defun commonmark-gfm-html-escape (text)
  "Escape TEXT for HTML text content."
  (let ((text (or text "")))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (setq text (replace-regexp-in-string ">" "&gt;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    text))

(defun commonmark-gfm-html-escape-attribute (text)
  "Escape TEXT for an HTML attribute value."
  (commonmark-gfm-html-escape text))

(defun commonmark-gfm-html--percent-encode-char (char)
  "Return percent-encoded UTF-8 representation of CHAR."
  (let ((bytes (encode-coding-string (char-to-string char) 'utf-8-unix))
        (index 0)
        parts)
    (while (< index (length bytes))
      (push (format "%%%02X" (aref bytes index)) parts)
      (setq index (1+ index)))
    (apply #'concat (nreverse parts))))

(defun commonmark-gfm-html-normalize-uri (uri)
  "Normalize URI for an HTML link attribute."
  (let ((uri (or uri ""))
        parts)
    (mapc
     (lambda (char)
       (push (if (or (> char 127)
                     (<= char 32)
                     (= char 127)
                     (memq char '(?\" ?< ?> ?\\ ?` ?\[ ?\])))
                 (commonmark-gfm-html--percent-encode-char char)
               (char-to-string char))
             parts))
     uri)
    (apply #'concat (nreverse parts))))

(defun commonmark-gfm-html--render-children (node)
  "Render NODE children to HTML."
  (mapconcat #'commonmark-gfm-html-render
             (commonmark-gfm-node-children node)
             ""))

(defun commonmark-gfm-html--style-block ()
  "Return optional CSS style block for rendered documents."
  (let* ((default-css (and commonmark-gfm-html-include-default-css
                           commonmark-gfm-html-default-css))
         (css (string-join
               (delq nil (list default-css commonmark-gfm-html-user-css))
               "\n")))
    (if (string-empty-p css)
        ""
      (format "<style>\n%s</style>\n" css))))

(defun commonmark-gfm-html--javascript-string (text)
  "Return TEXT escaped for a double-quoted JavaScript string."
  (let ((text (or text "")))
    (setq text (replace-regexp-in-string "\\\\" "\\\\\\\\" text t t))
    (setq text (replace-regexp-in-string "\"" "\\\\\"" text t t))
    (setq text (replace-regexp-in-string "\n" "\\\\n" text t t))
    (replace-regexp-in-string "\r" "\\\\r" text t t)))

(defun commonmark-gfm-html--mermaid-script-block ()
  "Return optional Mermaid.js script block for rendered documents."
  (if (not commonmark-gfm-html-include-mermaid-script)
      ""
    (format
     (concat "<script type=\"module\">\n"
             "import mermaid from \"%s\";\n"
             "mermaid.initialize({ startOnLoad: true });\n"
             "</script>\n")
     (commonmark-gfm-html--javascript-string
      commonmark-gfm-html-mermaid-script-url))))

(defun commonmark-gfm-html--plain-text (nodes)
  "Render NODES as plain text for image alt attributes."
  (mapconcat
   (lambda (node)
     (pcase (commonmark-gfm-node-type node)
       ('text (or (commonmark-gfm-node-literal node) ""))
       ('code (or (commonmark-gfm-node-literal node) ""))
       ('softbreak "\n")
       ((or 'emph 'strong 'link 'image)
        (commonmark-gfm-html--plain-text
         (commonmark-gfm-node-children node)))
       (_ "")))
   nodes
   ""))

(defun commonmark-gfm-html--code-class (node)
  "Return code language class attribute for NODE, or an empty string."
  (let* ((info (commonmark-gfm-node-attr node 'info ""))
         (language (car (split-string info "[ \t]+" t))))
    (if language
        (format " class=\"language-%s\""
                (commonmark-gfm-html-escape-attribute language))
      "")))

(defun commonmark-gfm-html--code-language (node)
  "Return the first word of NODE's code block info string, or nil."
  (car (split-string (commonmark-gfm-node-attr node 'info "") "[ \t]+" t)))

(defun commonmark-gfm-html--mermaid-code-block-p (node)
  "Return non-nil when NODE is a Mermaid fenced code block."
  (string= (commonmark-gfm-html--code-language node) "mermaid"))

(defun commonmark-gfm-html--tagfilter (html)
  "Apply the GFM tagfilter extension to raw HTML."
  (if (not commonmark-gfm-enable-gfm)
      html
    (let ((case-fold-search t)
          (regexp (format
                   "<\\(/?\\)\\(%s\\)\\(?:[ \t\n\f/>]\\|\\'\\)"
                   (regexp-opt commonmark-gfm-html--tagfilter-tags))))
      (replace-regexp-in-string
       regexp
       (lambda (match)
         (concat "&lt;" (substring match 1)))
       html
       t
       t))))

(defun commonmark-gfm-html--render-checkbox (state)
  "Render a GFM task-list checkbox for STATE."
  (pcase state
    ('checked "<input checked=\"\" disabled=\"\" type=\"checkbox\"> ")
    ('unchecked "<input disabled=\"\" type=\"checkbox\"> ")
    (_ "")))

(defun commonmark-gfm-html--render-tight-child (node)
  "Render NODE inside a tight list item."
  (pcase (commonmark-gfm-node-type node)
    ('paragraph
     (commonmark-gfm-html--render-children node))
    (_
     (commonmark-gfm-html-render node))))

(defun commonmark-gfm-html--render-tight-children (children)
  "Render CHILDREN inside a tight list item."
  (let (parts previous-type previous-rendered)
    (dolist (child children)
      (let* ((type (commonmark-gfm-node-type child))
             (rendered (commonmark-gfm-html--render-tight-child child)))
        (when (and parts
                   (or (eq previous-type 'paragraph)
                       (and (eq type 'paragraph)
                            (not (string-suffix-p "\n" previous-rendered)))))
          (push "\n" parts))
        (push rendered parts)
        (setq previous-type type
              previous-rendered rendered)))
    (apply #'concat (nreverse parts))))

(defun commonmark-gfm-html--render-list-item (node)
  "Render list item NODE to HTML."
  (let* ((tight (commonmark-gfm-node-attr node 'tight))
         (task-state (commonmark-gfm-node-attr node 'task-state))
         (children (commonmark-gfm-node-children node))
         (checkbox (commonmark-gfm-html--render-checkbox task-state))
         (body (if tight
                   (commonmark-gfm-html--render-tight-children children)
                 (commonmark-gfm-html--render-children node))))
    (if (and (null children) (string-empty-p checkbox))
        "<li></li>\n"
      (if (or (not tight)
            (and children
                 (not (eq (commonmark-gfm-node-type (car children))
                          'paragraph))))
        (format "<li>%s\n%s</li>\n" checkbox body)
        (format "<li>%s%s</li>\n" checkbox body)))))

(defun commonmark-gfm-html--render-list (node)
  "Render list NODE to HTML."
  (let* ((type (commonmark-gfm-node-attr node 'type))
         (ordered (eq type 'ordered))
         (tag (if ordered "ol" "ul"))
         (start (commonmark-gfm-node-attr node 'start))
         (start-attr (if (and ordered start (/= start 1))
                         (format " start=\"%d\"" start)
                       "")))
    (format "<%s%s>\n%s</%s>\n"
            tag
            start-attr
            (mapconcat #'commonmark-gfm-html--render-list-item
                       (commonmark-gfm-node-children node)
                       "")
            tag)))

(defun commonmark-gfm-html--table-align-attr (align)
  "Return HTML alignment attribute for ALIGN."
  (pcase align
    ('left " align=\"left\"")
    ('right " align=\"right\"")
    ('center " align=\"center\"")
    (_ "")))

(defun commonmark-gfm-html--render-table-cell (node)
  "Render table cell NODE to HTML."
  (let ((tag (if (commonmark-gfm-node-attr node 'header) "th" "td"))
        (align (commonmark-gfm-node-attr node 'align)))
    (format "<%s%s>%s</%s>\n"
            tag
            (commonmark-gfm-html--table-align-attr align)
            (commonmark-gfm-html--render-children node)
            tag)))

(defun commonmark-gfm-html--render-table-row (node)
  "Render table row NODE to HTML."
  (format "<tr>\n%s</tr>\n"
          (mapconcat #'commonmark-gfm-html--render-table-cell
                     (commonmark-gfm-node-children node)
                     "")))

(defun commonmark-gfm-html--render-table (node)
  "Render table NODE to HTML."
  (let* ((rows (commonmark-gfm-node-children node))
         (head (car rows))
         (body (cdr rows)))
    (concat "<table>\n<thead>\n"
            (when head (commonmark-gfm-html--render-table-row head))
            "</thead>\n"
            (when body
              (concat "<tbody>\n"
                      (mapconcat #'commonmark-gfm-html--render-table-row
                                 body
                                 "")
                      "</tbody>\n"))
            "</table>\n")))

;;;###autoload
(defun commonmark-gfm-html-render (node)
  "Render AST NODE to HTML."
  (pcase (commonmark-gfm-node-type node)
    ('document
     (concat (commonmark-gfm-html--style-block)
             (commonmark-gfm-html--render-children node)
             (commonmark-gfm-html--mermaid-script-block)))
    ('paragraph
     (format "<p>%s</p>\n"
             (commonmark-gfm-html--render-children node)))
    ('heading
     (let ((level (commonmark-gfm-node-attr node 'level 1)))
       (format "<h%d>%s</h%d>\n"
               level
               (commonmark-gfm-html--render-children node)
               level)))
    ('code-block
     (let ((literal (commonmark-gfm-html-escape
                     (commonmark-gfm-node-literal node))))
       (if (commonmark-gfm-html--mermaid-code-block-p node)
           (format "<div class=\"mermaid\">%s</div>\n" literal)
         (format "<pre><code%s>%s</code></pre>\n"
                 (commonmark-gfm-html--code-class node)
                 literal))))
    ('html-block
     (commonmark-gfm-html--tagfilter
      (or (commonmark-gfm-node-literal node) "")))
    ('thematic-break
     "<hr />\n")
    ('block-quote
     (format "<blockquote>\n%s</blockquote>\n"
             (commonmark-gfm-html--render-children node)))
    ('list
     (commonmark-gfm-html--render-list node))
    ('table
     (commonmark-gfm-html--render-table node))
    ('table-row
     (commonmark-gfm-html--render-table-row node))
    ('table-cell
     (commonmark-gfm-html--render-table-cell node))
    ('text
     (commonmark-gfm-html-escape (commonmark-gfm-node-literal node)))
    ('softbreak
     "\n")
    ('linebreak
     "<br />\n")
    ('code
     (format "<code>%s</code>"
             (commonmark-gfm-html-escape
              (commonmark-gfm-node-literal node))))
    ('emph
     (format "<em>%s</em>"
             (let ((commonmark-gfm-html--parent-inline-type 'emph))
               (commonmark-gfm-html--render-children node))))
    ('strong
     (let ((children
            (let ((commonmark-gfm-html--parent-inline-type 'strong))
              (commonmark-gfm-html--render-children node))))
       (if (and commonmark-gfm-enable-gfm
                (eq commonmark-gfm-html--parent-inline-type 'strong))
           children
         (format "<strong>%s</strong>" children))))
    ('strikethrough
     (format "<del>%s</del>"
             (let ((commonmark-gfm-html--parent-inline-type 'strikethrough))
               (commonmark-gfm-html--render-children node))))
    ('link
     (let ((destination (commonmark-gfm-node-attr node 'destination ""))
           (title (commonmark-gfm-node-attr node 'title)))
       (format "<a href=\"%s\"%s>%s</a>"
               (commonmark-gfm-html-escape-attribute
                (commonmark-gfm-html-normalize-uri destination))
               (if title
                   (format " title=\"%s\""
                           (commonmark-gfm-html-escape-attribute title))
                 "")
               (let ((commonmark-gfm-html--parent-inline-type 'link))
                 (commonmark-gfm-html--render-children node)))))
    ('image
     (let ((destination (commonmark-gfm-node-attr node 'destination ""))
           (title (commonmark-gfm-node-attr node 'title))
           (alt (commonmark-gfm-html--plain-text
                 (commonmark-gfm-node-children node))))
       (format "<img src=\"%s\" alt=\"%s\"%s />"
               (commonmark-gfm-html-escape-attribute
                (commonmark-gfm-html-normalize-uri destination))
               (commonmark-gfm-html-escape-attribute alt)
               (if title
                   (format " title=\"%s\""
                           (commonmark-gfm-html-escape-attribute title))
                 ""))))
    ('html-inline
     (commonmark-gfm-html--tagfilter
      (or (commonmark-gfm-node-literal node) "")))
    (_
     "")))

(provide 'commonmark-gfm-html)

;;; commonmark-gfm-html.el ends here
