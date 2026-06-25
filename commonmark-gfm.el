;;; commonmark-gfm.el --- CommonMark/GFM renderer in Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66
;; SPDX-License-Identifier: MIT

;; Author: kn66
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hypermedia, markdown, commonmark, gfm
;; URL: https://github.com/kn66/commonmark-gfm.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A pure Emacs Lisp CommonMark/GFM renderer scaffold.
;;
;; The current parser is intentionally small.  The package API and test hooks
;; are laid out so the implementation can grow toward CommonMark/GFM spec
;; compatibility without changing user-facing entry points.

;;; Code:

(require 'commonmark-gfm-ast)
(require 'commonmark-gfm-block)
(require 'commonmark-gfm-html)

(defgroup commonmark-gfm nil
  "CommonMark/GFM rendering in Emacs Lisp."
  :group 'hypermedia
  :prefix "commonmark-gfm-")

(defcustom commonmark-gfm-enable-gfm t
  "Whether GFM extensions should be enabled when implemented."
  :type 'boolean
  :group 'commonmark-gfm)

(defcustom commonmark-gfm-html-include-default-css nil
  "Whether `commonmark-gfm-render-to-html' includes the default CSS.
The default is nil so rendered output remains a plain HTML fragment unless
styling is explicitly requested."
  :type 'boolean
  :group 'commonmark-gfm)

(defcustom commonmark-gfm-html-user-css nil
  "Additional CSS included by `commonmark-gfm-render-to-html'.
When non-nil, this string is inserted after the optional default CSS inside a
single style block."
  :type '(choice (const :tag "No additional CSS" nil)
                 string)
  :group 'commonmark-gfm)

(defcustom commonmark-gfm-html-include-mermaid-script nil
  "Whether `commonmark-gfm-render-to-html' includes Mermaid.js initialization.
The default is nil so rendering never loads remote JavaScript unless explicitly
requested."
  :type 'boolean
  :group 'commonmark-gfm)

(defcustom commonmark-gfm-html-mermaid-script-url
  "https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.esm.min.mjs"
  "Mermaid.js ES module URL used by `commonmark-gfm-render-to-html'."
  :type 'string
  :group 'commonmark-gfm)

(defvar markdown-command)
(defvar markdown-command-needs-filename)

(defun commonmark-gfm--gfm-enabled-p (options)
  "Return whether GFM extensions should be enabled for OPTIONS."
  (if (plist-member options :gfm)
      (plist-get options :gfm)
    commonmark-gfm-enable-gfm))

(defun commonmark-gfm--html-include-default-css-p (options)
  "Return whether default CSS should be included for OPTIONS."
  (if (plist-member options :html-include-default-css)
      (plist-get options :html-include-default-css)
    commonmark-gfm-html-include-default-css))

(defun commonmark-gfm--html-user-css (options)
  "Return user CSS for OPTIONS."
  (if (plist-member options :html-user-css)
      (plist-get options :html-user-css)
    commonmark-gfm-html-user-css))

(defun commonmark-gfm--html-include-mermaid-script-p (options)
  "Return whether Mermaid.js initialization should be included for OPTIONS."
  (if (plist-member options :html-include-mermaid-script)
      (plist-get options :html-include-mermaid-script)
    commonmark-gfm-html-include-mermaid-script))

(defun commonmark-gfm--html-mermaid-script-url (options)
  "Return Mermaid.js URL for OPTIONS."
  (if (plist-member options :html-mermaid-script-url)
      (plist-get options :html-mermaid-script-url)
    commonmark-gfm-html-mermaid-script-url))

;;;###autoload
(defun commonmark-gfm-parse (markdown &optional options)
  "Parse MARKDOWN into a CommonMark/GFM AST.
OPTIONS is reserved for future compatibility controls."
  (let ((commonmark-gfm-enable-gfm
         (commonmark-gfm--gfm-enabled-p options)))
    (commonmark-gfm-block-parse markdown options)))

;;;###autoload
(defun commonmark-gfm-render-to-html (markdown &optional options)
  "Render MARKDOWN to HTML.
OPTIONS may include `:gfm', `:html-include-default-css', and
`:html-user-css', `:html-include-mermaid-script', and
`:html-mermaid-script-url'."
  (let ((commonmark-gfm-enable-gfm
         (commonmark-gfm--gfm-enabled-p options))
        (commonmark-gfm-html-include-default-css
         (commonmark-gfm--html-include-default-css-p options))
        (commonmark-gfm-html-user-css
         (commonmark-gfm--html-user-css options))
        (commonmark-gfm-html-include-mermaid-script
         (commonmark-gfm--html-include-mermaid-script-p options))
        (commonmark-gfm-html-mermaid-script-url
         (commonmark-gfm--html-mermaid-script-url options)))
    (commonmark-gfm-html-render (commonmark-gfm-parse markdown options))))

;;;###autoload
(defun commonmark-gfm-render-region-to-buffer (beg end output-buffer
                                                   &optional _filename)
  "Render Markdown between BEG and END into OUTPUT-BUFFER as HTML.
This function has the same shape expected by `markdown-command' when it is
bound to an Emacs Lisp function."
  (let ((html (commonmark-gfm-render-to-html
               (buffer-substring-no-properties beg end))))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert html))))

;;;###autoload
(defun commonmark-gfm-markdown-command (beg end output-buffer
                                            &optional filename)
  "Render Markdown between BEG and END into OUTPUT-BUFFER.
FILENAME is accepted for `markdown-command' compatibility."
  (commonmark-gfm-render-region-to-buffer beg end output-buffer filename))

;;;###autoload
(defun commonmark-gfm-use-as-markdown-command ()
  "Install `commonmark-gfm-markdown-command' as `markdown-command'."
  (interactive)
  (setq markdown-command #'commonmark-gfm-markdown-command)
  (setq markdown-command-needs-filename nil)
  markdown-command)

(provide 'commonmark-gfm)

;;; commonmark-gfm.el ends here
