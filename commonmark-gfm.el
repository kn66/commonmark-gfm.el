;;; commonmark-gfm.el --- CommonMark/GFM renderer in Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66

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

(defvar markdown-command)
(defvar markdown-command-needs-filename)

(defun commonmark-gfm--gfm-enabled-p (options)
  "Return whether GFM extensions should be enabled for OPTIONS."
  (if (plist-member options :gfm)
      (plist-get options :gfm)
    commonmark-gfm-enable-gfm))

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
OPTIONS is reserved for future compatibility controls."
  (let ((commonmark-gfm-enable-gfm
         (commonmark-gfm--gfm-enabled-p options)))
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
