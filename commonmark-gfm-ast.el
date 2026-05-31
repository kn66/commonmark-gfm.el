;;; commonmark-gfm-ast.el --- AST nodes for commonmark-gfm -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Small AST helpers shared by the parser and renderers.

;;; Code:

(require 'cl-lib)

(cl-defstruct (commonmark-gfm-node
               (:constructor commonmark-gfm-node-create
                             (&key type literal children attrs sourcepos)))
  "A CommonMark/GFM syntax tree node.

TYPE is a symbol such as `document', `paragraph', or `text'.
LITERAL is node text for leaf nodes.
CHILDREN is a list of child nodes.
ATTRS is an alist for renderer-relevant metadata.
SOURCEPOS is reserved for source position data."
  type
  literal
  children
  attrs
  sourcepos)

(defun commonmark-gfm-make-node (type &rest plist)
  "Return a node of TYPE using keyword values from PLIST."
  (commonmark-gfm-node-create
   :type type
   :literal (plist-get plist :literal)
   :children (plist-get plist :children)
   :attrs (plist-get plist :attrs)
   :sourcepos (plist-get plist :sourcepos)))

(defun commonmark-gfm-node-attr (node key &optional default)
  "Return NODE attribute KEY, or DEFAULT when KEY is absent."
  (let ((cell (assq key (commonmark-gfm-node-attrs node))))
    (if cell (cdr cell) default)))

(defun commonmark-gfm-node-set-attr (node key value)
  "Set NODE attribute KEY to VALUE and return VALUE."
  (let ((cell (assq key (commonmark-gfm-node-attrs node))))
    (if cell
        (setcdr cell value)
      (setf (commonmark-gfm-node-attrs node)
            (cons (cons key value)
                  (commonmark-gfm-node-attrs node)))))
  value)

(defun commonmark-gfm-node-append-child (node child)
  "Append CHILD to NODE and return CHILD."
  (setf (commonmark-gfm-node-children node)
        (append (commonmark-gfm-node-children node) (list child)))
  child)

(provide 'commonmark-gfm-ast)

;;; commonmark-gfm-ast.el ends here
