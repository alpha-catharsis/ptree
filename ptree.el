;;; ptree.el --- Property tree data structure -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Alpha Catharsis

;; Author: Alpha Catharsis <alpha.catharsis@gmail.com>
;; Maintainer: Alpha Catharsis <alpha.catharsis@gmail.com>
;; Version: 0.1.0
;; Keywords: lisp
;; URL: https://github.com/alpha-catharsis/ptree
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ptree provides a tree data structure for storing properties.
;; The package documentation is available here:
;;
;; https://github.com/alpha-catharsis/ptree
;;
;; Please report bugs, requests and suggestions to
;; Alpha Catharsis <alpha.catharsis@gmail.com> or on Github.

;;; Code:

;; Public constructor functions

(defun ptree-create ()
  "Create an empty property tree.
The function returns the root node of the property tree."
  (list nil nil nil))

;; Public predicate functions

(defun ptree-root-p (node)
  "Check if NODE is the root node of the property tree.
The root node has no tag or value associated but can have child nodes.
The function returns 't if NODE is the root node, otherwise 'nil."
  (null (car node)))

(defun ptree-branch-p (node)
  "Check if NODE is a branch node of the property tree.
A branch node has a tag and can have child nodes, but has no value
associated.
The function returns 't if NODE is a branch node, otherwise 'nil."
  (and (not (null (car node))) (not (null (cddr node)))))

(defun ptree-leaf-p (node)
  "Check if NODE is a leaf node of the property tree.
A leaf node has a tag and value associated, but cannot have child nodes.
The function returns 't if NODE is a leaf node, otherwise 'nil."
  (null (cddr node)))

;; Public node accessor functions

(defun ptree-tag (node)
  "Get the tag associated with NODE.
If NODE is the root node, 'nil is returned."
  (car node))

(defun ptree-value (node)
  "Get the value associated with NODE.
If NODE is not a leaf node, 'not-found is returned."
  (if (ptree-leaf-p node)
      (cadr node)
    'not-found))

(defun ptree-child-num (node)
  "Get the number of child nodes of NODE."
  (let ((child-nodes (cddr node)))
    (if (car child-nodes)
        (length child-nodes)
    0)))

;; Public node retrieval functions

(defun ptree-child-at-index (node index)
  "Get the child node of NODE at position INDEX.
If INDEX is out of range, it returns 'nil."
  (nth index (cddr node)))

(defun ptree-node-at-path (node path)
  "Get the descendant of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If descendant node at PATH does not exist, the function returns 'nil."
  (ptree--node-at-path node path))

;; Public value retrieval functions

(defun ptree-value-at-path (node path)
  "Retrieve the value of the descendant of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If the descendant node at PATH does no exist or if it is not a leaf node,
the function returns 'not-found."
  (let ((desc (ptree--node-at-path node path)))
    (if (and desc (ptree-leaf-p desc))
        (ptree-value desc)
      'not-found)))

;; Public value setting functions

(defun ptree-set-value-at-path (node path value)
  "Set the value of the descendant of NODE at PATH to VALUE.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If any node but the last one in the path does not exist, it is created as
branch node.
If any node but the last one in the path is a leaf node, it is transformed
into a branch node and its value is lost.
If the descendant node is the root node, its value is not set.
If the descendant node is a branch node, it is transformed in a leaf node
before setting its value.
If the descendant node value has been set, the function returns 't,
otherwise 'nil."
  (let ((desc (ptree--create-node-at-path node path)))
    (if (ptree-root-p desc)
        nil
      (ptree--make-leaf desc value)
      t)))

;; Public node setting functions

(defun ptree-set-branches-at-path (node path &rest child-tags)
  "Add multiple branch nodes to the descendant of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If any node in the path does not exist, it is created as branch node.
If any node in the path is a leaf node, it is transformed into a branch node
and its value is lost.
A branch node is added to the descendant node for each tag passed in CHILD-TAGS.
If the tag refers to an existing branch node, it is not modified.
It the refers to an existing leaf node, it is turned into a branch and its
associated value is lost.
The function returns the node associated with the last child tag."
  (let ((desc (ptree--create-node-at-path node path))
        (last-child-node nil))
    (while child-tags
      (setq last-child-node (ptree--create-node desc (car child-tags)))
      (when (ptree--incomplete-p last-child-node)
        (ptree--make-empty-branch last-child-node))
      (setq child-tags (cdr child-tags)))
    last-child-node))

(defun ptree-set-leaves-at-path (node path &rest leaves-data)
  "Add multiple leaf nodes to the descendant of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If any node but in the path does not exist, it is created as branch node.
If any node in the path is a leaf node, it is transformed into a branch node
and its value is lost.
A leaf node is added to the descendant node for each item passed in LEAVES-DATA.
LEAVES-DATA is a list of pairs containing the leaf node tag and value.
If the node tag refers to an existing branch node, it is transformed to a leaf
node before setting its value.
It the node tag refers to an existing leaf node, it is value is overwritten.
The function returns the node associated with the last leaf node tag."
  (let ((desc (ptree--create-node-at-path node path))
        (last-child-node nil))
    (while leaves-data
      (setq last-child-node (ptree--create-node desc (caar leaves-data)))
      (ptree--make-leaf last-child-node (cadar leaves-data))
      (setq leaves-data (cdr leaves-data)))
    last-child-node))

;; Public node deletion functions

(defun ptree-delete-nodes-at-path (node path &rest child-tags)
  "Delete multiple nodes that are the descendant of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
A node is deleted from the descendant node for each tag passed in CHILD-TAGS.
If a node cannot be deleted (cause it does not exist) the function stops
and returns the list of tags associated with node that have not been deleted,
otherwise the function returns 'nil."
  (let ((desc (ptree--node-at-path node path)))
    (if desc
        (let ((in-progress t))
          (while (and in-progress child-tags)
            (if (not (ptree--delete-node desc (car child-tags)))
                (setq in-progress nil)
              (setq child-tags (cdr child-tags))))
          child-tags))
      child-tags))

;; Public interator functions

(defun ptree-iter (node)
  "Create an iterator associated with NODE.
NODE represent the initial node for the iterator.
The iterator can only move down to the child nodes of the initial node.
The iterator cannot move to the parent or sibling nodes of the initial node."
  (list node nil))

(defun ptree-iter-node (iterator)
  "Get the node associated with ITERATOR."
  (car iterator))

(defun ptree-iter-move-to-tag (iterator tag)
  "Move ITERATOR to the child of the associated node with TAG.
If such child node exists, ITERATOR is associated to the child node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (let ((child-list (ptree--get-child-list (car iterator) tag)))
    (when child-list (ptree--iter-move-to-child-node iterator child-list))
    (consp child-list)))

(defun ptree-iter-move-down (iterator)
  "Move ITERATOR to the first child of the associated node.
If a child node exists, ITERATOR is associated to the child node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (let ((child-list (cddar iterator)))
    (if (and child-list (car child-list))
        (progn
          (ptree--iter-move-to-child-node iterator child-list)
          t)
      nil)))

(defun ptree-iter-move-up (iterator)
  "Move the ITERATOR to the parent of the associated node.
If the parent node exists, ITERATOR is associated to the parent node and 't
is returned. Otherwise ITER is unchanged and 'nil is returned."
  (let ((parent-node (cadr iterator)))
    (when parent-node (ptree--iter-move-to-parent-node iterator))
    (consp parent-node)))

(defun ptree-iter-move-next (iterator)
  "Move the ITERATOR to the next sibling node.
If the sibling node exists, ITERATOR is associated to the sibling node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (if (cddr iterator)
      (progn
        (ptree--iter-move-to-next-node iterator)
        t)
    nil))

(defun ptree-iter-move-previous (iterator)
  "Move the ITERATOR to the previous sibling node.
If the sibling node exists, ITERATOR is associated to the sibling node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (let ((prev-list (ptree--get-previous-sibling-list (car iterator)
                                                     (caadr iterator))))
    (when prev-list (ptree--iter-move-to-sibling-node iterator prev-list))
    (consp prev-list)))

(defun ptree-iter-add-branch-and-move (iterator tag)
  "Add child branch node to the node associated with ITERATOR and move to it.
The child node is created with TAG.
If the branch node exists, it is not modified.
If the node exists and it is a leaf node, it is transformed into a
branch node."
  (ptree-set-branches-at-path (ptree-iter-node iterator) nil tag)
  (ptree-iter-move-to-tag iterator tag))

(defun ptree-iter-delete-node (iterator)
  "Delete node associated with ITERATOR.
If the node is a root node, it is not deleted.
If the node is deleted, the iterator is moved to the next sibling of
the deleted node when it exists. Otherwise it is move to the parent node.
The function returns 't if the node is deleted, 'nil otherwise."
  (let ((parent (caadr iterator)))
    (if parent
        (progn
          (let ((target-node (car iterator)))
            (if (not (ptree-iter-move-next iterator))
                (ptree-iter-move-up iterator))
            (ptree--delete-exact-node parent target-node)
            target-node))
      nil)))

;; Public conversion function

(defun ptree-to-string (node)
  "Convert NODE to string."
  (let ((res "")
        (iter (ptree-iter node))
        (level 0))
    (ptree-iter-move-down iter)
    (while (not (eq (ptree-iter-node iter) node))
      (setq res (concat res (ptree--node-to-string
                             (ptree-iter-node iter) level)))
      (if (ptree-iter-move-down iter)
          (setq level (+ level 1))
        (unless (ptree-iter-move-next iter)
          (let ((in-progress t))
            (while in-progress
              (if (ptree-iter-move-up iter)
                  (setq level (- level 1))
                (setq in-progress nil))
              (when (ptree-iter-move-next iter)
                (setq in-progress nil)))))))
    res))

;; Internal functions

(defun ptree--compare-tags (ltag rtag)
  "Copares LTAG with RTAG.
Integers precede symbols, that precede strings.
Integers and strings are compared using standard comparison functions.
Symbols are converted to strings and then compared using string comparison
functions.
The comparison returns 'lt if LTAG is lower than RTAG, 'eq if LTAG is
equal to RTAG, 'gt if LTAG is greater than RTAG, 'empty if RTAG is 'nil."
  (cond ((null rtag) 'empty)
        ((integerp ltag)
         (cond ((integerp rtag)
                (cond ((< ltag rtag) 'lt)
                      ((= ltag rtag) 'eq)
                      (t 'gt)))
               (t 'lt)))
        ((symbolp ltag)
         (cond ((integerp rtag) 'gt)
               ((symbolp rtag)
                (let ((lstr (symbol-name ltag))
                      (rstr (symbol-name rtag)))
                  (cond ((string< lstr rstr) 'lt)
                        ((string= lstr rstr) 'eq)
                        (t 'gt))))
               (t 'lt)))
        (t (cond ((integerp rtag) 'gt)
                 ((symbolp rtag) 'gt)
                 (t (cond ((string< ltag rtag) 'lt)
                          ((string= ltag rtag) 'eq)
                          (t 'gt)))))))

(defun ptree--get-child (node tag)
  "Get child node of NODE with TAG.
The function returns 'nil if such child node does not exist."
  (let* ((cns (cddr node))
         (cn (car cns))
         (res nil))
    (while (and cn (not res))
      (if (eq (ptree--compare-tags tag (car cn)) 'eq)
          (setq res cn)
        (setq cns (cdr cns))
        (setq cn (car cns))))
    res))

(defun ptree--get-child-list (node tag)
  "Get the list of child nodes of NODE starting with node with TAG.
The function returns 'nil if such child node does not exist."
  (let* ((cns (cddr node))
         (cn (car cns))
         (res nil))
    (while (and cn (not res))
      (if (eq (ptree--compare-tags tag (car cn)) 'eq)
          (setq res cns)
        (setq cns (cdr cns))
        (setq cn (car cns))))
    res))

(defun ptree--node-at-path (node path)
  "Get the descendant of NODE at PATH.
If the descendant node does not exist, 'nil is returned."
  (let ((path-list (if (listp path)
                       path
                     (list path))))
    (while (and node path-list)
      (let ((child (ptree--get-child node (car path-list))))
        (setq path-list (cdr path-list))
        (setq node child))))
    node)

(defun ptree--get-previous-sibling-list (node parent)
  "Get the list of siblings starting from the node before NODE with PARENT.
The function returns 'nil if the previous node does not exist."
  (let* ((cns (cddr parent))
         (pcns nil)
         (cn (car cns))
         (res nil))
    (while cn
      (if (eq cn node)
          (progn
            (setq res pcns)
            (setq cn nil))
        (setq pcns cns)
        (setq cns (cdr cns))
        (setq cn (car cns))))
    res))

(defun ptree--make-empty-branch (node)
  "Transform NODE into an empty branch node."
  (setcdr node (list nil nil)))

(defun ptree--make-leaf (node value)
  "Transform NODE into a leaf node with VALUE."
  (setcdr node (list value)))

(defun ptree--incomplete-p (node)
  "Return 't if NODE is incomplete, otherwise 'nil."
  (not (cdr node)))

(defun ptree--create-node (node tag)
  "Add a child incomplete node with TAG in NODE.
If the node already exists, the function returns it instead of creating
a new incomplete node"
  (let* ((cns (cddr node))
         (cn (car cns))
         (res nil))
    (while (not res)
      (let ((cmp (ptree--compare-tags tag (car cn))))
        (if (and (eq cmp 'gt) (cdr cns))
            (progn (setq cns (cdr cns))
                   (setq cn (car cns)))
          (setq res (list tag))
          (cond ((eq cmp 'gt) (setcdr cns (list res)))
                ((eq cmp 'empty) (setcdr node (list nil res)))
                ((eq cmp 'eq) (setq res cn))
                (t (setcdr cns (cons cn (cdr cns)))
                   (setcar cns res))))))
    res))

(defun ptree--create-node-at-path (node path)
  "Add an incomplete descendent node of NODE at PATH.
If nodes in the path do not exist, they are created.
If the descendent node already exists, the function returns it instead of
creating a new incomplete node.
The function returns the descendent node."
  (let ((path-list (if (listp path)
                       path
                     (list path))))
    (while (and node path-list)
      (let ((child (ptree--create-node node (car path-list))))
        (when (ptree--incomplete-p child)
          (ptree--make-empty-branch child))
        (setq path-list (cdr path-list))
        (setq node child))))
    node)

(defun ptree--delete-node (node tag)
  "Delete child node with TAG from NODE.
The function returns the deleted node or 'nil if such child node does
not exist."
  (let* ((cns (cddr node))
         (pcns nil)
         (cn (car cns))
         (res nil))
    (while cn
      (let ((cmp (ptree--compare-tags tag (car cn))))
        (cond ((eq cmp 'eq) (progn (setq res cn)
                                   (setq cn nil)
                                   (if (cdr cns)
                                       (progn (setcar cns (cadr cns))
                                              (setcdr cns (cddr cns)))
                                     (if (eq pcns nil)
                                         (setcdr node (list nil nil))
                                       (setcdr pcns nil)))))
              ((eq cmp 'gt) (progn (setq pcns cns)
                                   (setq cns (cdr cns))
                                   (setq cn (car cns))))
              (t (setq cn nil)))))
    res))

(defun ptree--delete-exact-node (node target-node)
  "Delete TARGET-NODE from NODE.
The function returns the deleted node."
  (let* ((cns (cddr node))
         (pcns nil)
         (cn (car cns))
         (res nil))
    (while cn
      (if (eq cn target-node)
        (progn (setq res cn)
               (setq cn nil)
               (if (cdr cns)
                   (progn (setcar cns (cadr cns))
                          (setcdr cns (cddr cns)))
                 (if (eq pcns nil)
                     (setcdr node (list nil nil))
                   (setcdr pcns nil))))
      (setq pcns cns)
      (setq cns (cdr cns))
      (setq cn (car cns)))
    res)))

(defun ptree--iter-move-to-parent-node (iterator)
  "Move the iterator ITERATOR to the parent node."
  (setcar iterator (caadr iterator))
  (setcdr iterator (cdadr iterator)))

(defun ptree--iter-move-to-child-node (iterator child-list)
  "Move the iterator ITERATOR to the first node of CHILD-LIST.
The rest of the elements of CHILD-LIST refers to the successive nodes."
  (setcdr iterator
          (cons (cons (car iterator) (cdr iterator)) (cdr child-list)))
  (setcar iterator (car child-list)))

(defun ptree--iter-move-to-next-node (iterator)
  "Move the iterator ITERATOR to the next sibling node."
  (setcar iterator (caddr iterator))
  (setcdr iterator (cons (cadr iterator) (cdddr iterator))))

(defun ptree--iter-move-to-sibling-node (iterator sibling-list)
  "Move the iterator ITERATOR to the first node of SIBLING-LIST.
The rest of the elements of CHILD-LIST refers to the successive nodes."
  (setcar iterator (car sibling-list))
  (setcdr iterator (cons
                    (cadr iterator)
                    (cdr sibling-list))))

(defun ptree--tag-to-string (tag)
  "Convert TAG to string."
  (if (stringp tag)
      (format "\"%s\"" tag)
    (format "%s" tag)))

(defun ptree--node-to-string (node level)
  "Convert NODE at LEVEL to string."
  (let ((tag-str (ptree--tag-to-string (ptree-tag node)))
        (indent-str (make-string (* 2 level) ?\s)))
       (if (ptree-leaf-p node)
           (format "%s%s: %s\n" indent-str tag-str (ptree-value node))
    (format "%s%s\n" indent-str tag-str))))

;; Package provision

 (provide 'ptree)

;;; ptree.el ends here
