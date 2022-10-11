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
  "Create an empty property tree."
  (list nil nil nil))

;; Public predicates

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

;; Public accessors

(defun ptree-get-node-tag (node)
  "Get the tag associated with NODE.
If NODE is the root node, 'nil is returned."
  (car node))

(defun ptree-get-node-value (node)
  "Get the value associated with NODE.
If NODE is not a leaf node, 'not-found is returned."
  (if (ptree-leaf-p node)
      (cadr node)
    'not-found))

(defun ptree-get-child-nodes-num (node)
  "Get the number of child nodes of NODE."
  (let ((child-nodes (cddr node)))
    (if (or (not child-nodes) (not (car child-nodes)))
        0
      (length (cddr node)))))

(defun ptree-get-child-node-at-index (node index)
  "Get the child node of NODE at position INDEX.
If INDEX is out of range, it returns 'nil."
  (nth index (cddr node)))

(defun ptree-get-node-at-path (node path)
  "Retrieve a descendant node of NODE through PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag insead of passing PATH as the list of a single tag.
If node at PATH does not exist, it returns 'nil."
  (let ((path-list (ptree--get-path-as-list path)))
    (while (and node path-list)
      (setq node (ptree--get-node node (car path-list)))
      (setq path-list (cdr path-list)))
    node))

(defun ptree-get-value-at-path (node path)
  "Retrieve the value of a descendant node of NODE through PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag insead of passing PATH as the list of a single tag.
If node at PATH does no exist or if it is not a leaf node, it returns 'nil."
  (let* ((path-list (ptree--get-path-as-list path)))
    (setq node (ptree-get-node-at-path node path-list))
    (if (and node (ptree-leaf-p node))
        (ptree-get-node-value node)
      'not-found)))

;; Public mutators

(defun ptree-set-node-value (node value)
  "Set the value of NODE to VALUE.
If NODE is the root node, its value is not set.
If NODE is a branch node, it is transformed into a leaf node and its value
is set to VALUE.
If NODE is a leaf node, its value is set to VALUE.
If the node value has been set it returns 't, otherwise 'nil."
  (if (ptree-root-p node)
      nil
    (setcdr node (list value))
    t))

(defun ptree-add-child-nodes (node &rest child-tags)
  "Add multiple branch nodes to NODE, one for each tag passed in CHILD-TAGS.
If a tag refers to an existing branch node, it is not modified.
It a tag refers to an existing leaf node, it is turned into a branch and its
associated value is lost."
  (while child-tags
    (ptree--make-empty-branch
     (ptree--create-node node (car child-tags)))
    (setq child-tags (cdr child-tags))))

(defun ptree-add-node-at-path (node path)
  "Add a descendant branch node of NODE through PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag insead of passing PATH as the list of a single tag.
If part of the path already exists it is not modified, but If the path runs
through a leaf node, it is turned into a branch node to complete the specified
path.
The function returns the last node of the path."
  (let* ((path-list (ptree--get-path-as-list path))
        (res (ptree--create-path node path-list)))
    (ptree--make-empty-branch res)
    res))

(defun ptree-add-value-at-path (node path value)
  "Add a descendant leaf node of NODE with VALUE through PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag insead of passing PATH as the list of a single tag.
If part of the path already exists it is not modified, but If the path runs
through a leaf node, it is turned into a branch node to complete the specified
path. Similary, if the last node specified in PATH is a branch node, it
is turned into a leaf node to hold VALUE.
This function returns the leaf node."
  (let* ((path-list (ptree--get-path-as-list path))
         (res (ptree--create-path node path-list)))
    (ptree--make-leaf res value)
    res))

(defun ptree-delete-child-nodes (node &rest child-tags)
  "Delete multiple child nodes from NODE, one for each tag passed in CHILD-TAGS.
If a node with the specified tag does not exist, the function stops and returns
the list of tags associated to the child nodes that have not been deleted.
Otherwise it returns 'nil."
  (let ((in-progress t))
    (while (and in-progress child-tags)
      (if (not (ptree--delete-node node (car child-tags)))
          (setq in-progress nil)
        (setq child-tags (cdr child-tags))))
  child-tags))

(defun ptree-delete-node-at-path (node path)
  "Delete a descendant node of NODE through PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag insead of passing PATH as the list of a single tag.
The function Returns the deleted node or 'nil if it the path does not exist."
  (let ((path-list (ptree--get-path-as-list path)))
    (while (and node (cdr path-list))
      (setq node (ptree--get-node node (car path-list)))
      (setq path-list (cdr path-list)))
    (when node
      (setq node (ptree--delete-node node (car path-list)))))
    node)

;; Public interator functions

(defun ptree-iter (node)
  "Create an iterator associated with NODE.
NODE represent the initial node for the iterator.
It is worth noting that The iterator cannot move to the parent node of the
initial node."
  (list node nil))

(defun ptree-iter-node (iterator)
  "Get the node associated with the ITERATOR."
  (car iterator))

(defun ptree-iter-root-p (iterator)
  "Check if the node associated with ITERATOR is the root node.
The function returns 't if the node is the root node, 'nil otherwise."
  (ptree-root-p (car iterator)))

(defun ptree-iter-branch-p (iterator)
  "Check if the node associated with ITERATOR is a branch node.
The function returns 't if the node is a branch node, 'nil otherwise."
  (ptree-branch-p (car iterator)))

(defun ptree-iter-leaf-p (iterator)
  "Check if the node associated with ITERATOR is a leaf node.
The function retutns 't if the node is a leaf node, 'nil otherwise."
  (ptree-leaf-p (car iterator)))

(defun ptree-iter-tag (iterator)
  "Get the tag of the node associated with ITERATOR.
If the node is the root node, 'nil is returned."
  (ptree-get-node-tag (car iterator)))

(defun ptree-iter-value (iterator)
  "Get the value of the node associated with ITERATOR.
If the node is not a leaf node, 'not-found is returned."
  (ptree-get-node-value (car iterator)))

(defun ptree-iter-set-value (iterator value)
  "Set the value of node associated with ITERATOR to VALUE.
If the node is the root node, its value is not set.
If the node is a branch node, it is transformed into a leaf node and its value
is set to VALUE.
If the node is a leaf node, its value is set to VALUE.
If the node value has been set it returns 't, otherwise 'nil."
  (ptree-set-node-value (car iterator) value))

(defun ptree-iter-add-value (iterator tag value)
  "Add a child node with TAG and VALUE to the node associated with ITERATOR.
If the child node already exists and is a branch node, it is transformed into
a leaf node. If the child node already exists and is a leaf node, its value
is overwritten."
  (ptree-add-value-at-path (car iterator) tag value))

(defun ptree-iter-move-down (iterator)
  "Move ITERATOR to the first child of the associated node.
If a child node exists, ITERATOR is associated to the child node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (let ((child-node (ptree--get-first-child-node (car iterator))))
    (when child-node
        (ptree--iter-move-to-child-node iterator child-node))
    (consp child-node)))

(defun ptree-iter-move-to-tag (iterator tag)
  "Move ITERATOR to the child of the associated node with TAG.
If such child node exists, ITERATOR is associated to the child node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (let ((child-node (ptree--get-node (car iterator) tag)))
    (when child-node (ptree--iter-move-to-child-node iterator child-node))
    (consp child-node)))

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
  (let ((next-node (ptree--get-next-sibling-node (car iterator)
                                                 (cadr iterator))))
    (when next-node (ptree--iter-move-to-sibling-node iterator next-node))
    (consp next-node)))

(defun ptree-iter-move-previous (iterator)
  "Move the ITERATOR to the previous sibling node.
If the sibling node exists, ITERATOR is associated to the sibling node and 't
is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
  (let ((prev-node (ptree--get-previous-sibling-node (car iterator)
                                                     (cadr iterator))))
    (when prev-node (ptree--iter-move-to-sibling-node iterator prev-node))
    (consp prev-node)))

(defun ptree-iter-add-child-nodes (iterator &rest child-tags)
  "Add multiple branch nodes to the node associated with ITERATOR.
One node is added for each tag passed in CHILD-TAGS.
If a tag refers to an existing branch node, it is not modified.
It a tag refers to an existing leaf node, it is turned into a branch and its
associated value is lost."
  (apply #'ptree-add-child-nodes (car iterator) child-tags))

(defun ptree-iter-add-child-with-value (iterator tag value)
  "Add child node to the node associated with ITERATOR.
The child node is created with TAG and VALUE.
If a child node with the specified tag already exists, it is overwritten."
  (ptree--make-leaf (ptree--create-node (car iterator) tag) value))

(defun ptree-iter-delete-node (iterator)
  "Delete node associated with ITERATOR.
If the node is a root node, it is not deleted.
If the node is deleted, the iterator is moved to the next sibling of
the deleted node when it exists. Otherwise it is move to the parent node.
The function returns 't if the node is deleted, 'nil otherwise."
  (let ((parent (cadr iterator)))
    (if parent
        (progn
          (let ((target-node (car iterator)))
            (if (not (ptree-iter-move-next iterator))
                (ptree-iter-move-up iterator))
            (ptree--delete-exact-node parent target-node)
            target-node))
      nil)))

(defun ptree-iter-delete-child-nodes (iterator &rest child-tags)
  "Delete multiple child nodes from the node associated with ITERATOR.
One node is delted for each tag passed in CHILD-TAGS.
If a node with the specified tag does not exist, the function stops and returns
the list of tags associated to the child nodes that have not been deleted.
Otherwise it returns 'nil."
  (apply #'ptree-delete-child-nodes (car iterator) child-tags))

;; Public conversion function

(defun ptree-to-string (node)
  "Convert NODE to string."
  (let ((res "")
        (iter (ptree-iter node))
        (level 0)
        (dir 'down))
    (ptree-iter-move-down iter)
    (let ((enter-node t))
      (while (not (eq (ptree-iter-node iter) node))
        (when enter-node
          (setq res (concat res (ptree--node-to-string
                                 (ptree-iter-node iter) level))))
        (setq enter-node nil)
        (cond ((eq dir 'down)
               (if (not (ptree-iter-move-down iter))
                   (setq dir 'next)
                 (setq enter-node t)
                 (setq level (+ level 1))))
              ((eq dir 'next)
               (if (not (ptree-iter-move-next iter))
                   (setq dir 'up)
                 (setq enter-node t)
                 (setq dir 'down)))
              (t
               (when (ptree-iter-move-up iter)
                 (setq level (- level 1))
                 (setq dir 'next))))))
    res))

;; Internal functions

(defun ptree--get-path-as-list (path)
  "Get PATH as list."
  (if (listp path)
      path
    (list path)))

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

(defun ptree--get-node (node tag)
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

(defun ptree--get-first-child-node (node)
  "Return the first child node of NODE.
The function returns 'nil if such child node does not exist."
  (caddr node))

(defun ptree--get-next-sibling-node (node parent)
  "Return the next sibling of NODE with PARENT.
The function returns 'nil if such child node does not exist."
  (let* ((cns (cddr parent))
         (cn (car cns))
         (res nil))
    (while cn
      (if (eq cn node)
          (progn
            (setq res (cadr cns))
            (setq cn nil))
        (setq cns (cdr cns))
        (setq cn (car cns))))
    res))

(defun ptree--get-previous-sibling-node (node parent)
  "Return the previous sibling of NODE with PARENT.
The function returns 'nil if such child node does not exist."
  (let* ((cns (cddr parent))
         (pcns nil)
         (cn (car cns))
         (res nil))
    (while cn
      (if (eq cn node)
          (progn
            (setq res (car pcns))
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

(defun ptree--create-node (node tag)
  "Add a child branch node with TAG in NODE.
If the node already exist it is not modified.
The function returns the created node."
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
                (t (setcdr cns (cons cn (cddr cns)))
                   (setcar cns res))))))
    res))

(defun ptree--create-path (node path-list)
  "Create a path in accordance PATH-LIST starting from NODE.
The path is constituted by branch nodes.
Branch nodes already existing are not modified.
The function returns the last node of the path."
  (while path-list
    (setq node (ptree--create-node node (car path-list)))
    (setq path-list (cdr path-list)))
  node)

(defun ptree--delete-node (node tag)
  "Delete child node with TAG from NODE.
The function returns the deleted node or  'nil if such child node does
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
  (setcar iterator (cadr iterator))
  (setcdr iterator (cddr iterator)))

(defun ptree--iter-move-to-child-node (iterator child-node)
  "Move the iterator ITERATOR to the CHILD-NODE."
  (setcdr iterator (cons (car iterator) (cdr iterator)))
  (setcar iterator child-node))

(defun ptree--iter-move-to-sibling-node (iterator sibling-node)
  "Move the iterator ITERATOR to the SIBLING-NODE."
  (setcar iterator sibling-node))

(defun ptree--tag-to-string (tag)
  "Convert TAG to string."
  (if (stringp tag)
      (format "\"%s\"" tag)
    (format "%s" tag)))

(defun ptree--node-to-string (node level)
  "Convert NODE at LEVEL to string."
  (let ((tag-str (ptree--tag-to-string (ptree-get-node-tag node)))
        (indent-str (make-string (* 2 level) ?\s)))
       (if (ptree-leaf-p node)
           (format "%s%s: %s\n" indent-str tag-str (ptree-get-node-value node))
    (format "%s%s\n" indent-str tag-str))))

;; Package provision

 (provide 'ptree)

;;; ptree.el ends here
