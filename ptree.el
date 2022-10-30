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

;; Errors definition

(define-error 'ptree-not-a-category "A property as been passed to a function \
expecting a category")

(define-error 'ptree-not-a-property "A category as been passed to a function \
expecting a property")

(define-error 'ptree-node-already-existing "Tried to create an existing node")

(define-error 'ptree-node-not-existing "Tried to modify a non existing node")

;; Property tree construction

(defun ptree-empty ()
  "Create an empty property tree."
  (list nil))

;; Node predicates

(defun ptree-category-p (node)
  "Check if NODE is a category of the property tree.
The function returns 't if NODE is a category, otherwise 'nil."
  (not (car node)))

(defun ptree-property-p (node)
  "Check if NODE is a property of the property tree.
The function returns 't if NODE is a property, otherwise 'nil."
  (not (null (car node))))

;; Node attributes

(defun ptree-property-type (node)
  "Get the property value associated with NODE.
If NODE is a category, 'ptree-not-a-property error is signaled."
  (if (ptree-property-p node)
      (car node)
    (signal 'ptree-not-a-property node)))

(defun ptree-property-value (node)
  "Get the property value associated with NODE.
If NODE is a category, 'ptree-not-a-property error is signaled."
  (if (ptree-property-p node)
      (cdr node)
    (signal 'ptree-not-a-property node)))

(defun ptree-set-property (node value &optional type)
  "Set VALUE and TYPE for property at NODE.
If NODE is a category, 'ptree-not-a-property error is signaled."
  (if (ptree-category-p node)
      (signal 'ptree-not-a-property node)
    (when type
      (setcar node type))
    (setcdr node value)))

(defun ptree-child-nodes-num (node)
  "Get the number of child nodes of NODE.
If NODE is a property, 0 is returned."
  (if (car node)
      0
    (/ (length (cdr node)) 2)))

;; Node retrieval

(defun ptree-child-node-at-index (node index)
  "Get the child node of NODE at position INDEX.
If NODE is a property or if INDEX is out of range, 'nil is returned"
  (if (car node)
      nil
    (nth (1+ (* index 2)) (cdr node))))

(defun ptree-child-node-with-tag (node tag)
  "Get child node of NODE with TAG.
If NODE is a property or if such child node does not exist, 'nil is returned."
  (ptree--child-node node tag))

;; Nodes construction

(defun ptree-add-category (node tag)
  "Add a category with TAG as child of NODE.
If NODE is a property, 'ptree-not-a-category error is signaled.
If a child node with the specified tag already exists,
'ptree-node-already-existing error is signaled.
The functions returns the created category."
  (let ((res (list nil)))
    (ptree--add-child-node node tag res nil)
    res))

(defun ptree-add-property (node tag value &optional type)
  "Add a property with TAG, VALUE and TYPE as child of NODE.
If TYPE is not provided, type is set to 'generic.
If NODE is a property, 'ptree-not-a-category error is signaled.
If a child node with the specified tag already exists,
'ptree-node-already-existing error is signaled.
The functions returns the created property."
  (unless type
    (setq type 'generic))
  (let ((res (cons type value)))
    (ptree--add-child-node node tag res nil)
    res))

;; Nodes deletion

(defun ptree-delete-child-node (node tag)
  "Delete child node of NODE with TAG.
If NODE is a property, 'ptree-not-a-category error is signaled.
If such child node does not exist, 'ptree-node-not-existing error is raised."
  (ptree--delete-child-node node tag))

;; Convenience functions

(defun ptree-node-at-path (node path)
  "Get the descendant of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If descendant node at PATH does not exist, the function returns 'nil."
  (unless (listp path)
    (setq path (list path)))
  (ptree--node-at-path node path))

(defun ptree-value-at-path (node path)
  "Get the value of descendat property of NODE at PATH.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If descendant node at PATH does not exist or if it is not a property,
'not-found is returned."
  (setq node (ptree-node-at-path node path))
  (if (or (not node) (ptree-category-p node))
      'not-found
    (ptree-property-value node)))

(defun ptree-write-categories-at-path (node path &rest tags)
  "Create categories as child nodes of NODE at PATH.
TAGS the list of tags of the categories to be created.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If a node in the path does not exist, it is created.
If a node in the path is a property, it is turned into a catagory.
If a tag in TAGS refers to an existing property, it is turned into a
category.
The function returns the last child node created."
  (unless (listp path)
    (setq path (list path)))
  (setq node (ptree--write-path node path))
  (let ((res nil))
    (while tags
      (setq res (ptree--add-child-node node (car tags) (list nil) t))
      (setq tags (cdr tags)))
    res))

(defun ptree-write-properties-at-path (node path &rest props-data)
  "Create properties as child nodes of NODE at PATH.
PROPS-DATA is a list of property data items. Each property data item can have
the format (tag . value) or (tag value . type). In the latter case, the
property value is set to 'generic.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If a node in the path does not exist, it is created.
If a node in the path is a property, it is turned into a catagory.
If a tag in PROP-DATA refers to an existing category, it is turned into a
property.
The function returns the last child node created."
  (unless (listp path)
    (setq path (list path)))
  (setq node (ptree--write-path node path))
  (let ((res nil))
    (while props-data
      (let ((prop-data (car props-data)))
        (if (consp (cdr prop-data))
            (setq res (cons (cddr prop-data) (cadr prop-data)))
          (setq res (cons 'generic (cdr prop-data))))
        (ptree--add-child-node node (car prop-data) res t)
        (setq props-data (cdr props-data))))
    res))


;; (defun ptree-set-branches-at-path (node path &rest child-tags)
;;   "Add multiple branch nodes to the descendant of NODE at PATH.
;; PATH is the list of tags that shall be followed to reach the descendant node.
;; If only one tag has to be specified, it is possible to set PATH to the value
;; of the tag instead of passing PATH as a list with a single tag.
;; If PATH is nil, the descendant of NODE is NODE itself.
;; If any node in the path does not exist, it is created as branch node.
;; If any node in the path is a leaf node, it is transformed into a branch node
;; and its value is lost.
;; A branch node is added to the descendant node for each tag passed in CHILD-TAGS.
;; If the tag refers to an existing branch node, it is not modified.
;; It the refers to an existing leaf node, it is turned into a branch and its
;; associated value is lost.
;; The function returns the node associated with the last child tag."
;;   (let ((desc (ptree--create-node-at-path node path))
;;         (last-child-node nil))
;;     (while child-tags
;;       (setq last-child-node (ptree--create-node desc (car child-tags)))
;;       (when (ptree--incomplete-p last-child-node)
;;         (ptree--make-empty-branch last-child-node))
;;       (setq child-tags (cdr child-tags)))
;;     last-child-node))

;; (defun ptree-set-leaves-at-path (node path &rest leaves-data)
;;   "Add multiple leaf nodes to the descendant of NODE at PATH.
;; PATH is the list of tags that shall be followed to reach the descendant node.
;; If only one tag has to be specified, it is possible to set PATH to the value
;; of the tag instead of passing PATH as a list with a single tag.
;; If PATH is nil, the descendant of NODE is NODE itself.
;; If any node but in the path does not exist, it is created as branch node.
;; If any node in the path is a leaf node, it is transformed into a branch node
;; and its value is lost.
;; A leaf node is added to the descendant node for each item passed in LEAVES-DATA.
;; LEAVES-DATA is a list of pairs containing the leaf node tag and value.
;; If the node tag refers to an existing branch node, it is transformed to a leaf
;; node before setting its value.
;; It the node tag refers to an existing leaf node, it is value is overwritten.
;; The function returns the node associated with the last leaf node tag."
;;   (let ((desc (ptree--create-node-at-path node path))
;;         (last-child-node nil))
;;     (while leaves-data
;;       (setq last-child-node (ptree--create-node desc (caar leaves-data)))
;;       (ptree--make-leaf last-child-node (cadar leaves-data))
;;       (setq leaves-data (cdr leaves-data)))
;;     last-child-node))

;; Public node deletion functions

;; (defun ptree-delete-nodes-at-path (node path &rest child-tags)
;;   "Delete multiple nodes that are the descendant of NODE at PATH.
;; PATH is the list of tags that shall be followed to reach the descendant node.
;; If only one tag has to be specified, it is possible to set PATH to the value
;; of the tag instead of passing PATH as a list with a single tag.
;; If PATH is nil, the descendant of NODE is NODE itself.
;; A node is deleted from the descendant node for each tag passed in CHILD-TAGS.
;; If a node cannot be deleted (cause it does not exist) the function stops
;; and returns the list of tags associated with node that have not been deleted,
;; otherwise the function returns 'nil."
;;   (let ((desc (ptree--node-at-path node path)))
;;     (if desc
;;         (let ((in-progress t))
;;           (while (and in-progress child-tags)
;;             (if (not (ptree--delete-node desc (car child-tags)))
;;                 (setq in-progress nil)
;;               (setq child-tags (cdr child-tags))))
;;           child-tags))
;;       child-tags))

;; Public interator functions

;; (defun ptree-iter (node)
;;   "Create an iterator associated with NODE.
;; NODE represent the initial node for the iterator.
;; The iterator can only move down to the child nodes of the initial node.
;; The iterator cannot move to the parent or sibling nodes of the initial node."
;;   (list node nil))

;; (defun ptree-iter-node (iterator)
;;   "Get the node associated with ITERATOR."
;;   (car iterator))

;; (defun ptree-iter-move-to-tag (iterator tag)
;;   "Move ITERATOR to the child of the associated node with TAG.
;; If such child node exists, ITERATOR is associated to the child node and 't
;; is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
;;   (let ((child-list (ptree--get-child-list (car iterator) tag)))
;;     (when child-list (ptree--iter-move-to-child-node iterator child-list))
;;     (consp child-list)))

;; (defun ptree-iter-move-down (iterator)
;;   "Move ITERATOR to the first child of the associated node.
;; If a child node exists, ITERATOR is associated to the child node and 't
;; is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
;;   (let ((child-list (cddar iterator)))
;;     (if (and child-list (car child-list))
;;         (progn
;;           (ptree--iter-move-to-child-node iterator child-list)
;;           t)
;;       nil)))

;; (defun ptree-iter-move-up (iterator)
;;   "Move the ITERATOR to the parent of the associated node.
;; If the parent node exists, ITERATOR is associated to the parent node and 't
;; is returned. Otherwise ITER is unchanged and 'nil is returned."
;;   (let ((parent-node (cadr iterator)))
;;     (when parent-node (ptree--iter-move-to-parent-node iterator))
;;     (consp parent-node)))

;; (defun ptree-iter-move-next (iterator)
;;   "Move the ITERATOR to the next sibling node.
;; If the sibling node exists, ITERATOR is associated to the sibling node and 't
;; is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
;;   (if (cddr iterator)
;;       (progn
;;         (ptree--iter-move-to-next-node iterator)
;;         t)
;;     nil))

;; (defun ptree-iter-move-previous (iterator)
;;   "Move the ITERATOR to the previous sibling node.
;; If the sibling node exists, ITERATOR is associated to the sibling node and 't
;; is returned. Otherwise ITERATOR is unchanged and 'nil is returned."
;;   (let ((prev-list (ptree--get-previous-sibling-list (car iterator)
;;                                                      (caadr iterator))))
;;     (when prev-list (ptree--iter-move-to-sibling-node iterator prev-list))
;;     (consp prev-list)))

;; (defun ptree-iter-add-branch-and-move (iterator tag)
;;   "Add child branch node to the node associated with ITERATOR and move to it.
;; The child node is created with TAG.
;; If the branch node exists, it is not modified.
;; If the node exists and it is a leaf node, it is transformed into a
;; branch node."
;;   (ptree-set-branches-at-path (ptree-iter-node iterator) nil tag)
;;   (ptree-iter-move-to-tag iterator tag))

;; (defun ptree-iter-delete-node (iterator)
;;   "Delete node associated with ITERATOR.
;; If the node is a root node, it is not deleted.
;; If the node is deleted, the iterator is moved to the next sibling of
;; the deleted node when it exists. Otherwise it is move to the parent node.
;; The function returns 't if the node is deleted, 'nil otherwise."
;;   (let ((parent (caadr iterator)))
;;     (if parent
;;         (progn
;;           (let ((target-node (car iterator)))
;;             (if (not (ptree-iter-move-next iterator))
;;                 (ptree-iter-move-up iterator))
;;             (ptree--delete-exact-node parent target-node)
;;             target-node))
;;       nil)))

;; Public conversion function

;; (defun ptree-to-string (node)
;;   "Convert NODE to string."
;;   (let ((res "")
;;         (iter (ptree-iter node))
;;         (level 0))
;;     (ptree-iter-move-down iter)
;;     (while (not (eq (ptree-iter-node iter) node))
;;       (setq res (concat res (ptree--node-to-string
;;                              (ptree-iter-node iter) level)))
;;       (if (ptree-iter-move-down iter)
;;           (setq level (+ level 1))
;;         (unless (ptree-iter-move-next iter)
;;           (let ((in-progress t))
;;             (while in-progress
;;               (if (ptree-iter-move-up iter)
;;                   (setq level (- level 1))
;;                 (setq in-progress nil))
;;               (when (ptree-iter-move-next iter)
;;                 (setq in-progress nil)))))))
;;     res))

;; (defun ptree-to-string-2 (node)
;;   "Convert NODE to string."
;;   (let ((res "")
;;         (level 0)
;;         (child-stack (list (list 0 (ptree-child-nodes-num node)))))
;;     (while child-stack
;;       (let ((curr-child (caar child-stack))
;;             (child-num (cadar child-stack))
;;             (node (ptree-child-nodes-num (caar child-stack))))
;;         (setq res (concat res (ptree--node-to-string node)))
;;         (if (ptree-leaf-p node)
;;             (setq child-stack (cons (list (+ curr-child 1) child-num)
;;                                     (cdr child-stack)))
;;           (setq level (+ level 1))
;;           (setq child-stack (cons (list 0 (ptree-child-nodes-num node)) child-stack)))
;;         (while (= (caar child-stack) (cadar child-stack))
;;           (setq level (- level 1))
;;           (setq child-stack (cdr child-stack)))))))





;; Internal functions

(defun ptree--compare-tags (ltag rtag)
  "Copares LTAG with RTAG.
Integers precede symbols, that precede strings.
Integers and strings are compared using standard comparison functions.
Symbols are converted to strings and then compared using string comparison
functions.
The comparison returns 'lt if LTAG is lower than RTAG, 'eq if LTAG is
equal to RTAG, 'gt if LTAG is greater than RTAG."
  (cond ((integerp ltag)
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

(defun ptree--child-node (node tag)
  "Get child node of NODE with TAG.
If NODE is a property or if such child node does not exist, 'nil is returned."
    (if (car node)
        nil
      (let* ((res nil)
             (cns (cdr node))
             (cn (car cns)))
        (while (and cn (not res))
          (let ((cmp (ptree--compare-tags tag cn)))
            (cond ((eq cmp 'eq) (setq res (cadr cns)))
                  ((eq cmp 'ge) (setq cn nil))
                  (t
                   (setq cns (cddr cns))
                   (setq cn (car cns))))))
        res)))

;; (defun ptree--get-child-list (node tag)
;;   "Get the list of child nodes of NODE starting with node with TAG.
;; The function returns 'nil if such child node does not exist."
;;   (let* ((cns (cddr node))
;;          (cn (car cns))
;;          (res nil))
;;     (while (and cn (not res))
;;       (if (eq (ptree--compare-tags tag (car cn)) 'eq)
;;           (setq res cns)
;;         (setq cns (cdr cns))
;;         (setq cn (car cns))))
;;     res))

(defun ptree--node-at-path (node path)
  "Get the descendant of NODE at PATH.
If a node in the PATH is a property, 'ptree-not-a-category error is signaled.
If the descendant node does not exist, 'nil is returned."
  (while (and node path)
    (let ((child (ptree--child-node node (car path))))
      (setq path (cdr path))
      (setq node child)))
  node)

;; (defun ptree--add-child-node (node tag child)
;;   "Add the CHILD node with TAG to NODE.
;; If NODE is a property, 'ptree-not-a-category error is signaled.
;; If the node already exists, 'ptree-node-already-existing error is signaled."
;;   (when (ptree-property-p node)
;;     (signal 'ptree-not-a-category node))
;;   (if (not (cdr node))
;;       (setcdr node (list tag child))
;;     (let* ((cns (cdr node))
;;            (prev-cns nil)
;;            (in-progress t))
;;       (while in-progress
;;         (let ((cmp (ptree--compare-tags tag (car cns))))
;;           (cond ((eq cmp 'gt)
;;                  (if (cddr cns)
;;                      (progn
;;                        (setq prev-cns cns)
;;                        (setq cns (cddr cns)))
;;                    (setcdr (cdr cns) (list tag child))
;;                    (setq in-progress nil)))
;;                 ((eq cmp 'eq) (signal 'ptree-node-already-existing
;;                                       (cons tag node)))
;;                 (t (if prev-cns
;;                        (setcdr (cdr prev-cns) (cons tag (cons child cns)))
;;                      (setcdr node (cons tag (cons child (cdr node)))))
;;                    (setq in-progress nil))))))))

(defun ptree--add-child-node (node tag child overwrite)
  "Add the CHILD node with TAG to NODE.
If NODE is a property and OVERWRITE is not set, 'ptree-not-a-category error is
signaled. If OVERWRITE is set the property is turned into a category.
If the node already exists and OVERWRITE is not set,
'ptree-node-already-existing error is signaled. If OVERWRITE is set and the
child node is overwritten if both the CHILD node and existing node are
categories. In such a case, the existing node is not modified.
The function returns the child node of NODE with TAG."
  (when (ptree-property-p node)
    (if overwrite
        (progn
          (setcar node nil)
          (setcdr node (list tag child)))
      (signal 'ptree-not-a-category node)))
  (if (not (cdr node))
      (setcdr node (list tag child))
    (let* ((cns (cdr node))
           (prev-cns nil)
           (in-progress t))
      (while in-progress
        (let ((cmp (ptree--compare-tags tag (car cns))))
          (cond ((eq cmp 'gt)
                 (if (cddr cns)
                     (progn
                       (setq prev-cns cns)
                       (setq cns (cddr cns)))
                   (setcdr (cdr cns) (list tag child))
                   (setq in-progress nil)))
                ((eq cmp 'eq)
                 (if overwrite
                     (progn
                       (if (and (ptree-category-p child)
                                (ptree-category-p (cadr cns)))
                           (setq child (cadr cns))
                         (setcdr cns (cons child (cddr cns))))
                       (setq in-progress nil))
                   (signal 'ptree-node-already-existing (cons tag node))))
                (t (if prev-cns
                       (setcdr (cdr prev-cns) (cons tag (cons child cns)))
                     (setcdr node (cons tag (cons child (cdr node)))))
                   (setq in-progress nil)))))))
  child)

(defun ptree--delete-child-node (node tag)
  "Delete child node of NODE with TAG.
If NODE is a property, 'ptree-not-a-category error is signaled.
If such child node does not exist, 'ptree-node-not-existing error is raised."
  (if (ptree-property-p node)
      (signal 'ptree-not-a-category node)
    (if (not (cdr node))
        (signal 'ptree-node-not-existing (cons tag node))
      (let* ((cns (cdr node))
             (prev-cns nil)
             (in-progress t))
        (while in-progress
          (let ((cmp (ptree--compare-tags tag (car cns))))
            (if (eq cmp 'eq)
                (let ((actual-cns (if prev-cns (cdr prev-cns) node)))
                    (setcdr actual-cns (cdddr actual-cns))
                    (setq in-progress nil))
                (if (or (eq cmp 'lt) (not (cddr cns)))
                    (signal 'ptree-node-not-existing (cons tag node))
                  (setq prev-cns cns)
                  (setq cns (cddr cns))))))))))

(defun ptree--write-path (node path)
  "Create a PATH up to a descentant node starting from NODE.
PATH is the list of tags that shall be followed to reach the descendant node.
If only one tag has to be specified, it is possible to set PATH to the value
of the tag instead of passing PATH as a list with a single tag.
If PATH is nil, the descendant of NODE is NODE itself.
If a node in the path does not exist, it is created.
If a node in the path is a property, it is turned into a catagory.
The function return the descendant node."
  (while path
    (setq node (ptree--add-child-node node (car path) (list nil) t))
    (setq path (cdr path)))
  node)

;; (defun ptree--get-previous-sibling-list (node parent)
;;   "Get the list of siblings starting from the node before NODE with PARENT.
;; The function returns 'nil if the previous node does not exist."
;;   (let* ((cns (cddr parent))
;;          (pcns nil)
;;          (cn (car cns))
;;          (res nil))
;;     (while cn
;;       (if (eq cn node)
;;           (progn
;;             (setq res pcns)
;;             (setq cn nil))
;;         (setq pcns cns)
;;         (setq cns (cdr cns))
;;         (setq cn (car cns))))
;;     res))

;; (defun ptree--make-empty-branch (node)
;;   "Transform NODE into an empty branch node."
;;   (setcdr node (list nil nil)))

;; (defun ptree--make-leaf (node value)
;;   "Transform NODE into a leaf node with VALUE."
;;   (setcdr node (list value)))

;; (defun ptree--incomplete-p (node)
;;   "Return 't if NODE is incomplete, otherwise 'nil."
;;   (not (cdr node)))

;; (defun ptree--create-node (node tag)
;;   "Add a child incomplete node with TAG in NODE.
;; If the node already exists, the function returns it instead of creating
;; a new incomplete node"
;;   (let* ((cns (cddr node))
;;          (cn (car cns))
;;          (res nil))
;;     (while (not res)
;;       (let ((cmp (ptree--compare-tags tag (car cn))))
;;         (if (and (eq cmp 'gt) (cdr cns))
;;             (progn (setq cns (cdr cns))
;;                    (setq cn (car cns)))
;;           (setq res (list tag))
;;           (cond ((eq cmp 'gt) (setcdr cns (list res)))
;;                 ((eq cmp 'empty) (setcdr node (list nil res)))
;;                 ((eq cmp 'eq) (setq res cn))
;;                 (t (setcdr cns (cons cn (cdr cns)))
;;                    (setcar cns res))))))
;;     res))

;; (defun ptree--create-node-at-path (node path)
;;   "Add an incomplete descendent node of NODE at PATH.
;; If nodes in the path do not exist, they are created.
;; If the descendent node already exists, the function returns it instead of
;; creating a new incomplete node.
;; The function returns the descendent node."
;;   (let ((path-list (if (listp path)
;;                        path
;;                      (list path))))
;;     (while (and node path-list)
;;       (let ((child (ptree--create-node node (car path-list))))
;;         (when (ptree--incomplete-p child)
;;           (ptree--make-empty-branch child))
;;         (setq path-list (cdr path-list))
;;         (setq node child))))
;;     node)

;; (defun ptree--delete-node (node tag)
;;   "Delete child node with TAG from NODE.
;; The function returns the deleted node or 'nil if such child node does
;; not exist."
;;   (let* ((cns (cddr node))
;;          (pcns nil)
;;          (cn (car cns))
;;          (res nil))
;;     (while cn
;;       (let ((cmp (ptree--compare-tags tag (car cn))))
;;         (cond ((eq cmp 'eq) (progn (setq res cn)
;;                                    (setq cn nil)
;;                                    (if (cdr cns)
;;                                        (progn (setcar cns (cadr cns))
;;                                               (setcdr cns (cddr cns)))
;;                                      (if (eq pcns nil)
;;                                          (setcdr node (list nil nil))
;;                                        (setcdr pcns nil)))))
;;               ((eq cmp 'gt) (progn (setq pcns cns)
;;                                    (setq cns (cdr cns))
;;                                    (setq cn (car cns))))
;;               (t (setq cn nil)))))
;;     res))

;; (defun ptree--delete-exact-node (node target-node)
;;   "Delete TARGET-NODE from NODE.
;; The function returns the deleted node."
;;   (let* ((cns (cddr node))
;;          (pcns nil)
;;          (cn (car cns))
;;          (res nil))
;;     (while cn
;;       (if (eq cn target-node)
;;         (progn (setq res cn)
;;                (setq cn nil)
;;                (if (cdr cns)
;;                    (progn (setcar cns (cadr cns))
;;                           (setcdr cns (cddr cns)))
;;                  (if (eq pcns nil)
;;                      (setcdr node (list nil nil))
;;                    (setcdr pcns nil))))
;;       (setq pcns cns)
;;       (setq cns (cdr cns))
;;       (setq cn (car cns)))
;;     res)))

;; (defun ptree--iter-move-to-parent-node (iterator)
;;   "Move the iterator ITERATOR to the parent node."
;;   (setcar iterator (caadr iterator))
;;   (setcdr iterator (cdadr iterator)))

;; (defun ptree--iter-move-to-child-node (iterator child-list)
;;   "Move the iterator ITERATOR to the first node of CHILD-LIST.
;; The rest of the elements of CHILD-LIST refers to the successive nodes."
;;   (setcdr iterator
;;           (cons (cons (car iterator) (cdr iterator)) (cdr child-list)))
;;   (setcar iterator (car child-list)))

;; (defun ptree--iter-move-to-next-node (iterator)
;;   "Move the iterator ITERATOR to the next sibling node."
;;   (setcar iterator (caddr iterator))
;;   (setcdr iterator (cons (cadr iterator) (cdddr iterator))))

;; (defun ptree--iter-move-to-sibling-node (iterator sibling-list)
;;   "Move the iterator ITERATOR to the first node of SIBLING-LIST.
;; The rest of the elements of CHILD-LIST refers to the successive nodes."
;;   (setcar iterator (car sibling-list))
;;   (setcdr iterator (cons
;;                     (cadr iterator)
;;                     (cdr sibling-list))))

;; (defun ptree--tag-to-string (tag)
;;   "Convert TAG to string."
;;   (if (stringp tag)
;;       (format "\"%s\"" tag)
;;     (format "%s" tag)))

;; (defun ptree--node-to-string (node level)
;;   "Convert NODE at LEVEL  to string."
;;   (let ((tag-str (ptree--tag-to-string (ptree-tag node)))
;;         (prefix (concat (make-string (* 4 level) ?\s))))

;;        (if (ptree-leaf-p node)
;;            (format "%s%s: %s\n" prefix tag-str (ptree-value node))
;;     (format "%s%s\n" prefix tag-str))))

;; Package provision

 (provide 'ptree)

;;; ptree.el ends here
