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
  "Return t if NODE is the root of the property tree, otherwise nil."
  (null (car node)))

(defun ptree-branch-p (node)
  "Return t if NODE is a branch of the property tree, otherwise nil."
  (and (not (null (car node))) (not (null (cddr node)))))

(defun ptree-leaf-p (node)
  "Return t if NODE is a leaf of the property tree, otherwise nil."
  (null (cddr node)))

;; Public accessors

(defun ptree-get-node-tag (node)
  "Return the tag associated with the NODE."
  (car node))

(defun ptree-get-node-value (node)
  "Return the value associated with the NODE."
  (cadr node))

(defun ptree-get-child-nodes-num (node)
  "Return the number of child nodes of NODE."
  (let ((child-nodes (cddr node)))
    (if (or (not child-nodes) (not (car child-nodes)))
        0
      (length (cddr node)))))

(defun ptree-get-child-node-at-index (node index)
  "Get child node at INDEX in NODE. Return nil if index is out of range."
  (nth index (cddr node)))

(defun ptree-get-node-at-path (node path)
  "Get node at PATH in NODE. Return nil if the node does not exist."
  (let ((path-list (ptree--get-path-as-list path)))
    (while (and node path-list)
      (setq node (ptree--get-node node (car path-list)))
      (setq path-list (cdr path-list)))
    node))

(defun ptree-get-value-at-path (node path)
  "Get the value of leaf at PATH in NODE.
Return 'not-found if the node does not exist or if it is not a leaf."
  (let* ((path-list (ptree--get-path-as-list path)))
    (setq node (ptree-get-node-at-path node path-list))
    (if (and node (ptree-leaf-p node))
        (ptree-get-node-value node)
      'not-found)))

;; Public mutatorsb

(defun ptree-set-node-value (node value)
  "Set value of NODE to VALUE.
An error is returned if the node is not a leaf."
  (if (ptree-leaf-p node)
      (setcdr node (list value))
    (error "Node is not a leaf")))

(defun ptree-add-child-nodes (node &rest branches)
  "Add BRANCHES in tree NODE.
If the branch already exists, is in not modified. It the brach has the
 same tag of an existing leaf, the leaf is turned into a branch."
  (while branches
    (ptree--make-empty-branch
     (ptree--create-node node (car branches)))
    (setq branches (cdr branches))))

(defun ptree-add-node-at-path (node path)
  "Add PATH in tree NODE.
If part of the path already exist, it is not modified. If the path runs
through a leaf, it is turned into a branch to complete the specified path.
The function returns the last node of the path."
  (let* ((path-list (ptree--get-path-as-list path))
        (res (ptree--create-path node path-list)))
    (ptree--make-empty-branch res)
    res))

(defun ptree-add-value-at-path (node value path)
  "Add a leaf with VALUE in NODE at PATH the specified path.
If part of the path already exist, it is not modified. If the path runs
through a leaf, it is turned into a branch to complete the specified path.
The function returns the leaf node."
  (let* ((path-list (ptree--get-path-as-list path))
         (res (ptree--create-path node path-list)))
    (ptree--make-leaf res value)
    res))

(defun ptree-delete-child-nodes (node &rest child-nodes)
  "Delete the specified CHILD-NODES from NODE."
  (while child-nodes
    (if (not (ptree--delete-node node (car child-nodes)))
        (error "Path does not exist"))
    (setq child-nodes (cdr child-nodes))))

(defun ptree-delete-node-at-path (node path)
  "Delete node at PATH in NODE.
Return the deleted node. Throw a an error if the node does not exist"
  (let ((path-list (ptree--get-path-as-list path)))
    (while (and node (cdr path-list))
      (setq node (ptree--get-node node (car path-list)))
      (setq path-list (cdr path-list)))
    (if (not node)
        (error "Path does not exist")
      (let ((deleted-node (ptree--delete-node node (car path-list))))
        (if deleted-node
            deleted-node
          (error "Path does not exist"))))))

;; Internal functions

(defun ptree--make-empty-branch (node)
  "Transform the NODE into an empty tree."
  (setcdr node (list nil nil)))

(defun ptree--make-leaf (node value)
  "Transform the NODE into a leaf with VALUE."
  (setcdr node (list value)))

(defun ptree--get-path-as-list (path)
  "Get PATH as list."
  (if (listp path)
      path
    (list path)))

(defun ptree--compare-tags (ltag rtag)
  "Copares LTAG with RTAG."
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

(defun ptree--create-node (node tag)
  "Get or insert node with TAG in NODE."
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

(defun ptree--get-node (node tag)
  "Get node with TAG in NODE."
  (let* ((cns (cddr node))
         (cn (car cns))
         (res nil))
    (while (and cn (not res))
      (if (eq (ptree--compare-tags tag (car cn)) 'eq)
          (setq res cn)
        (setq cns (cdr cns))
        (setq cn (car cns))))
    res))

(defun ptree--create-path (node path-list)
  "Create PATH-LIST in NODE."
  (while path-list
    (setq node (ptree--create-node node (car path-list)))
    (setq path-list (cdr path-list)))
  node)

(defun ptree--delete-node (node tag)
  "Delete node with TAG in NODE."
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

;; Package provision

 (provide 'ptree)

;;; ptree.el ends here
