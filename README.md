# ptree.el

Property tree data structure for Emacs

**Table of Contents**

- [Quick start](#quick-start)
- [Concepts](#concepts)
- [Functions](#functions)
- [Contacts](#contacts)

## Quick start

This section provides some basic guidance for using Property Tree data 
structure.

### Creating a propery tree

A property tree is created using the expression `(ptree-create)`.
Such expression create an empty tree with no property.

### Populating the property tree

Several different approaches can be followed to populate a property tree.

Let's assume that we want to create the following property tree:

```
user: alpha
windows
  0
    name: xterm
    pos
      x: 10
      y: 50
  1
    name: emacs
    pos
      x: 100
      y: 200
  current: 0
```

In the tree above, leaf nodes are represented by their tag followed by a colon and their value.
The rest of the nodes are branch nodes.

The most direct approach to populate a property tree is to call the
function `ptree-set-value-at-path` specifying the value of the property
and its path in the property tree. 

Example:

```
(setq pt (ptree-create))
(ptree-set-value-at-path pt 'user "alpha")
(ptree-set-value-at-path pt '(windows 0 name) "xterm")
(ptree-set-value-at-path pt '(windows 0 pos x) 10)
(ptree-set-value-at-path pt '(windows 0 pos y) 50)
(ptree-set-value-at-path pt '(windows 1 name) "emacs")
(ptree-set-value-at-path pt '(windows 1 pos x) 100)
(ptree-set-value-at-path pt '(windows 1 pos y) 200)
(ptree-set-value-at-path pt '(windows current) 0)
(message (ptree-to-string pt))
```

This approach is not very efficient since every time a value is set, the property tree is scanned
from the root node up to the leaf that shall contain the value.

Another approch is to use also the function `ptree-set-leaves-at-path` 
that constructs leaf node as children of a node at the specified path.

Example:

```
(setq pt (ptree-create))
(ptree-set-value-at-path pt 'user "alpha")
(ptree-set-value-at-path pt '(windows 0 name) "xterm")
(ptree-set-leaves-at-path pt '(windows 0 pos) '(x 10) '(y 50))
(ptree-set-value-at-path pt '(windows 1 name) "emacs")
(ptree-set-leaves-at-path pt '(windows 1 pos) '(x 100) '(y 200))
(ptree-set-value-at-path pt '(windows current) 0)
(message (ptree-to-string pt))
```

This approach is somewhat more efficient since the function `ptree-set-leaves-at-path`
traverse the tree only once, independently from how many leaf nodes must be created.
Yet also in this case, the tree is scanned multiple times starting from the root node.

Another approach is to use also the function `ptree-set-branches-at-path`
that constructs branch nodes as children of a node at the specified path
and returns the last child node.

Example:

```
(setq pt (ptree-create))
(ptree-set-value-at-path pt 'user "alpha")
(let ((n1 (ptree-set-branches-at-path pt nil 'windows)))
    (let ((n2 (ptree-set-branches-at-path n1 nil 0)))
        (ptree-set-value-at-path n2 'name "xterm")
        (ptree-set-leaves-at-path n2 'pos '(x 10) '(y 50)))
    (let ((n2 (ptree-set-branches-at-path n1 nil 1)))
        (ptree-set-value-at-path n2 'name "emacs")
        (ptree-set-leaves-at-path n2 'pos '(x 100) '(y 200)))
    (ptree-set-value-at-path n1 'current 0))
(message (ptree-to-string pt))
```

This is the most efficient approach since it minimizes the number of tree transversal.

The last approach is to use property tree iterators.

Example:
```
(setq pt (ptree-create))
(let ((iter (ptree-iter pt)))
    (ptree-set-value-at-path (ptree-iter-node iter) 'user "alpha")
    (ptree-iter-add-branch-and-move iter 'windows)
    (ptree-iter-add-branch-and-move iter 0)
    (ptree-set-value-at-path (ptree-iter-node iter) 'name "xterm")
    (ptree-iter-add-branch-and-move iter 'pos)
    (ptree-set-value-at-path (ptree-iter-node iter) 'x 10)
    (ptree-set-value-at-path (ptree-iter-node iter) 'y 50)
    (ptree-iter-move-up iter)
    (ptree-iter-move-up iter)
    (ptree-iter-add-branch-and-move iter 1)
    (ptree-set-value-at-path (ptree-iter-node iter) 'name "emacs")
    (ptree-iter-add-branch-and-move iter 'pos)
    (ptree-set-value-at-path (ptree-iter-node iter) 'x 100)
    (ptree-set-value-at-path (ptree-iter-node iter) 'y 200)
    (ptree-iter-move-up iter)
    (ptree-iter-move-up iter)
    (ptree-set-value-at-path (ptree-iter-node iter) 'current 0))
(message (ptree-to-string pt))
```
    
This approach is reasonably efficient (it is better than the first two illustrated)
and can be useful when the tree must be populated at different times.

### Reading tree properties

Properties can be read with the function `ptree-value-at-path`. If a leaf node exists
at the specified path, its value is returned. Otherwise the symbol `'not-found` is returned.

Example:

```
(setq pt (ptree-create))
(ptree-set-leaves-at-path pt '(a b c) '(0 "test") '(1 nil) '(2 3))
(message "%s" (ptree-value-at-path pt '(a b c 0))) ;; prints "test"
(message "%s" (ptree-value-at-path pt '(a b c 1))) ;; prints nil
(message "%s" (ptree-value-at-path pt '(a b c 2))) ;; prints 3
(message "%s" (ptree-value-at-path pt '(a c b 2))) ;; prints not-found
```

This approach, again, is not very efficient since the path `(a b c)` is traversed
multiple time in order to get the required values.

A more efficient approach leverage the `ptree-node-at-path` function that allows
to get a descendant of the node at the specified path.

Example:

```
(setq pt (ptree-create))
(ptree-set-leaves-at-path pt '(a b c) '(0 "test") '(1 nil) '(2 3))
(let ((node (ptree-node-at-path pt '(a b c))))
    (message "%s" (ptree-value-at-path node 0)) ;; prints "test"
    (message "%s" (ptree-value-at-path node 1)) ;; prints nil
    (message "%s" (ptree-value-at-path node 2))) ;; prints 3
```

Finally, iterators can also be used to retrieve values.

Example:

```
(setq pt (ptree-create))
(ptree-set-leaves-at-path pt '(a b c) '(0 "test") '(1 nil) '(2 3))
(let ((iter (ptree-iter pt)))
    (ptree-iter-move-down iter)
    (ptree-iter-move-down iter)
    (ptree-iter-move-down iter)
    (message "%s" (ptree-value-at-path (ptree-iter-node iter) 0)) ;; prints "test"
    (message "%s" (ptree-value-at-path (ptree-iter-node iter) 1)) ;; prints nil
    (message "%s" (ptree-value-at-path (ptree-iter-node iter) 2))) ;; prints 3
```

### Deleting tree nodes

Nodes can be deleted using the `ptree-delete-nodes-at-path` function.

Example:

```
(setq pt (ptree-create))
(ptree-set-leaves-at-path pt '(a b c) '(0 "test") '(1 nil) '(2 3))
(ptree-set-leaves-at-path pt 'a '("one" 1) `("two" 2))
(ptree-delete-nodes-at-path pt '(a b c) 0 2) ;; deletes nodes (0 "test") and '(2 3)
(ptree-delete-nodes-at-path pt 'a "two") ;; deletes node ("two" 2)
(ptree-delete-nodes-at-path pt nil 'a) ;; deletes all remainin nodes
```

Iterator can also be used for deleting nodes:

Exaple:

```
(setq pt (ptree-create))
(ptree-set-leaves-at-path pt '(a b c) '(0 "test") '(1 nil) '(2 3))
(let ((iter (ptree-iter pt)))
    (ptree-iter-move-down iter)
    (ptree-iter-move-down iter)
    (ptree-iter-move-down iter)
    (ptree-iter-move-to-tag iter '1)
    (ptree-iter-delete-node iter)) ;; node (1 nil) is deleted and iter moves to node (2 3)
(message (ptree-to-string pt))
```

## Concepts

## Functions

## Contacts

Please report bugs, requests and suggestions to Alpha Catharsis 
<alpha.catharsis@gmail.com> or on Github.
