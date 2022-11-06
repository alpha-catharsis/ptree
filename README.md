# ptree.el

Property tree data structure for Emacs

**Table of Contents**

- [Quick start](#quick-start)
- [Concepts](#concepts)
- [Functions](#functions)
- [Contacts](#contacts)

## Quick start

This section provides some basic guidance for using property tree data 
structure.

A property tree is designed to store properties organizing them in categories
that can be nested one into another.

### Creating a propery tree

A property tree is created using the expression `(ptree-empty)`.
Such expression create an empty tree with no properties.

### Populating the property tree

Several different approaches can be adopted to populate a property tree.

Let's assume that we want to create the following property tree:

```
current-users: 12
desktop
    background-color: black
processes
    by-id
        0: startx
        1: dbus-launch
        2: X
windows
    "emacs"
        height: 800
        pos-x: 600
        pos-y: 0
        width: 1000
```

In the tree above, properties are represented by their tag followed by a colon
and their value. The rest of the nodes are categories and are represented just
by their tag.

The most direct approach to populate a property tree is to call the
function `ptree-write-properties-at-path` as follow:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))
(message (ptree-to-string pt))
```

Another approach is to use the `ptree-add-property` and `ptree-add-category`
functions:

```
(setq pt (ptree-empty))
(ptree-add-property pt 'current-users 12)
(let ((cn1 (ptree-add-category pt 'desktop)))
    (ptree-add-property cn1 'background-color "black"))
(let* ((cn1 (ptree-add-category pt 'processes))
       (cn2 (ptree-add-category cn1 'by-id)))
    (ptree-add-property cn2 0 "startx")
    (ptree-add-property cn2 1 "dbus-launch")
    (ptree-add-property cn2 2 "X"))
(let* ((cn1 (ptree-add-category pt 'windows))
       (cn2 (ptree-add-category cn1 "emacs")))
    (ptree-add-property cn2 'pos-x 600)
    (ptree-add-property cn2 'pos-y 0)
    (ptree-add-property cn2 'height 800)
    (ptree-add-property cn2 'width 1000))
(message (ptree-to-string pt))
```

Another approach is to use an iterator:

```
(setq pt (ptree-empty))
(setq iter (ptree-iter pt))
(ptree-iter-add-property iter 'current-users 12)
(ptree-iter-add-category-and-move iter 'desktop)
(ptree-iter-add-property iter 'background-color "black")
(ptree-iter-up iter)
(ptree-iter-add-category-and-move iter 'processes)
(ptree-iter-add-category-and-move iter 'by-id)
(ptree-iter-add-property iter 0 "startx")
(ptree-iter-add-property iter 1 "dbus-launch")
(ptree-iter-add-property iter 2 "X")
(ptree-iter-up iter 2)
(ptree-iter-add-category-and-move iter 'windows)
(ptree-iter-add-category-and-move iter 'emacs)
(ptree-iter-add-property iter 'pos-x 600)
(ptree-iter-add-property iter 'pos-y 0)
(ptree-iter-add-property iter 'height 800)
(ptree-iter-add-property iter 'width 1000)
(message (ptree-to-string pt))
```

### Reading tree properties

Properties can be read with the function `ptree-value-at-path`. If property
exists at the specified path, its value is returned. Otherwise the symbol
`'not-found` is returned.

Taking into account the property tree constructed in the previous section:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(message "%s" (ptree-value-at-path pt 'current-users)) ;; prints 12
(message "%s" (ptree-value-at-path pt '(desktop 
                                        background-color))) ;; prints "black"
(message "%s" (ptree-value-at-path pt '(windows "emacs" pos-x))) ;; prints 600
(message "%s" (ptree-value-at-path pt '(processes 
                                        by-name))) ;; prints 'not-found
```

Another approach is to use the `ptree-child-node-with-tag` and
`ptree-property-value` functions:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(let ((cn1 (ptree-child-node-with-tag pt 'current-users)))
    (message "%s" (ptree-property-value cn1))) ;; prints 12
(let* ((cn1 (ptree-child-node-with-tag pt 'desktop))
       (cn2 (ptree-child-node-with-tag cn1 'background-color)))
    (message "%s" (ptree-property-value cn2))) ;; prints "black"
(let* ((cn1 (ptree-child-node-with-tag pt 'windows))
       (cn2 (ptree-child-node-with-tag cn1 "emacs"))
       (cn3 (ptree-child-node-with-tag cn2 'pos-x)))
    (message "%s" (ptree-property-value cn3))) ;; prints 600
```

Another approach is to use an iterator:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(setq iter (ptree-iter pt))
(ptree-iter-move-with-tag iter 'current-users)
(message "%s" (ptree-iter-value iter)) ;; prints 12
(ptree-iter-up iter)
(ptree-iter-move-with-tag iter 'desktop)
(ptree-iter-move-with-tag iter 'background-color)
(message "%s" (ptree-iter-value iter)) ;; prints "black"
(ptree-iter-up iter 2)
(ptree-iter-move-with-tag iter 'windows)
(ptree-iter-move-with-tag iter "emacs")
(ptree-iter-move-with-tag iter 'pos-x)
(message "%s" (ptree-iter-value iter)) ;; prints 600
```

### Deleting tree nodes

Nodes can be deleted locating the parent node used the function
`ptree-node-at-path` and then deleting one of his child node using the function
`ptree-delete-child-node`:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(ptree-delete-child-node pt 'current-users) ;; deletes 'current-user property
(ptree-delete-child-node (ptree-node-at-path pt '(windows "emacs")) 'pos-x)
;; deletes pos-x property under path '(windows "emacs")
(ptree-delete-child-node pt 'desktop) ;; deletes 'desktop category
(message (ptree-to-string pt))
```

Iterators can also be utilized to delete nodes:


```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(setq iter (ptree-iter pt))
(ptree-iter-move-with-tag iter 'current-users)
(ptree-iter-delete-node iter)
(ptree-iter-move-with-tag iter 'windows)
(ptree-iter-move-with-tag iter "emacs")
(ptree-iter-move-with-tag iter 'pos-x)
(ptree-iter-delete-node iter)
(ptree-iter-up iter 2)
(ptree-iter-move-with-tag iter 'desktop)
(ptree-iter-delete-node iter)
(message (ptree-to-string pt))
```

### Updating properties

The simplest way to update a property is to overwrite it using the function
`ptree-write-properties-at-path`:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(ptree-write-properties-at-path pt 'desktop '(background-color . "white"))
;; now property 'background-color at path 'desktop has value "white"
(message (ptree-to-string pt))
```

Another approach is to locate the target property using the function
`ptree-node-at-path` and then updating its value using the function
`ptree-set-property`:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(ptree-set-property (ptree-node-at-path pt '(desktop background-color))
                    "white")
;; now property 'background-color at path 'desktop has value "white"
(message (ptree-to-string pt))
```

Finally, properties can also be updated using iterators:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))

(setq iter (ptree-iter pt))
(ptree-iter-move-with-tag iter 'desktop)
(ptree-iter-move-with-tag iter 'background-color)
(ptree-iter-set-property iter "white")
(message (ptree-to-string pt))
```

## Concepts

This section reports a formal description of the concepts associated with 
property trees.

### Basics

A property tree is a tree data structure in which non-leaf nodes represent
categories and leaf nodes represent properties.

A category is characterized just by a tag.

A property is characterized by a tag, a value and a type. The property type
can be optionally omitted. In such cases it is set to `'generic`.

Properties and categories tags can be of one of the following types:

* Integer
* Symbol
* String

### Ordering

Child nodes of a given category are ordered based on their tags according to
the following criteria:

1. Integers come before symbols and strings. Among integers, the smallest ones
   come first.
2. Symbols come before strings but after integers. Among symbols, the smallest
   ones according to lexographical order come first.
3. Strings come after integers and symbols. Among strings, the smallest ones
   according to lexographical order come first.

Tag ordering is relevant for two reason:

* It determines the order in which the nodes are stored. This affects the 
  result of functions such as `ptree-child-node-at-index`, `ptree-iter-next` 
  and `ptree-iter-previous`.
* It affects the performance when populating the tree. 

With regard to the last point, it is more efficient to insert child nodes in
reverse order since it minimizes the number of node traversed.
For example, the following code:
```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(width . 1000)
                                '(pos-y . 0)
                                '(pos-x . 600)
                                '(height . 800))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(2 . "X")
                                '(1 . "dbus-launch")
                                '(0 . "startx"))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt nil '(current-users . 12))
```

is faster than:

```
(setq pt (ptree-empty))
(ptree-write-properties-at-path pt nil '(current-users . 12))
(ptree-write-properties-at-path pt 'desktop '(background-color . "black"))
(ptree-write-properties-at-path pt '(processes by-id)
                                '(0 . "startx")
                                '(1 . "dbus-launch")
                                '(2 . "X"))
(ptree-write-properties-at-path pt '(windows "emacs")
                                '(pos-x . 600)
                                '(pos-y . 0)
                                '(height . 800)
                                '(width . 1000))
```

### Paths

A path is a list of tags used to identify a descendant node from a starting
node. The tags in the list indicate the child nodes tag that have to be 
followed from the starting node up to the descendant node.
If the path is nil, the descendant node is the starting node itself.
If the path contains a single tag, it refers to a direct child node of the
starting node. For convenience, path containing a single tag can be indicated 
with the tag itself instead of using a list of a single tag. For example the
path `windows` is equivalent to the path `(windows)`.

### Iterators

Iterators allow to navigate the property tree structure keeping track of
a specific position inside the tree. They are particularly useful when
the property tree structure is not known beforehand.
Iterators are created using the function `ptree-iter` on an initial node. 
It is worth noting that the iterators cannot move to the parent or siblings
node of the initial node, but only to its descendant. Therefore, if an iterator
needs to have access to the whole property tree, it has to be created on the 
tree root node.

**WARINING**: when an iterator is used, modify the property tree only using
the iterator. Modifications through any other mean will result in undefined
behaviors.

### Errors

The following errors might be signaled when working on property trees:

* `ptree-not-a-category` is signaled with a function expects a category but
  a property is passed to it
* `ptree-not-a-property` is signaled with a function expects a property but
  a category is passed to it
* `ptree-node-already-existing` is signaled by some functions when it is
  requested to create a node at a path in which there is already an existing
  node
* `ptree-node-not-existing` is signaled by some functions when it is
  requested to delete a non-existing node.

## Functions

This section provides descriptions and examples for the functions provided by
`ptree.el` package.

### Property tree construction

#### ptree-empty

`(ptree-empty)` creates an empty property tree and returns the root node of
such tree.

The root node is a category.

Complexity: O(1).

### Node predicates

#### ptree-category-p

`(ptree-category-p NODE)` returns `'t` if NODE is a category, `'nil` otherwise.

Complexity: O(1).

#### ptree-property-p

`(ptree-property-p NODE)` returns `'t` if NODE is a property, `'nil` otherwise.

Complexity: O(1).

### Node attributes

#### ptree-property-value

`(ptree-property-value NODE)` returns the value associated with property at
NODE. 

If NODE is a category, the error `ptree-not-a-property` is signaled.

Complexity: O(1).

#### ptree-property-type

`(ptree-property-type NODE)` returns the type associated with property at
NODE. 

If NODE is a category, the error `ptree-not-a-property` is signaled.

Complexity: O(1).

#### ptree-set-property

`(ptree-set-property NODE VALUE &optional TYPE)` set the VALUE and TYPE of
property at NODE.

If TYPE is not specified, it is set to `'generic`.

If NODE is a category, the error `ptree-not-a-property` is signaled.

Complexity: O(1).

#### ptree-child-nodes-num

`(ptree-child-nodes-num NODE)` returns the number of child nodes of NODE.

if NODE is a property, the function returns 0.

Complexity: O(C), where C is the number of child nodes of NODE.

## Node retrieval

### ptree-child-node-at-index

`(ptree-child-node-at-index NODE INDEX)` returns the child node of NODE at
INDEX.

if NODE is a property or INDEX is out of range, 'nil is returned.

**NOTE**: Automatic tag ordering affects the result of this function.

Complexity: O(C), where C is the number of child nodes of NODE.

### ptree-child-node-with-tag

`(ptree-child-node-with-tag NODE TAG)` returns the child node of NODE having
the specified TAG.

If NODE is a property or if no child node exists with the specified TAG,
'nil is returned.

Complexity: O(C), where C is the number of child nodes of NODE.

## Node construction

### ptree-add-category

`(ptree-add-category NODE TAG)` adds a category with TAG as child of NODE.

The function returns the node associated with the created category.

If NODE is a property, the error `ptree-not-a-category` is signaled.

If a child node with the same tag already exists, the error 
`ptree-node-already-existing` is signaled.

Complexity: O(C), where C is the number of child nodes of NODE.

### ptree-add-property

`(ptree-add-property NODE TAG VALUE &optional TYPE)` adds a property with
TAG, VALUE and TYPE as child of NODE.

If TYPE is not specified, it is set to `'generic`.

The function returns the node associated with the created property.

If NODE is a property, the error `ptree-not-a-category` is signaled.

If a child node with the same tag already exists, the error 
`ptree-node-already-existing` is signaled.

Complexity: O(C), where C is the number of child nodes of NODE.

## Node attachment

### ptree-attach-node

`(ptree-attach-node NODE TAG CHILD)` attaches CHILD node to NODE with TAG.

If NODE is a property, the error `ptree-not-a-category` is signaled.

If a child node with the same tag already exists, the error 
`ptree-node-already-existing` is signaled.

Complexity: O(C), where C is the number of child nodes of NODE.

## Node deletion

### ptree-delete-child-node

`(ptree-delete-child-node NODE TAG)` deletes the child node of NODE with TAG.

If NODE is a property, the error `ptree-not-a-category` is signaled.

If no child node with the specified TAG exists, the error 
`ptree-node-not-existing` is signaled.

Complexity: O(C), where C is the number of child nodes of NODE.

## Convenience functions

### ptree-node-at-path

`(ptree-node-at-path NODE PATH)` returns the descendant node of NODE at PATH.

If such node does not exist, `'nil` is returned.

Complexity: O(D * C), where:

* D is the depth of the path
* C is the average number of child nodes of the node in the path

### ptree-value-at-path

`(ptree-value-at-path NODE PATH)` returns the value of the descendant property
of NODE at PATH.

If such node does not exist or if it is a category, `'not-found` is returned.

Complexity: O(D * C), where:

* D is the depth of the path
* C is the average number of child nodes of the node in the path

### write-categories-at-path

`(ptree-write-categories-at-path NODE PATH &rest TAGS)` add categories with
specified TAGS as child of the descendant node of NODE at PATH.

**WARNING**: This function overwrites any existing node without signaling
errors.

Complexity: O(D * C + CAT * DC), where:

* D is the depth of the path
* C is the average number of child nodes of the node in the path
* CAT is the number of categories to be added
* DC is the number of child nodes of the descendant node at PATH

### write-properties-at-path

`(ptree-write-properties-at-path NODE PATH &rest PROPS-DATA)` add properties
as child of the descendant node of NODE at PATH.

Properties data is speified with paramenter PROPS-DATA, which is a list of
items that can have the following format:

* `(tag . value)`. In such a case the property type is set to `'generic`.
* `(tag value . type)`

**WARNING**: This function overwrites any existing node without signaling
errors.

Complexity: O(D * C + PROP * DC), where:

* D is the depth of the path
* C is the average number of child nodes of the node in the path
* PROP is the number of properties to be added
* DC is the number of child nodes of the descendant node at PATH

## Iterator management

### ptree-iter

`(ptree-iter NODE)` creates an iterator associated with NODE.

NODE represents the inital node for the iterator. 

**NOTE**: The iterator can only move down to the child nodes of the initial 
node. The iterator cannot move to the parent or sibling nodes of the initial 
node.

Complexity: O(1).

### ptree-iter-node

`(ptree-iter-node ITERATOR)` returns the node associated with ITERATOR.

Complexity: O(1).

### ptree-iter-category-p

`(ptree-iter-category-p NODE)` returns `'t` if the node associated with
ITERATOR is a category, `'nil` otherwise.

Complexity: O(1).

### ptree-iter-property-p

`(ptree-iter-property-p NODE)` returns `'t` if the node associated with
ITERATOR is a property, `'nil` otherwise.

Complexity: O(1).

### ptree-iter-value

`(ptree-iter-value ITERATOR)` returns the value of the property associated
with ITERATOR.

If the node associated with ITERATOR is not a property, the error
`ptree-not-a-property` is signaled.

Complexity: O(1).

### ptree-iter-type

`(ptree-iter-type ITERATOR)` returns the type of the property associated
with ITERATOR.

If the node associated with ITERATOR is not a property, the error
`ptree-not-a-property` is signaled.

Complexity: O(1).

### ptree-iter-set-property

`(ptree-iter-set-property ITERATOR VALUE &optional TYPE)` set the VALUE and TYPE
of property associated with ITERATOR.

If TYPE is not specified, it is set to `'generic`.

If NODE is a category, the error `ptree-not-a-property` is signaled.

Complexity: O(1).

### ptree-iter-tag

`(ptree-iter-tag ITERATOR)` returns the tag of the node associated with
ITERATOR.

If the node associated with ITERATOR is the iterator initial node, `'nil` is
returned.

Complexity: O(1).

### ptree-iter-path

`(ptree-iter-path ITERATOR)` returns the path of the node associated with
ITERATOR with respect to the iterator initial node.

If the node associated with ITERATOR is the iterator initial node, `'nil` is
returned.

Complexity: O(P), where P is the length of the returned path.

### ptree-iter-has-child

`(ptree-iter-has-child ITERATOR` returns `'t` if the node associated with the
iterator has child nodes, `'nil` otherwise.

If the node associated with ITERATOR is a property, `'nil` is returned.

Complexity: O(1).

### ptree-iter-has-parent

`(ptree-iter-has-parent ITERATOR` returns `'t` if the node associated with the
iterator has a parent node, `'nil` otherwise.

When this function is called on the iterator initial node, it returns `'nil`
even if the initial node has parent nodes. This is due to the fact that the
iterator cannot look at ancestors or sibling nodes of its initial nodes.

Complexity: O(1).

### ptree-iter-has-next

`(ptree-iter-has-next ITERATOR` returns `'t` if the node associated with the
iterator has a next sibling node, `'nil` otherwise.

Complexity: O(1).

### ptree-iter-has-previous

`(ptree-iter-has-next ITERATOR` returns `'t` if the node associated with the
iterator has a previous sibling node, `'nil` otherwise.

Complexity: O(1).

### ptree-iter-down

`(ptree-iter-down ITERATOR &optional STEPS)` moves ITERATOR down the property
tree by STEPS.

If STEP is not specified a single step is performed.

After each STEP, ITERATOR is associated to the first child node of the current
node.

If it is not possible to perform a step because the node is a property or
because it has no child nodes, the function stops.

The function returns the number of steps not performed, i.e. a return value of
0 indicates that all the requested steps have been successfully performed.

Complexity: O(S), where S is the number of steps performed.

### ptree-iter-up

`(ptree-iter-up ITERATOR &optional STEPS)` moves ITERATOR up the property
tree by STEPS.

If STEP is not specified a single step is performed.

After each STEP, ITERATOR is associated to the parent node of the current
node.

If it is not possible to perform a step because the node is a property or
because it is the iterator initial node, the function stops.

The function returns the number of steps not performed, i.e. a return value of
0 indicates that all the requested steps have been successfully performed.

Complexity: O(S), where S is the number of steps performed.

### ptree-iter-next

`(ptree-iter-next ITERATOR &optional STEPS)` moves ITERATOR to the next 
sibling node of the the property tree by STEPS.

If STEP is not specified a single step is performed.

If it is not possible to perform a step because the node is a property or
because there are no next sibling nodes, the function stops.

The function returns the number of steps not performed, i.e. a return value of
0 indicates that all the requested steps have been successfully performed.

**NOTE**: Automatic tag ordering affects the result of this function.

Complexity: O(S), where S is the number of steps performed.

### ptree-iter-previous

`(ptree-iter-previous ITERATOR &optional STEPS)` moves ITERATOR to the previous 
sibling node of the the property tree by STEPS.

If STEP is not specified a single step is performed.

If it is not possible to perform a step because the node is a property or
because there are no previous sibling nodes, the function stops.

The function returns the number of steps not performed, i.e. a return value of
0 indicates that all the requested steps have been successfully performed.

**NOTE**: Automatic tag ordering affects the result of this function.

Complexity: O(S), where S is the number of steps performed.

### ptree-iter-move-with-tag

`(ptree-iter-move-with-tag ITERATOR TAG)` moves ITERATOR to the child node
of its associated node with TAG.

If such node does not exist, the iterator is not moved and `'nil` is returned.
Otherwise `'t` is returned.

Complexity: O(C), where C is the number of child nodes of the node associated
with ITERATOR.

### ptree-iter-add-property

`(ptree-iter-add-property ITERATOR TAG VALUE &optional TYPE)` adds a property 
with TAG, VALUE and TYPE as child of the node associated with ITERATOR

If TYPE is not specified, it is set to `'generic`.

The function returns the node associated with the created property.

If NODE is a property, the error `ptree-not-a-category` is signaled.

If a child node with the same tag already exists, the error 
`ptree-node-already-existing` is signaled.

Complexity: O(C), where C is the number of child nodes of the node associated
with ITERATOR.

### ptree-iter-add-category

`(ptree-iter-add-category ITERATOR TAG)` adds a category with TAG as child of 
the node associated with ITERATOR

The function returns the node associated with the created category.

If NODE is a property, the error `ptree-not-a-category` is signaled.

If a child node with the same tag already exists, the error 
`ptree-node-already-existing` is signaled.

Complexity: O(C), where C is the number of child nodes of the node associated
with ITERATOR.

### ptree-iter-add-category-and-move

`(ptree-iter-add-category-and-move ITERATOR TAG)` is similar to
`(ptree-iter-add-category ITERATOR TAG)` but the iterator is moved to the
newly created category.

Complexity: O(C), where C is the number of child nodes of the node associated
with ITERATOR.

### ptree-iter-delete-node
`(ptree-iter-delete-node ITERATOR)` deleted the node associated with ITERATOR

If ITERATOR is pointing to the iterator initial node, the node is not deleted
and `'nil` is returned.

Otherwise the node is deleted, ITERATOR is move to the parent node and `'t`
is returned.

Complexity: O(1).

## String conversion

### ptree-to-string

`(ptree-to-string NODE &optional INDENT)` return a string representation of
the property tree at NODE, indenting each tree level by INDENT.

If INDENT is not specified, it is set to 4.

Complexity: O(1).

Complexity: O(D * C), where:

* D is the depth of the property tree at NODE
* C is the average number of child nodes of the node of the property tree at
NODE

## Contacts

Please report bugs, requests and suggestions to Alpha Catharsis 
<alpha.catharsis@gmail.com> or on Github.
