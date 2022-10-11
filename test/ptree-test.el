(require 'ptree)

;; Property tree tests

(ert-deftest ptree-test-create ()
  (should (equal (list nil nil nil) '(nil nil nil))))

(ert-deftest ptree-test-root-p ()
  (should (eq (ptree-root-p '(nil nil nil)) t))
  (should (eq (ptree-root-p '("test" nil nil)) nil))
  (should (eq (ptree-root-p '("test" nil ("test2" nil nil))) nil))
  (should (eq (ptree-root-p '("test" 42)) nil))
  (should (eq (ptree-root-p '("test" nil)) nil)))

(ert-deftest ptree-test-branch-p ()
  (should (eq (ptree-branch-p '(nil nil nil)) nil))
  (should (eq (ptree-branch-p '("test" nil nil)) t))
  (should (eq (ptree-branch-p '("test" nil ("test2" nil nil))) t))
  (should (eq (ptree-branch-p '("test" 42)) nil))
  (should (eq (ptree-branch-p '("test" nil)) nil)))

(ert-deftest ptree-test-leaf-p ()
  (should (eq (ptree-leaf-p '(nil nil nil)) nil))
  (should (eq (ptree-leaf-p '("test" nil nil)) nil))
  (should (eq (ptree-leaf-p '("test" nil ("test2" nil nil))) nil))
  (should (eq (ptree-leaf-p '("test" 42)) t))
  (should (eq (ptree-leaf-p '("test" nil)) t)))

(ert-deftest ptree-test-get-node-tag ()
  (should (eq (ptree-get-node-tag '(nil nil nil)) nil))
  (should (equal (ptree-get-node-tag '("test" nil nil)) "test"))
  (should (equal (ptree-get-node-tag '("test" nil ("test2" nil nil))) "test"))
  (should (equal (ptree-get-node-tag '("test" 42)) "test")))

(ert-deftest ptree-test-get-node-value ()
  (should (eq (ptree-get-node-value '(nil nil nil)) 'not-found))
  (should (eq (ptree-get-node-value '("test" nil nil)) 'not-found))
  (should (eq (ptree-get-node-value '("test" nil ("test2" nil nil)))
              'not-found))
  (should (eq (ptree-get-node-value '("test" 42)) 42))
  (should (eq (ptree-get-node-value '("test" nil)) nil)))

(ert-deftest ptree-test-get-child-nodes-num ()
  (should (eq (ptree-get-child-nodes-num '(nil nil nil)) 0))
  (should (eq (ptree-get-child-nodes-num '("test" nil nil)) 0))
  (should (eq (ptree-get-child-nodes-num '("test" nil ("test2" nil nil))) 1))
  (should (eq (ptree-get-child-nodes-num '("test" 42)) 0))
  (should (eq (ptree-get-child-nodes-num '(nil nil (0 nil nil) (1 nil nil)
                                               (2 nil nil))) 3))
  (should (eq (ptree-get-child-nodes-num '(nil nil (0 42) (1 9)
                                               (2 3))) 3))
  (should (eq (ptree-get-child-nodes-num '("test" nil (0 nil nil) (1 nil nil)
                                           (2 nil nil) (3 nil (31 nil nil))
                                           (4 nil (41 nil nil))
                                           (5 nil (51 nil nil)))) 6)))

(ert-deftest ptree-test-get-child-node-at-index ()
  (should (equal (ptree-get-child-node-at-index '(nil nil nil) 0) nil))
  (should (equal (ptree-get-child-node-at-index '(nil nil nil) 1) nil))
  (should (equal (ptree-get-child-node-at-index '("test" nil nil) 0) nil))
  (should (equal (ptree-get-child-node-at-index '("test" nil nil) 1) nil))
  (should (equal (ptree-get-child-node-at-index '("test" 42) 0) nil))
  (should (equal (ptree-get-child-node-at-index '("test" 42) 1) nil))
  (let ((pt '(nil nil (0 nil nil) (1 "a") (3 nil nil) (4 42))))
    (should (equal (ptree-get-child-node-at-index pt 0)
                   '(0 nil nil)))
    (should (equal (ptree-get-child-node-at-index pt 1)
                   '(1 "a")))
    (should (equal (ptree-get-child-node-at-index pt 2)
                   '(3 nil nil)))
    (should (equal (ptree-get-child-node-at-index pt 3)
                   '(4 42)))
    (should (equal (ptree-get-child-node-at-index pt 4) nil)))
  (let ((pt '("test" nil (0 nil nil) (1 "a") (3 nil nil) (4 42))))
    (should (equal (ptree-get-child-node-at-index pt 0)
                   '(0 nil nil)))
    (should (equal (ptree-get-child-node-at-index pt 1)
                   '(1 "a")))
    (should (equal (ptree-get-child-node-at-index pt 2)
                   '(3 nil nil)))
    (should (equal (ptree-get-child-node-at-index pt 3)
                   '(4 42)))
    (should (equal (ptree-get-child-node-at-index pt 4) nil))))

(ert-deftest ptree-test-get-node-at-path ()
  (let ((pt '(nil nil (0 nil (zero nil ("zero" nil nil)))
                  (zero nil ("zero" nil (0 nil nil)))
                  ("zero" nil (0 nil (zero nil nil))))))
    (should (equal (ptree-get-node-at-path pt 1) nil))
    (should (equal (ptree-get-node-at-path pt '(0 one)) nil))
    (should (equal (ptree-get-node-at-path pt '(0 zero "one")) nil))
    (should (equal (ptree-get-node-at-path pt '(0 zero "zero" "one")) nil))
    (should (equal (ptree-get-node-at-path pt 0)
                   '(0 nil (zero nil ("zero" nil nil)))))
    (should (equal (ptree-get-node-at-path pt '(0 zero))
                   '(zero nil ("zero" nil nil))))
    (should (equal (ptree-get-node-at-path pt 'zero)
                   '(zero nil ("zero" nil (0 nil nil)))))
    (should (equal (ptree-get-node-at-path pt '(zero "zero"))
                   '("zero" nil (0 nil nil))))
    (should (equal (ptree-get-node-at-path pt "zero")
                   '("zero" nil (0 nil (zero nil nil)))))
    (should (equal (ptree-get-node-at-path pt '("zero" 0))
                   '(0 nil (zero nil nil))))))

(ert-deftest ptree-test-get-value-at-path ()
  (let ((pt '(nil nil (0 42) (zero nil ("zero" 9))
                  ("one" nil)
                  ("zero" nil (0 nil (zero 3))))))
    (should (eq (ptree-get-value-at-path pt 1) 'not-found))
    (should (eq (ptree-get-value-at-path pt '(zero 0)) 'not-found))
    (should (eq (ptree-get-value-at-path pt '("zero" 0 one)) 'not-found))
    (should (eq (ptree-get-value-at-path pt '("zero" 0 zero one)) 'not-found))
    (should (eq (ptree-get-value-at-path pt 0) 42))
    (should (eq (ptree-get-value-at-path pt '(zero "zero")) 9))
    (should (eq (ptree-get-value-at-path pt '("zero" 0 zero)) 3))
    (should (eq (ptree-get-value-at-path pt "one") nil))))

(ert-deftest ptree-test-set-node-value ()
  (let ((pt (list nil nil nil)))
    (should (eq (ptree-set-node-value pt 42) nil))
    (should (equal pt '(nil nil nil))))
  (let ((pt (list "test" nil '(zero 3) '(one 9))))
    (should (eq (ptree-set-node-value pt 42) t))
    (should (equal pt '("test" 42))))
  (let ((pt (list "test" 42)))
    (should (eq (ptree-set-node-value pt 9) t))
    (should (equal pt '("test" 9)))
    (should (eq (ptree-set-node-value pt "a") t))
    (should (equal pt '("test" "a")))))

(ert-deftest ptree-test-add-child-nodes ()
  (let ((pt (list nil nil nil)))
    (ptree-add-child-nodes pt)
    (should (equal pt '(nil nil nil)))
    (ptree-add-child-nodes pt "zero" 0 'zero)
    (should (equal pt '(nil nil (0 nil nil) (zero nil nil) ("zero" nil nil))))
    (ptree-add-child-nodes pt 'zero 0 "zero")
    (should (equal pt '(nil nil (0 nil nil) (zero nil nil) ("zero" nil nil)))))
  (let ((pt (list nil nil nil)))
    (ptree-add-child-nodes pt 0 0 0 0 0 0 0 0)
    (should (equal pt '(nil nil (0 nil nil)))))
  (let ((pt (list nil nil (list 0 nil (list 1 nil nil)))))
    (ptree-add-child-nodes pt 0)
    (should (equal pt '(nil nil (0 nil (1 nil nil)))))))

(ert-deftest ptree-test-add-node-at-path ()
  (let ((pt (list nil nil nil)))
    (ptree-add-node-at-path pt 0)
    (ptree-add-node-at-path pt '(zero))
    (ptree-add-node-at-path pt "one")
    (should (equal pt '(nil nil (0 nil nil) (zero nil nil) ("one" nil nil)))))
  (let ((pt (list nil nil nil)))
    (ptree-add-node-at-path pt '(0 1 2))
    (ptree-add-node-at-path pt '(0 zero "one"))
    (ptree-add-node-at-path pt '("one" zero 0))
    (should (equal pt '(nil nil (0 nil (1 nil (2 nil nil))
                                   (zero nil ("one" nil nil)))
                            ("one" nil (zero nil (0 nil nil)))))))
  (let ((pt (list nil (list "test" 42))))
    (ptree-add-node-at-path pt '("test" 0))
    (should (equal pt '(nil nil ("test" nil (0 nil nil))))))
  (let ((pt (list nil nil nil)))
    (ptree-add-node-at-path pt '(0 1 2 3))
    (ptree-add-node-at-path pt '(0 1))
    (should (equal pt '(nil nil (0 nil (1 nil (2 nil (3 nil nil)))))))))


(ert-deftest ptree-test-add-value-at-path ()
  (let ((pt (list nil nil nil)))
    (ptree-add-value-at-path pt "test" 42)
    (should (equal pt '(nil nil ("test" 42))))
    (ptree-add-value-at-path pt '(0 zero) 9)
    (should (equal pt '(nil nil (0 nil (zero 9)) ("test" 42))))
    (ptree-add-value-at-path pt '(0 zero) 3)
    (should (equal pt '(nil nil (0 nil (zero 3)) ("test" 42))))
    (ptree-add-value-at-path pt "test2" nil)
    (should (equal pt '(nil nil (0 nil (zero 3)) ("test" 42) ("test2" nil))))
    (ptree-add-value-at-path pt 0 10)
    (should (equal pt '(nil nil (0 10) ("test" 42) ("test2" nil))))))

(ert-deftest ptree-test-delete-child-nodes ()
  (let ((pt (list nil nil (list 0 42)
                  (list 'zero nil (list "zero" 9))
                  (list "one" nil)
                  (list "zero" nil (list 0 nil (list 'zero 3))))))
    (should (equal (ptree-delete-child-nodes pt 'one) '(one)))
    (should (equal (ptree-delete-child-nodes pt 1) '(1)))
    (should (equal (ptree-delete-child-nodes pt "two") '("two")))
    (should (eq (ptree-delete-child-nodes pt) nil))
    (should (equal pt '(nil nil (0 42) (zero nil ("zero" 9))
                            ("one" nil)
                            ("zero" nil (0 nil (zero 3))))))
    (should (eq (ptree-delete-child-nodes pt "zero") nil))
    (should (equal pt '(nil nil (0 42) (zero nil ("zero" 9))
                            ("one" nil))))
    (should (eq (ptree-delete-child-nodes pt "one") nil))
    (should (equal pt '(nil nil (0 42) (zero nil ("zero" 9)))))
    (should (eq (ptree-delete-child-nodes pt 0 'zero) nil))
    (should (equal pt '(nil nil nil)))))

(ert-deftest ptree-test-delete-node-at-path ()
  (let ((pt (list nil nil (list 0 42)
                  (list 'zero nil (list "zero" 9))
                  (list "one" nil)
                  (list "zero" nil (list 0 nil (list 'zero 3))))))
    (should (eq (ptree-delete-node-at-path pt -1) nil))
    (should (eq (ptree-delete-node-at-path pt "zzz") nil))
    (should (eq (ptree-delete-node-at-path pt '("zero" 0 zero one)) nil))
    (should (equal (ptree-delete-node-at-path pt '("zero" 0 zero))
                   '(zero 3)))
    (should (equal pt '(nil nil (0 42) (zero nil ("zero" 9))
                            ("one" nil)
                            ("zero" nil (0 nil nil)))))
    (should (equal (ptree-delete-node-at-path pt "zero")
                   '("zero" nil (0 nil nil))))
    (should (equal pt '(nil nil (0 42) (zero nil ("zero" 9))
                            ("one" nil))))
    (should (equal (ptree-delete-node-at-path pt 0)
                   '(0 42)))
    (should (equal pt '(nil nil (zero nil ("zero" 9)) ("one" nil))))
    (should (equal (ptree-delete-node-at-path pt "one")
                   '("one" nil)))
    (should (equal pt '(nil nil (zero nil ("zero" 9)))))
    (should (equal (ptree-delete-node-at-path pt '(zero "zero"))
                   '("zero" 9)))
    (should (equal pt '(nil nil (zero nil nil))))
    (should (equal (ptree-delete-node-at-path pt 'zero)
                   '(zero nil nil)))
    (should (equal pt '(nil nil nil)))))

;; Property tree iterator tests

(ert-deftest ptree-test-iter ()
  (let* ((pt '("test" nil (zero 42)))
         (iter (ptree-iter pt)))
    (should (equal iter (list pt nil)))
    (should (eq (car iter) pt))))

(ert-deftest ptree-test-iter-node ()
  (let* ((pt '("test" nil (zero 42)))
         (iter (ptree-iter pt)))
    (should (eq (ptree-iter-node iter) pt))))

(ert-deftest ptree-test-iter-root-p ()
  (should (eq (ptree-iter-root-p (ptree-iter '(nil nil (zero 42)))) t))
  (should (eq (ptree-iter-root-p (ptree-iter '("test" nil (zero 42)))) nil))
  (should (eq (ptree-iter-root-p (ptree-iter '(zero 42))) nil)))

(ert-deftest ptree-test-iter-branch-p ()
  (should (eq (ptree-iter-branch-p (ptree-iter '(nil nil (zero 42)))) nil))
  (should (eq (ptree-iter-branch-p (ptree-iter '("test" nil (zero 42)))) t))
  (should (eq (ptree-iter-branch-p (ptree-iter '(zero 42))) nil)))

(ert-deftest ptree-test-iter-leaf-p ()
  (should (eq (ptree-iter-leaf-p (ptree-iter '(nil nil (zero 42)))) nil))
  (should (eq (ptree-iter-leaf-p (ptree-iter '("test" nil (zero 42)))) nil))
  (should (eq (ptree-iter-leaf-p (ptree-iter '(zero 42))) t)))

(ert-deftest ptree-test-iter-tag ()
  (should (equal (ptree-iter-tag (ptree-iter '(nil nil (zero 42)))) nil))
  (should (equal (ptree-iter-tag (ptree-iter '("test" nil (zero 42)))) "test"))
  (should (equal (ptree-iter-tag (ptree-iter '(zero 42))) 'zero))
  (let ((tag "test"))
    (should (eq (ptree-iter-tag (ptree-iter (list tag nil nil))) tag))))

(ert-deftest ptree-test-iter-value ()
  (should (equal (ptree-iter-value (ptree-iter '(nil nil (zero 42))))
                 'not-found))
  (should (equal (ptree-iter-value (ptree-iter '("test" nil (zero 42))))
                 'not-found))
  (should (equal (ptree-iter-value (ptree-iter '(zero 42))) 42))
  (should (equal (ptree-iter-value (ptree-iter '(zero nil))) nil))
  (let ((value "test"))
    (should (eq (ptree-iter-value (ptree-iter (list "test" value))) value))))

(ert-deftest ptree-test-iter-set-value ()
  (should-error (ptree-iter-value (ptree-iter '(nil nil (zero 42))) 9))
  (should-error (ptree-iter-value (ptree-iter '("test" nil (zero 42))) 9))
  (let* ((pt (list 'zero 42))
         (iter (ptree-iter pt)))
    (ptree-iter-set-value iter 9)
    (should (equal pt '(zero 9)))))

(ert-deftest ptree-test-iter-move-down ()
  (let* ((child-3 '(zero 42))
         (child-2 (list "test" nil child-3 '("one" nil nil)))
         (child-1 (list 0 nil child-2 '("ZZZ" 9)))
         (root (list nil nil child-1))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-move-down iter) t))
    (should (eq (car iter) child-1))
    (should (eq (ptree-iter-move-down iter) t))
    (should (eq (car iter) child-2))
    (should (eq (ptree-iter-move-down iter) t))
    (should (eq (car iter) child-3))
    (should (eq (ptree-iter-move-down iter) nil))
    (should (eq (car iter) child-3))))

(ert-deftest ptree-test-iter-move-to-tag ()
  (let* ((child-3 '(zero 42))
         (child-2 (list "test" nil '("one" nil nil) child-3))
         (child-1 (list 0 nil child-2 '("ZZZ" 9)))
         (root (list nil nil '(-1 nil nil) child-1))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-move-to-tag iter 10) nil))
    (should (eq (car iter) root))
    (should (eq (ptree-iter-move-to-tag iter 0) t))
    (should (eq (car iter) child-1))
    (should (eq (ptree-iter-move-to-tag iter "abc") nil))
    (should (eq (car iter) child-1))
    (should (eq (ptree-iter-move-to-tag iter "test") t))
    (should (eq (car iter) child-2))
    (should (eq (ptree-iter-move-to-tag iter 'two) nil))
    (should (eq (car iter) child-2))
    (should (eq (ptree-iter-move-to-tag iter 'zero) t))
    (should (eq (car iter) child-3))
    (should (eq (ptree-iter-move-to-tag iter 0) nil))
    (should (eq (car iter) child-3))))

(ert-deftest ptree-test-iter-move-up ()
  (let* ((child-3 '(zero 42))
         (child-2 (list "test" nil child-3 '("one" nil nil)))
         (child-1 (list 0 nil child-2 '("ZZZ" 9)))
         (root (list nil nil child-1))
         (iter (list child-3 child-2 child-1 root)))
    (should (eq (ptree-iter-move-up iter) t))
    (should (eq (car iter) child-2))
    (should (eq (ptree-iter-move-up iter) t))
    (should (eq (car iter) child-1))
    (should (eq (ptree-iter-move-up iter) t))
    (should (eq (car iter) root))
    (should (eq (ptree-iter-move-up iter) nil))))

(ert-deftest ptree-test-iter-move-next ()
  (let* ((child-1 '(zero 42))
         (child-2 '(0 nil nil))
         (child-3 '("test" nil (one 9)))
         (parent (list "a" nil child-1 child-2 child-3))
         (iter (list child-1 parent)))
    (should (eq (ptree-iter-move-next iter) t))
    (should (eq (car iter) child-2))
    (should (eq (ptree-iter-move-next iter) t))
    (should (eq (car iter) child-3))
    (should (eq (ptree-iter-move-next iter) nil))))

(ert-deftest ptree-test-iter-move-previous ()
  (let* ((child-1 '(zero 42))
         (child-2 '(0 nil nil))
         (child-3 '("test" nil (one 9)))
         (parent (list "a" nil child-1 child-2 child-3))
         (iter (list child-3 parent)))
    (should (eq (ptree-iter-move-previous iter) t))
    (should (eq (car iter) child-2))))

(ert-deftest ptree-test-iter-add-child-nodes ()
  (let* ((child-1 (list "test" nil (list 'zero 42)))
         (root (list nil nil child-1))
         (iter (list child-1 root)))
    (ptree-iter-add-child-nodes iter)
    (should (eq (car iter) child-1))
    (should (eq (cadr iter) root))
    (ptree-iter-add-child-nodes iter 'abc "b" "c")
    (should (equal (car iter) '("test" nil (abc nil nil) (zero 42)
                                ("b" nil nil) ("c" nil nil))))
    (should (equal (cadr iter) '(nil nil ("test" nil (abc nil nil) (zero 42)
                                ("b" nil nil) ("c" nil nil)))))
    (ptree-iter-add-child-nodes iter 'abc)
    (should (equal (car iter) '("test" nil (abc nil nil) (zero 42)
                                ("b" nil nil) ("c" nil nil))))
    (should (equal (cadr iter) '(nil nil ("test" nil (abc nil nil) (zero 42)
                                ("b" nil nil) ("c" nil nil)))))))

(ert-deftest ptree-test-iter-add-child-with-value ()
  (let* ((child-1 (list "test" nil (list 'zero 42)))
         (root (list nil nil child-1))
         (iter (list child-1 root)))
    (ptree-iter-add-child-with-value iter 0 "x")
    (should (equal (car iter) '("test" nil (0 "x") (zero 42))))
    (should (equal (cadr iter) '(nil nil ("test" nil (0 "x") (zero 42)))))
    (ptree-iter-add-child-with-value iter "a" 9)
    (should (equal (car iter) '("test" nil (0 "x") (zero 42) ("a" 9))))
    (should (equal (cadr iter) '(nil nil ("test" nil (0 "x") (zero 42)
                                          ("a" 9)))))
    (ptree-iter-add-child-with-value iter 'zero 100)
    (should (equal (car iter) '("test" nil (0 "x") (zero 100) ("a" 9))))
    (should (equal (cadr iter) '(nil nil ("test" nil (0 "x") (zero 100)
                                          ("a" 9)))))))

(ert-deftest ptree-test-iter-add-child-and-move ()
  (let* ((child-1 (list "test" nil (list 'zero 42)))
         (root (list nil nil child-1))
         (iter (list root root)))
    (ptree-iter-add-child-and-move iter 'zero)
    (should (equal (car iter) '(zero nil nil)))
    (should (equal (cadr iter) '(nil nil (zero nil nil)
                                     ("test" nil (zero 42)))))
    (setq iter (list root root))
    (ptree-iter-add-child-and-move iter "test")
    (should (equal (car iter) '("test" nil (zero 42))))
    (should (equal (cadr iter) '(nil nil (zero nil nil)
                                     ("test" nil (zero 42)))))
    (setq iter (list root root))
    (ptree-iter-add-child-and-move iter "zero")
    (should (equal (car iter) '("zero" nil nil)))
    (should (equal (cadr iter) '(nil nil (zero nil nil)
                                     ("test" nil (zero 42))
                                     ("zero" nil nil))))))

(ert-deftest ptree-test-iter-delete-node ()
  (let* ((child-1 '(zero 42))
         (child-2 '(0 nil nil))
         (child-3 '("test" nil (one 9)))
         (parent (list "a" nil child-1 child-2 child-3))
         (iter (list child-1 parent)))
    (should (eq (ptree-iter-delete-node iter) child-1))
    (should (eq (car iter) child-2))
    (should (equal (cadr iter) '("a" nil (0 nil nil) ("test" nil (one 9)))))
    (should (eq (ptree-iter-delete-node iter) child-2))
    (should (eq (car iter) child-3))
    (should (equal (cadr iter) '("a" nil ("test" nil (one 9)))))
    (should (eq (ptree-iter-delete-node iter) child-3))
    (should (eq (car iter) parent))
    (should (eq (cadr iter) nil))
    (should (eq (ptree-iter-delete-node iter) nil))))

(ert-deftest ptree-test-iter-delete-child-nodes ()
  (let* ((child-1 '(zero 42))
         (child-2 '(0 nil nil))
         (child-3 '("test" nil (one 9)))
         (parent (list "a" nil child-1 child-2 child-3))
         (iter (list parent nil)))
    (ptree-iter-delete-child-nodes iter 'zero "test")
    (should-error (ptree-iter-delete-child-nodes 'one))
    (should (equal (car iter) '("a" nil (0 nil nil))))
    (should (equal (cadr iter) nil))
    (ptree-iter-delete-child-nodes iter 0)
    (should (equal (car iter) '("a" nil nil)))
    (should (equal (cadr iter) nil))))

(ert-deftest ptree-test-to-string ()
  (let ((pt '(nil nil
                  (current-users 12)
                  (desktop nil (background-color "black"))
                  (windows nil
                           ("xterm" nil
                            (pos-x 50)
                            (pos-y 100)
                            (width 500)
                            (height 200))
                           ("emacs" nil
                            (pos-x 600)
                            (pos-y 0)
                            (width 1000)
                            (height 800)))
                  (processes nil
                             (by-id nil
                                    (0 "startx")
                                    (1 "dbus-launch")
                                    (2 "X"))))))
    (should (string= (ptree-to-string pt) "current-users: 12
desktop
  background-color: black
windows
  \"xterm\"
    pos-x: 50
    pos-y: 100
    width: 500
    height: 200
  \"emacs\"
    pos-x: 600
    pos-y: 0
    width: 1000
    height: 800
processes
  by-id
    0: startx
    1: dbus-launch
    2: X\n"))
  ))

;; Test launcher

(defun ptree-run-tests ()
  (interactive)
  (ert-run-tests-interactively "ptree-test-"))
