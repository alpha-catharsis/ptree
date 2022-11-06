(require 'ptree)

;; Property tree tests

(ert-deftest ptree-test-empty ()
  (should (equal (ptree-empty) '(nil))))

(ert-deftest ptree-test-category-p ()
  (should (eq (ptree-category-p '(nil)) t))
  (should (eq (ptree-category-p '(nil "tsst" (nil))) t))
  (should (eq (ptree-category-p '(nil "test" (void . nil))) t))
  (should (eq (ptree-category-p '(nil "test" (number . 42))) t))
  (should (eq (ptree-category-p '(nil 0 (number . 42))) t))
  (should (eq (ptree-category-p '(nil test (number . 42))) t))
  (should (eq (ptree-category-p '(nil "test" (nil "test2" (number . 42)))) t))
  (should (eq (ptree-category-p '(generic . 42)) nil))
  (should (eq (ptree-category-p '(string . "a")) nil)))

(ert-deftest ptree-test-property-p ()
  (should (eq (ptree-property-p '(nil)) nil))
  (should (eq (ptree-property-p '(nil "tsst" (nil))) nil))
  (should (eq (ptree-property-p '(nil "test" (void . nil))) nil))
  (should (eq (ptree-property-p '(nil "test" (number . 42))) nil))
  (should (eq (ptree-property-p '(nil 0 (number . 42))) nil))
  (should (eq (ptree-property-p '(nil test (number . 42))) nil))
  (should (eq (ptree-property-p '(nil "test" (nil "test2" (number . 42)))) nil))
  (should (eq (ptree-property-p '(generic . 42)) t))
  (should (eq (ptree-property-p '(string . "a")) t)))

(ert-deftest ptree-test-property-type ()
  (should-error (ptree-property-type '(nil)) :type 'ptree-not-a-property)
  (should-error (ptree-property-type '(nil "test" (nil)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-type '(nil "test" (void . nil)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-type '(nil "test" (number . 42)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-type '(nil 0 (number . 42)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-type '(nil test (number . 42)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-type '(nil "test" (nil "test2" (number . 42))))
                :type 'ptree-not-a-property)
  (should (eq (ptree-property-type '(generic . 42)) 'generic))
  (should (eq (ptree-property-type '(string . "a")) 'string)))

(ert-deftest ptree-test-property-value ()
  (should-error (ptree-property-value '(nil)) :type 'ptree-not-a-property)
  (should-error (ptree-property-value '(nil "test" (nil)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-value '(nil "test" (void . nil)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-value '(nil "test" (number . 42)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-value '(nil 0 (number . 42)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-value '(nil test (number . 42)))
                :type 'ptree-not-a-property)
  (should-error (ptree-property-value '(nil "test" (nil "test2" (number . 42))))
                :type 'ptree-not-a-property)
  (should (eq (ptree-property-value '(generic . 42)) 42))
  (should (equal (ptree-property-value '(string . "a")) "a")))

(ert-deftest ptree-test-set-property ()
  (should-error (ptree-set-property '(nil) 9) :type 'ptree-not-a-property)
  (should-error (ptree-set-property '(nil "test" (nil)) 9 'number)
                :type 'ptree-not-a-property)
  (let ((node (list 'generic 42)))
    (ptree-set-property node 9)
    (should (equal node '(generic . 9)))
    (ptree-set-property node "aaa" 'string)
    (should (equal node '(string . "aaa")))))

(ert-deftest ptree-test-child-nodes-num ()
  (should (eq (ptree-child-nodes-num '(generic . 42)) 0))
  (should (eq (ptree-child-nodes-num '(nil)) 0))
  (should (eq (ptree-child-nodes-num '(nil "test" (nil))) 1))
  (should (eq (ptree-child-nodes-num '(nil "test" (void . nil))) 1))
  (should (eq (ptree-child-nodes-num '(nil "test" (number . 42))) 1))
  (should (eq (ptree-child-nodes-num '(nil "test" (nil "test2" (void . nil))))
              1))
  (should (eq (ptree-child-nodes-num '(nil 0 (void . nil) 1 (number . 42)
                                     2 (string . "a"))) 3))
  (should (eq (ptree-child-nodes-num '(nil 0 (void . nil) 'a (number . 42)
                                     "a" (number . 9))) 3))
  (should (eq (ptree-child-nodes-num '(nil 0 (void . nil) 1 (void . nil)
                                     2 (void . nil)
                                     3 (nil 31 (void . nil))
                                     4 (nil 41 (void . nil))
                                     5 (nil 51 (void . nil)))) 6)))

(ert-deftest ptree-test-child-node-at-index ()
  (should (eq (ptree-child-node-at-index '(generic . 42) 0) nil))
  (should (eq (ptree-child-node-at-index '(nil) 0) nil))
  (should (eq (ptree-child-node-at-index '(nil) 1) nil))
  (let* ((child-1 '(nil "test" (void . nil)))
         (child-2 '(number . 42))
         (child-3 '(string . "a"))
         (child-4 '(nil))
         (root (list nil 0 child-1 1 child-2 2 child-3 3 child-4)))
    (should (eq (ptree-child-node-at-index root 0) child-1))
    (should (eq (ptree-child-node-at-index root 1) child-2))
    (should (eq (ptree-child-node-at-index root 2) child-3))
    (should (eq (ptree-child-node-at-index root 3) child-4))
    (should (eq (ptree-child-node-at-index root 4) nil))))

(ert-deftest ptree-test-child-node-with-tag ()
  (should (eq (ptree-child-node-with-tag '(nil) 'a) nil))
  (should (eq (ptree-child-node-with-tag '(generic . 42) 'a) nil))
  (let* ((child-1 '(nil "test" (void . nil)))
         (child-2 '(number . 42))
         (child-3 '(string . "a"))
         (child-4 '(nil))
         (root (list nil 0 child-1 1 child-2 'test child-3 "test" child-4)))
    (should (eq (ptree-child-node-with-tag root 0) child-1))
    (should (eq (ptree-child-node-with-tag root 1) child-2))
    (should (eq (ptree-child-node-with-tag root 'test) child-3))
    (should (eq (ptree-child-node-with-tag root "test") child-4))
    (should (eq (ptree-child-node-with-tag root -1) nil))
    (should (eq (ptree-child-node-with-tag root "tooth") nil))))

(ert-deftest ptree-test-add-category ()
  (should-error (ptree-add-category '(generic . 42) "test")
                :type 'ptree-not-a-category)
  (should-error (ptree-add-category '(nil "test" (nil)) "test")
                :type 'ptree-node-already-existing)
  (let ((root (list nil)))
    (should (equal (ptree-add-category root "test") '(nil)))
    (should (equal root '(nil "test" (nil)))))
  (let ((root (list nil 'test '(generic . 42))))
    (should (equal (ptree-add-category root 0) '(nil)))
    (should (equal root '(nil 0 (nil) test (generic . 42)))))
  (let ((root (list nil 'test '(generic . 42))))
    (should (equal (ptree-add-category root "test") '(nil)))
    (should (equal root '(nil test (generic . 42) "test" (nil)))))
  (let ((root (list nil 0 '(generic . 42) "test" '(nil "test2" (nil)))))
    (should (equal (ptree-add-category root 'test) '(nil)))
    (should (equal root '(nil 0 (generic . 42) test (nil) "test"
                              (nil "test2" (nil)))))))

(ert-deftest ptree-test-add-property ()
  (should-error (ptree-add-property '(generic . 42) "test" 9)
                :type 'ptree-not-a-category)
  (should-error (ptree-add-property '(nil "test" (nil)) "test" 9)
                :type 'ptree-node-already-existing)
  (let ((root (list nil)))
    (should (equal (ptree-add-property root "test" 9) '(generic . 9)))
    (should (equal root '(nil "test" (generic . 9)))))
  (let ((root (list nil 'test '(generic . 42))))
    (should (equal (ptree-add-property root 0 9 'number) '(number . 9)))
    (should (equal root '(nil 0 (number . 9) test (generic . 42)))))
  (let ((root (list nil 'test '(generic . 42))))
    (should (equal (ptree-add-property root "test" "a" 'string) '(string ."a")))
    (should (equal root '(nil test (generic . 42) "test" (string . "a")))))

  (let ((root (list nil 0 '(generic . 42) "test" '(nil "test2" (nil)))))
    (should (equal (ptree-add-property root 'test 'abc) '(generic . abc)))
    (should (equal root '(nil 0 (generic . 42) test (generic . abc) "test"
                              (nil "test2" (nil)))))))

(ert-deftest ptree-test-attach-node ()
  (should-error (ptree-attach-node '(generic . 42) 'test '(nil))
                :type 'ptree-not-a-category)
  (let ((child '(nil "test2" (generic . 42)))
        (root (list nil 0 '(nil) "test" '(string . "aaa"))))
    (should-error (ptree-attach-node root "test" child)
                  :type 'ptree-node-already-existing)
    (ptree-attach-node root 'test child)
    (should (equal root '(nil 0 (nil) test (nil "test2" (generic . 42))
                              "test" (string . "aaa"))))))

(ert-deftest ptree-test-delete-child-node ()
  (should-error (ptree-delete-child-node '(generic . 42) "test")
                :type 'ptree-not-a-category)
  (let* ((child-1 '(generic . 42))
         (child-2 '(nil))
         (child-3 '(nil "test2" (nil)))
         (root (list nil 0 child-1 'test child-2 "test" child-3)))
    (should-error (ptree-delete-child-node root -1)
                  :type 'ptree-node-not-existing)
    (should-error (ptree-delete-child-node root 'taste)
                  :type 'ptree-node-not-existing)
    (should-error (ptree-delete-child-node root "taste")
                  :type 'ptree-node-not-existing)
    (should-error (ptree-delete-child-node root "toast")
                  :type 'ptree-node-not-existing))
  (let* ((child-1 '(generic . 42))
         (child-2 '(nil))
         (child-3 '(nil "test2" (nil)))
         (root (list nil 0 child-1 'test child-2 "test" child-3)))
    (ptree-delete-child-node root 0)
    (should (equal root (list nil 'test child-2 "test" child-3))))
  (let* ((child-1 '(generic . 42))
         (child-2 '(nil))
         (child-3 '(nil "test2" (nil)))
         (root (list nil 0 child-1 'test child-2 "test" child-3)))
    (ptree-delete-child-node root 'test)
    (should (equal root (list nil 0 child-1 "test" child-3))))
  (let* ((child-1 '(generic . 42))
         (child-2 '(nil))
         (child-3 '(nil "test2" (nil)))
         (root (list nil 0 child-1 'test child-2 "test" child-3)))
    (ptree-delete-child-node root "test")
    (should (equal root (list nil 0 child-1 'test  child-2)))))

(ert-deftest ptree-test-node-at-path ()
  (let* ((child-1 '(nil))
         (child-2-1-1 '(string . "a"))
         (child-2-1-2 '(string . "b"))
         (child-2-1-3 '(string . "c"))
         (child-2-1 (list nil 'a child-2-1-1 'b child-2-1-2
                          'c child-2-1-3))
         (child-2-2-1 '(string . "aaa"))
         (child-2-2-2 '(string . "bbb"))
         (child-2-2 (list nil "a" child-2-2-1 "b" child-2-2-2))
         (child-2 (list nil "letters" child-2-1 "strings" child-2-2))
         (child-3-1-1-1 '(symbol . z))
         (child-3-1-1 (list nil 'y child-3-1-1-1))
         (child-3-1 (list nil 'x child-3-1-1))
         (child-3 (list nil 'symbols child-3-1))
         (root (list nil 0 child-1 1 child-2 2 child-3)))
    (should (eq (ptree-node-at-path root 0) child-1))
    (should (eq (ptree-node-at-path root '(0)) child-1))
    (should (eq (ptree-node-at-path root '(1)) child-2))
    (should (eq (ptree-node-at-path root '(1 "letters")) child-2-1))
    (should (eq (ptree-node-at-path root '(1 "letters" a)) child-2-1-1))
    (should (eq (ptree-node-at-path root '(1 "letters" b)) child-2-1-2))
    (should (eq (ptree-node-at-path root '(1 "letters" c)) child-2-1-3))
    (should (eq (ptree-node-at-path root '(1 "strings")) child-2-2))
    (should (eq (ptree-node-at-path root '(1 "strings" "a")) child-2-2-1))
    (should (eq (ptree-node-at-path root '(1 "strings" "b")) child-2-2-2))
    (should (eq (ptree-node-at-path root 2) child-3))
    (should (eq (ptree-node-at-path root '(2)) child-3))
    (should (eq (ptree-node-at-path root '(2 symbols)) child-3-1))
    (should (eq (ptree-node-at-path root '(2 symbols x)) child-3-1-1))
    (should (eq (ptree-node-at-path root '(2 symbols x y)) child-3-1-1-1))
    (should (eq (ptree-node-at-path root '(2 symbols x y z)) nil))
    (should (eq (ptree-node-at-path root '(2 symbols x z)) nil))
    (should (eq (ptree-node-at-path root 3) nil))))

(ert-deftest ptree-test-value-at-path ()
  (let ((root '(nil 0 (generic . 42) 1 (nil) test (nil 2 (number . 9)
                                                       3 (currenty . 100))
                    "test" (nil 4 (nil 5 (nil))))))
    (should (eq (ptree-value-at-path root 1) 'not-found))
    (should (eq (ptree-value-at-path root '(1 2)) 'not-found))
    (should (eq (ptree-value-at-path root '("test" 4 5)) 'not-found))
    (should (eq (ptree-value-at-path root '("test" 4 5 6)) 'not-found))
    (should (eq (ptree-value-at-path root 6) 'not-found))
    (should (eq (ptree-value-at-path root 0) 42))
    (should (eq (ptree-value-at-path root '(test 2)) 9))
    (should (eq (ptree-value-at-path root '(test 3)) 100))))

(ert-deftest ptree-test-write-categories-at-path ()
  (let ((root (list nil)))
    (should (equal (ptree-write-categories-at-path root '(0 "test" test)) nil))
    (should (equal root '(nil 0 (nil "test" (nil test (nil)))))))
  (let ((root (list nil 0 '(generic . 42) "test" '(nil))))
    (should (equal (ptree-write-categories-at-path root 'test 'a 'b 'c)
                   '(nil)))
    (should (equal root '(nil 0 (generic . 42) test (nil a (nil) b (nil)
                                                         c (nil))
                              "test" (nil)))))
  (let ((root (list nil 0 '(generic . 42) "test" '(nil))))
    (should (equal (ptree-write-categories-at-path root '(0 1 2) 'a 'b 'c)
                   '(nil)))
    (should (equal root '(nil 0 (nil 1 (nil 2 (nil a (nil) b (nil) c (nil))))
                              "test" (nil)))))
  (let ((root (list nil "test" '(nil 0 (generic . 42) 2 (nil)))))
    (should (equal (ptree-write-categories-at-path root "test" '1)
                   '(nil)))
    (should (equal root '(nil "test" (nil 0 (generic . 42) 1 (nil)
                                          2 (nil))))))
  (let* ((child-1 (list nil "a" (list nil)))
         (root (list nil "test" (list nil 0 child-1))))
    (should (eq (ptree-write-categories-at-path root "test" '0) child-1))))

(ert-deftest ptree-test-write-properties-at-path ()
  (let ((root (list nil)))
    (should (equal (ptree-write-properties-at-path root '(0 "test" test)) nil))
    (should (equal root '(nil 0 (nil "test" (nil test (nil)))))))
  (let ((root (list nil 0 '(generic . 42) "test" '(nil))))
    (should (equal (ptree-write-properties-at-path root 'test '("a" . 9))
                   '(generic . 9)))
    (should (equal root '(nil 0 (generic . 42) test (nil "a" (generic . 9))
                              "test" (nil)))))
  (let ((root (list nil 0 '(generic . 42) "test" '(nil))))
    (should (equal (ptree-write-properties-at-path root '("tooth" 0 int)
                                                   '("a" 9 . number))
                   '(number . 9)))
    (should (equal root '(nil 0 (generic . 42) "test" (nil)
                              "tooth" (nil 0 (nil int (nil "a"
                                                           (number . 9))))))))
  (let ((root (list nil 0 '(generic . 42) "test" '(nil))))
    (should (equal (ptree-write-properties-at-path root '(0 1 2) '("a" . 9)
                                                   '(b 10 . integer)
                                                   '(c 20 . currency))
                   '(currency . 20)))
    (should (equal root '(nil 0 (nil 1 (nil 2 (nil b (integer . 10)
                                                   c (currency . 20)
                                                   "a" (generic . 9))))
                              "test" (nil)))))
  (let ((root (list nil "test" '(nil 0 (generic . 42) 2 (nil)))))
    (should (equal (ptree-write-properties-at-path root "test" '(1 . 9))
                   '(generic . 9)))
    (should (equal root '(nil "test" (nil 0 (generic . 42) 1 (generic . 9)
                                          2 (nil))))))
  (let ((root (list nil "test" '(nil 0 (nil)))))
    (should (equal (ptree-write-properties-at-path root "test" '(0 . 9))
                   '(generic . 9)))
    (should (equal root '(nil "test" (nil 0 (generic . 9)))))))

;; ;; Property tree iterator tests

(ert-deftest ptree-test-iter ()
  (let* ((root '("test" nil zero (generic . 42) "one" (number . 9)
                 "two" (currency . 3)))
         (iter (ptree-iter root)))
    (should (eq (cadr iter) root))
    (should (equal iter (list nil root nil nil)))))

(ert-deftest ptree-test-iter-node ()
  (let* ((root '("test" nil zero (generic . 42) "one" (number . 9)
                 "two" (currency . 3)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-node iter) root))))

(ert-deftest ptree-test-iter-category-p ()
  (let* ((root '(generic . 42))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-category-p iter) nil)))
  (let* ((root '(nil 0 (generic . 42)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-category-p iter) t))))

(ert-deftest ptree-test-iter-property-p ()
  (let* ((root '(generic . 42))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-property-p iter) t)))
  (let* ((root '(nil 0 (generic . 42)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-property-p iter) nil))))

(ert-deftest ptree-test-iter-value ()
  (let* ((root '(generic . 42))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-value iter) 42)))
  (let* ((root '(nil 0 (generic . 42)))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-value iter) :type 'ptree-not-a-property)))

(ert-deftest ptree-test-iter-type ()
  (let* ((root '(generic . 42))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-type iter) 'generic)))
  (let* ((root '(nil 0 (generic . 42)))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-type iter) :type 'ptree-not-a-property)))

(ert-deftest ptree-test-iter-tag ()
  (let* ((root '(nil 0 (nil) test (generic . 42)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-tag iter) nil))
    (ptree-iter-down iter)
    (should (eq (ptree-iter-tag iter) 0))
    (ptree-iter-next iter)
    (should (eq (ptree-iter-tag iter) 'test))))

(ert-deftest ptree-test-iter-path ()
  (let* ((root '(nil 0 (nil zero (nil "zero" (generic . 42)))))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-path iter) nil))
    (ptree-iter-down iter)
    (should (equal (ptree-iter-path iter) '(0)))
    (ptree-iter-down iter)
    (should (equal (ptree-iter-path iter) '(0 zero)))
    (ptree-iter-down iter)
    (should (equal (ptree-iter-path iter) '(0 zero "zero")))))

(ert-deftest ptree-test-iter-has-child ()
  (let* ((root '(nil))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-has-child iter) nil)))
  (let* ((root '(generic . 42))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-has-child iter) nil)))
  (let* ((root '(nil 0 (nil)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-has-child iter) t))))

(ert-deftest ptree-test-iter-has-parent ()
  (let* ((root '(nil 0 (nil)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-has-parent iter) nil))
    (ptree-iter-down iter)
    (should (eq (ptree-iter-has-parent iter) t))))

(ert-deftest ptree-test-iter-has-next ()
  (let* ((root '(nil 0 (nil) 1 (generic . 42)))
         (iter (ptree-iter root)))
    (ptree-iter-down iter)
    (should (eq (ptree-iter-has-next iter) t))
    (ptree-iter-next iter)
    (should (eq (ptree-iter-has-next iter) nil))))

(ert-deftest ptree-test-iter-has-previous ()
  (let* ((root '(nil 0 (nil) 1 (generic . 42)))
         (iter (ptree-iter root)))
    (ptree-iter-down iter)
    (should (eq (ptree-iter-has-previous iter) nil))
    (ptree-iter-next iter)
    (should (eq (ptree-iter-has-previous iter) t))))

(ert-deftest ptree-test-iter-down ()
  (let* ((root '(generic . 42))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter) 1))
    (should (equal iter '(nil (generic . 42) nil nil ))))
  (let* ((root '(nil))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter) 1))
    (should (equal iter '(nil (nil) nil nil))))
  (let* ((root '(nil 0 (generic . 42) test (nil)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter) 0))
    (should (equal iter '(0 (generic . 42) ((nil (nil 0 (generic .  42)
                                                      test (nil)) nil nil))
                            (test (nil))))))
  (let* ((root '(nil 0 (generic . 42) test (nil)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter 3) 2))
    (should (equal iter '(0 (generic . 42) ((nil (nil 0 (generic . 42)
                                                      test (nil)) nil nil))
                            (test (nil))))))
  (let* ((root '(nil 0 (nil 1 (generic . 42))))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter 2) 0))
    (should (equal iter '(1
                          (generic . 42)
                          ((0
                            (nil 1 (generic . 42))
                            ((nil (nil 0 (nil 1 (generic . 42))) nil nil))
                            nil)
                           (nil
                            (nil 0 (nil 1 (generic . 42)))
                            nil
                            nil))
                          nil))))
  (let* ((root '(nil 0 (nil 1 (generic . 42))))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter 5) 3))
    (should (equal iter '(1
                          (generic . 42)
                          ((0
                            (nil 1 (generic . 42))
                            ((nil (nil 0 (nil 1 (generic . 42))) nil nil))
                            nil)
                           (nil
                            (nil 0 (nil 1 (generic . 42)))
                            nil
                            nil))
                          nil))))
  (let* ((root '(nil 1 (nil 10 (nil 100 (generic . 42)) 11 (nil))
                     2 (number . 9)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-down iter 2) 0))
    (should (equal iter '(10
                          (nil 100 (generic . 42))
                          ((1
                            (nil 10 (nil 100 (generic . 42)) 11 (nil))
                            ((nil
                              (nil
                               1
                               (nil 10 (nil 100 (generic . 42)) 11 (nil))
                               2
                               (number . 9))
                              nil
                              nil))
                            (2 (number . 9)))
                           (nil
                            (nil
                             1
                             (nil 10 (nil 100 (generic . 42)) 11 (nil))
                             2
                             (number . 9))
                            nil
                            nil))
                          (11 (nil)))))))

(ert-deftest ptree-test-iter-up ()
  (let* ((root '(nil))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-up iter) 1))
    (should (equal iter '(nil (nil) nil nil))))
  (let* ((iter (list
                0
                (cons 'generic 42)
                (list (list nil
                            (list nil 0 (cons 'generic 42) 'test (list nil))
                            nil
                            nil))
                (list 'test (list nil)))))
    (should (eq (ptree-iter-up iter) 0))
    (should (equal iter '(nil (nil 0 (generic . 42) test (nil)) nil nil))))
  (let* ((iter (list
                0
                (cons 'generic 42)
                (list (list nil
                            (list nil 0 (cons 'generic 42) 'test (list nil))
                            nil
                            nil))
                (list 'test (list nil)))))
    (should (eq (ptree-iter-up iter 3) 2))
    (should (equal iter '(nil (nil 0 (generic . 42) test (nil)) nil nil))))
  (let* ((root '(nil 1 (nil 11 (nil 111 (generic . 42) 112 (string . "aaa"))
                            12 (nil 121 (number . 9)))
                     2 (string . "test")))
         (iter (ptree-iter root))
         (iter-0 iter)
         (iter-1 nil)
         (iter-2 nil))
    (ptree-iter-down iter)
    (setq iter-1 iter)
    (ptree-iter-down iter)
    (setq iter-2 iter)
    (ptree-iter-down iter)
    (ptree-iter-up iter)
    (should (equal iter iter-2))
    (ptree-iter-up iter)
    (should (equal iter iter-1))
    (ptree-iter-up iter)
    (should (equal iter iter-0))))

(ert-deftest ptree-test-iter-next ()
  (let* ((root '(nil))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-next iter) 1))
    (should (equal iter '(nil (nil) nil nil))))
  (let* ((root '(nil 0 (nil) 1 (generic . 42) 2 (string . "aaa")))
         (iter (ptree-iter root)))
    (ptree-iter-down iter)
    (should (eq (ptree-iter-next iter) 0))
    (should (equal iter '(1
                          (generic . 42)
                          ((nil
                            (nil 0 (nil) 1 (generic . 42) 2 (string . "aaa"))
                            nil
                            nil))
                          (2 (string . "aaa"))
                          (nil)
                          0)))
    (should (eq (ptree-iter-next iter) 0))
    (should (equal iter '(2
                          (string . "aaa")
                          ((nil
                            (nil 0 (nil) 1 (generic . 42) 2 (string . "aaa"))
                            nil
                            nil))
                          nil
                          (generic . 42)
                          1
                          (nil)
                          0)))
    (should (eq (ptree-iter-next iter 3) 3))
    (should (equal iter '(2
                          (string . "aaa")
                          ((nil
                            (nil 0 (nil) 1 (generic . 42) 2 (string . "aaa"))
                            nil
                            nil))
                          nil
                          (generic . 42)
                          1
                          (nil)
                          0))))
  (let* ((root '(nil 0 (nil 1 (nil 11 (nil))
                            2 (nil 21 (generic . 42))
                            3 (nil 31 (string . "aaa")))))
         (iter (ptree-iter root)))
    (ptree-iter-down iter 2)
    (should (eq (ptree-iter-next iter 2) 0))
    (should (equal iter '(3
                          (nil 31 (string . "aaa"))
                          ((0
                            (nil 1 (nil 11 (nil))
                                 2 (nil 21 (generic . 42))
                                 3 (nil 31 (string . "aaa")))
                            ((nil
                              (nil 0 (nil 1 (nil 11 (nil))
                                          2 (nil 21 (generic . 42))
                                          3 (nil 31 (string . "aaa"))))
                              nil
                              nil))
                            nil)
                           (nil
                            (nil
                             0 (nil 1 (nil 11 (nil))
                                    2 (nil 21 (generic . 42))
                                    3 (nil 31 (string . "aaa"))))
                            nil
                            nil))
                          nil
                          (nil 21 (generic . 42)) 2
                          (nil 11 (nil)) 1)))))

(ert-deftest ptree-test-iter-previous ()
  (let* ((root '(nil))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-previous iter) 1))
    (should (equal iter '(nil (nil) nil nil))))
  (let* ((root '(nil 0 (nil) 1 (generic . 42) 2 (string . "aaa")))
         (iter (ptree-iter root)))
    (ptree-iter-down iter)
    (ptree-iter-next iter 2)
    (should (eq (ptree-iter-previous iter) 0))
    (should (equal iter '(1
                          (generic . 42)
                          ((nil
                            (nil 0 (nil) 1 (generic . 42) 2 (string . "aaa"))
                            nil
                            nil))
                          (2 (string . "aaa"))
                          (nil)
                          0)))
    (should (eq (ptree-iter-previous iter) 0))
    (should (equal iter '(0
                          (nil)
                          ((nil
                            (nil 0 (nil) 1 (generic . 42) 2 (string . "aaa"))
                            nil
                            nil))
                          (1 (generic . 42) 2 (string . "aaa")))))
    (should (eq (ptree-iter-previous iter 3) 3))
    (should (equal iter '(0
                          (nil)
                          ((nil
                            (nil 0 (nil) 1 (generic . 42) 2 (string . "aaa"))
                            nil
                            nil))
                          (1 (generic . 42) 2 (string . "aaa"))))))
  (let* ((root '(nil 0 (nil 1 (nil 11 (nil))
                            2 (nil 21 (generic . 42))
                            3 (nil 31 (string . "aaa")))))
         (iter (ptree-iter root)))
    (ptree-iter-down iter 2)
    (ptree-iter-next iter 2)
    (should (eq (ptree-iter-previous iter) 0))
    (should (equal iter '(2
                          (nil 21 (generic . 42))
                          ((0
                            (nil 1 (nil 11 (nil))
                                 2 (nil 21 (generic . 42))
                                 3 (nil 31 (string . "aaa")))
                            ((nil
                              (nil 0 (nil 1 (nil 11 (nil))
                                          2 (nil 21 (generic . 42))
                                          3 (nil 31 (string . "aaa"))))
                              nil
                              nil))
                            nil)
                           (nil
                            (nil
                             0 (nil 1 (nil 11 (nil))
                                    2 (nil 21 (generic . 42))
                                    3 (nil 31 (string . "aaa"))))
                            nil
                            nil))
                          (3 (nil 31 (string . "aaa")))
                          (nil 11 (nil)) 1)))
    (should (eq (ptree-iter-previous iter) 0))
    (should (equal iter '(1
                          (nil 11 (nil))
                          ((0
                            (nil 1 (nil 11 (nil))
                                 2 (nil 21 (generic . 42))
                                 3 (nil 31 (string . "aaa")))
                            ((nil
                              (nil 0 (nil 1 (nil 11 (nil))
                                          2 (nil 21 (generic . 42))
                                          3 (nil 31 (string . "aaa"))))
                              nil
                              nil))
                            nil)
                           (nil
                            (nil
                             0 (nil 1 (nil 11 (nil))
                                    2 (nil 21 (generic . 42))
                                    3 (nil 31 (string . "aaa"))))
                            nil
                            nil))
                          (2 (nil 21 (generic . 42))
                             3 (nil 31 (string . "aaa"))))))))

(ert-deftest ptree-test-move-with-tag ()
  (let* ((root '(nil 0 (nil) test (generic . 42) "test" (number . 9)))
         (iter (ptree-iter root)))
    (should (eq (ptree-iter-move-with-tag iter 'taste) nil))
    (should (equal iter '(nil
                          (nil
                           0 (nil)
                           test (generic . 42)
                           "test" (number . 9))
                          nil nil)))
    (should (eq (ptree-iter-move-with-tag iter 0) t))
    (should (equal iter '(0
                          (nil)
                          ((nil
                            (nil
                             0 (nil)
                             test (generic . 42)
                             "test" (number . 9))
                            nil nil))
                          (test (generic . 42) "test" (number . 9)))))
    (ptree-iter-up iter)
    (should (eq (ptree-iter-move-with-tag iter 'test) t))
    (should (equal iter '(test
                          (generic . 42)
                          ((nil
                            (nil
                             0 (nil)
                             test (generic . 42)
                             "test" (number . 9))
                            nil nil))
                          ("test" (number . 9))
                          (nil) 0)))
    (ptree-iter-up iter)
    (should (eq (ptree-iter-move-with-tag iter "test") t))
    (should (equal iter '("test"
                          (number . 9)
                          ((nil
                            (nil
                             0 (nil)
                             test (generic . 42)
                             "test" (number . 9))
                            nil nil))
                          nil
                          (generic . 42) test (nil) 0)))))

(ert-deftest ptree-test-add-property ()
  (let* ((root (cons 'generic 42))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-add-property iter "test" 9 'number)
                  :type 'ptree-not-a-category))
  (let* ((root (list nil 0 (list nil) "test" (cons 'generic 42)))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-add-property iter "test" 9 'number)
                  :type 'ptree-node-already-existing)
    (should (equal (ptree-iter-add-property iter 'test 9 'number)
                   '(number . 9)))
    (should (equal iter '(nil
                          (nil 0 (nil) test (number . 9) "test" (generic . 42))
                          nil
                          nil)))))

(ert-deftest ptree-test-add-category ()
  (let* ((root (cons 'generic 42))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-add-category iter "test")
                  :type 'ptree-not-a-category))
  (let* ((root (list nil 0 (list nil) "test" (cons 'generic 42)))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-add-category iter "test")
                  :type 'ptree-node-already-existing)
    (should (equal (ptree-iter-add-category iter 'test)
                   '(nil)))
    (should (equal iter '(nil
                          (nil 0 (nil) test (nil) "test" (generic . 42))
                          nil
                          nil)))))

(ert-deftest ptree-test-add-category-and-move ()
  (let* ((root (cons 'generic 42))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-add-category-and-move iter "test")
                  :type 'ptree-not-a-category))
  (let* ((root (list nil 0 (list nil) "test" (cons 'generic 42)))
         (iter (ptree-iter root)))
    (should-error (ptree-iter-add-category-and-move iter "test")
                  :type 'ptree-node-already-existing)
    (ptree-iter-add-category-and-move iter 'test)
    (should (equal iter '(test (nil)
                               ((nil
                                 (nil
                                  0 (nil)
                                  test (nil)
                                  "test" (generic . 42))
                                 nil nil))
                               ("test" (generic . 42))
                               (nil) 0)))))

(ert-deftest ptree-test-iter-delete-node ()
  (let* ((root (list nil
                     0 (list nil)
                     'test (list nil 1 (list nil) 'test2 (cons 'generic 42))
                     "test" (cons 'number 9)))
         (iter (ptree-iter root)))
    (ptree-iter-down iter)
    (ptree-iter-next iter)
    (ptree-iter-down iter)
    (should (eq (ptree-iter-delete-node iter) t))
    (should (equal iter '(test
                          (nil test2 (generic . 42))
                          ((nil
                            (nil
                             0 (nil)
                             test (nil test2 (generic . 42))
                             "test" (number . 9))
                            nil nil))
                          ("test" (number . 9))
                          (nil) 0)))
    (should (eq (ptree-iter-delete-node iter) t))
    (should (equal iter '(nil (nil 0 (nil) "test" (number . 9)) nil nil)))
    (should (eq (ptree-iter-delete-node iter) nil))))

(ert-deftest ptree-test-to-string ()
  (let ((root '(nil
                current-users (generic . 12)
                desktop (nil
                         background-color (color . "black"))
                processes (nil
                           by-id (nil
                                  0 (string . "startx")
                                  1 (string . "dbus-launch")
                                  2 (string . "X")))
                windows (nil
                         "emacs" (nil
                                  height (number . 800)
                                  pos-x (number . 600)
                                  pos-y (number . 0)
                                  width (number . 1000))
                         "xterm" (nil
                                  height (number . 200)
                                  pos-x (number . 50)
                                  pos-y (number . 100)
                                  width (number . 500))))))
    (message "%s" (ptree-to-string root))
    (should (string= (ptree-to-string root) "current-users: 12
desktop
    background-color: black
processes
    by-id
        0: startx
        1: dbus-launch
        2: X
windows
    \"emacs\"
        height: 800
        pos-x: 600
        pos-y: 0
        width: 1000
    \"xterm\"
        height: 200
        pos-x: 50
        pos-y: 100
        width: 500
"))))

;; Test launcher

(defun ptree-run-tests ()
  (interactive)
  (ert-run-tests-interactively "ptree-test-"))
