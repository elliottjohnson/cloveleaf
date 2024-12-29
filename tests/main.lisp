(defpackage cloveleaf/tests/main
  (:use :cl
        :cloveleaf
        :rove))
(in-package :cloveleaf/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cloveleaf)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
