(ns clj-delegate.derive-test
  (:require [clojure.test :refer :all]
            [clj-delegate.derive :refer :all])
  (:import [clj_delegate.fixtures Record ParentRecord ChildRecord]))
  

(deftest test-derive-delegate
  (binding [*ns* (find-ns 'clj-delegate.machinery-test)]
    (derive-delegate ChildRecord ParentRecord)
    (testing "delegates?"
      (is (= true
             (delegates? ChildRecord ParentRecord)))
      (is (= false
             (delegates? ParentRecord ChildRecord)))
      (is (= false
             (delegates? ChildRecord Object))))
    (testing "isa?"
      (is (= true
             (isa? ChildRecord ParentRecord)))
      (is (= false
             (isa? ParentRecord ChildRecord)))
      (is (= true
             (isa? ChildRecord Object))))))
