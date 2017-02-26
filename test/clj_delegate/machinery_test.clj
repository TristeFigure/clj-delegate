(ns clj-delegate.machinery-test
  (:require [clojure.test :refer :all]
            [clj-delegate.machinery :refer :all]
            [clj-delegate.fixtures])
  (:import [clj_delegate.fixtures Record]))

(deftest test-emit-delegate-fields-accessors
  (binding [*ns* (find-ns 'clj-delegate.machinery-test)]
    (is (= '(RecordFields
             (a [this] (.a (.delegate this)))
             (b [this] (.b (.delegate this)))
             (c [this] (.c (.delegate this))))
           (emit-delegate-fields-accessors 'Record)))))
