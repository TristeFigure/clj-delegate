(ns clj-delegate.core-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clj-delegate.core :refer :all]
            [clj-delegate.fixtures :refer [Protocol ProtocolA ProtocolB]])
  (:import [clj_delegate.fixtures Type Record Interface]))

(defdelegate DelegateType Type [z]
  Protocol
  (method [this a] :overriden)
  
  ProtocolA
  (method-a [this a] :overriden-a)
  
  ProtocolB
  (method-c [this c] :value))

(deftest test-delegate-a-type
  (let [deleg (DelegateType. (Type. 1 2 3) 4)]
    (testing "methods"
      (testing "multiple interfaces or protocols"
        (is (= :value (.method-c deleg :_))))
      (testing "delegate-local methods override base methods"
        (is (= :overriden (.method deleg :_)))
        (is (= :overriden-a (.method-a deleg :_)))
        (is (= :original-b (.method-b deleg :_))))
      (testing "original Object methods"
        (is (= "xyz" (.toString deleg))))
      (testing "fields"
        (testing "delegated fields"
          (is (= 1 (.w deleg)))
          (is (= 2 (.x deleg)))
          (is (= 3 (.y deleg))))
        (testing "delegate-local fields"
          (is (= 4 (.z deleg))))))))


(defdelegate DelegateRecord Record [d]
  Interface
  (m [this a] :overriden-m1)
  
  ProtocolA
  (method-a [this a] :overriden-a)
  
  Object
  (toString [this] "abc"))

(deftest test-delegate-a-record
  (let [deleg (DelegateRecord. (Record. 1 2 3) 4)]
    (testing "methods"
      (testing "interfaces"
        (testing "overriden"
          (is (= :overriden-m1 (.m deleg :x))))
        (testing "original"
          (is (= :original-m2  (.m deleg :x :x)))))
      (testing "protocols"
        (testing "overriden"
          (is (= :overriden-a (.method-a deleg :x))))
        (testing "original"
          (is (= :original (.method deleg :x)))
          (is (= :original-aa (.method-a deleg :x :x)))))
      (testing "delegating Object methods"
        (is (= "abc" (.toString deleg)))))
    (testing "fields"
      (testing "delegated fields"
        (is (= 1 (.a deleg)))
        (is (= 2 (.b deleg))))
      (testing "delegate-local fields"
        (is (= 3 (.c deleg)))))
    (testing "it's an associative datastructure"
      (testing "original"
        (is (= (:c deleg) 3)))
      (testing "added"
        (is (= (:d deleg) 4))))))
