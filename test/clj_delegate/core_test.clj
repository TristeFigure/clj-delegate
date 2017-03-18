(ns clj-delegate.core-test
  (:require [clojure.test :refer :all]
            [clj-delegate.core :refer :all]
            [clj-delegate.fixtures :refer [Protocol ProtocolA ProtocolB
                                           map->Record]]
            [shuriken.core :refer [thrown?]])
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
    (testing "inheritance"
      (is (isa? (class deleg) Type))
      (is (isa? (class deleg) DelegateType)))
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
  (let [rec (Record. 1 2 3)
        deleg (DelegateRecord. rec 4)]
    (testing "inheritance"
      (is (isa? (class deleg) Record))
      (is (isa? (class deleg) DelegateRecord)))
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
      (testing "original fields"
        (is (= 1 (.a deleg)))
        (is (= 2 (.b deleg)))
        (is (= 3 (.c deleg))))
      (testing "added fields"
        (is (= 4 (.d deleg))))
      (testing ".delegate special field"
        (is (= rec (.delegate deleg)))))
    (testing "it's an associative datastructure"
      (testing "original"
        (is (= 1 (:a deleg))))
      (testing "added"
        (is (= 4 (:d deleg))))
      (testing "get"
        (testing "original"
          (is (= 1 (get deleg :a))))
        (testing "added"
          (is (= 4 (get deleg :d)))))
      (let [expect (fn [f]
                     (let [result (f deleg {:z 26})]
                       (is (= {:a 1 :b 2 :c 3 :d 4 :z 26}
                              (into {} result)))
                       (is (= DelegateRecord
                              (class result))))
                     (let [result (f {:z 26} deleg)]
                       (is (= {:a 1 :b 2 :c 3 :d 4 :z 26}
                              result))
                       (is (= clojure.lang.PersistentArrayMap
                              (class result)))))]
        (testing "into"
          (expect into))
        (testing "merge"
          (expect merge)))
      (let [expect (fn [f]
                     (let [result (f deleg [:z 26])]
                       (is (= {:a 1 :b 2 :c 3 :d 4 :z 26}
                              (into {} result)))
                       (is (= DelegateRecord
                              (class result)))))]
        (testing "cons"
          (expect #(.cons %1 %2)))
        (testing "conj"
          (expect conj)))
      (testing "assoc"
        (let [expect (fn [key value]
                       (let [f assoc]
                         (let [result (f deleg key value)]
                           (is (= (f (into {} deleg) key value)
                                  (into {} result)))
                           (is (= DelegateRecord (class result))))))]
          (testing "delegate keys"
            (expect :a 123))
          (testing "delegator keys"
            (expect :d 456))
          (testing "other keys"
            (expect :x 789)
            (testing "are stored on the delegate"
              (is (= {:a 1 :b 2 :c 3 :x 24}
                     (into {} (.delegate (assoc deleg :x 24)))))))))
      (testing "put"
        (is (thrown? UnsupportedOperationException
                     (.put deleg :z 0))))
      (testing "update"
        (let [deleg (assoc deleg :x 24)
              expect (fn [key]
                       (let [result (update deleg key (constantly 123))]
                         (is (= 123
                                (get result key)))))]
          (testing "original"
            (expect :a))
          (testing "added"
            (expect :d))
          (testing "external"
            (expect :x))))
      (testing "dissoc & without"
        (let [deleg (assoc deleg :x 24)
              expect (fn [key class]
                       (doseq [f [dissoc #(.without %1 %2)]]
                         (let [result (f deleg key)]
                           (is (instance? class result))
                           (is (= (dissoc (into {} deleg)
                                          key)
                                  (into {} result))))))]
          (testing "original"
            (expect :a clojure.lang.PersistentArrayMap))
          (testing "added"
            (expect :d clojure.lang.PersistentArrayMap))
          (testing "external"
            (expect :x DelegateRecord))))
      (testing "remove"
        (is (thrown? UnsupportedOperationException
                     (.remove deleg :a))))
      (testing "size & count"
        (doseq [f [#(.size %) count]]
          (is (= 4 (f deleg)))))
      (testing "empty, clear"
        (doseq [f [empty #(.clear %)]]
          (is (thrown? UnsupportedOperationException
                             (f deleg)))))
      (testing "contains?"
        (let [deleg (assoc deleg :x 24)
              expect (fn [key truth-value]
                       (is (= (contains? deleg key)
                              truth-value)))]
          (testing "original"
            (expect :a true))
          (testing "added"
            (expect :d true))
          (testing "external"
            (expect :x true))
          (testing "absent"
            (expect :z false))))
      (testing "keys, keySet"
        (let [deleg (assoc deleg :x 24)]
          (doseq [f [keys #(.keySet %)]]
            (is (= [:a :b :c :d :x]
                   (sort (f deleg)))))))
      (testing "equality"
        (is (= deleg rec)))
      (testing "meta"
        (let [m {:a :aa}]
          (= m
             (-> (with-meta deleg m)
                 .delegate
                 meta))))
      (testing "with the strict minimum"
        (is (not (thrown? Throwable
                          (defdelegate ADelegate1 Record [])))))
      (testing "with empty transforms"
        (is (not (thrown? Throwable
                          (defdelegate ADelegate2 Record [] [])))))
      (testing "using fully qualified names"
        (is (not (thrown? Throwable
                   (defdelegate ADelegate3 clj_delegate.fixtures.Record
                     []))))))))
