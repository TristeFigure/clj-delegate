(ns clj-delegate.reflect-test
  (:refer-clojure :exclude [methods])
  (:require [clojure.test :refer :all]
            [clj-delegate.reflect :refer :all]))

(defrecord R [a])

(def expected-ancestors
  (list
    java.lang.Object
    java.util.Map
    clojure.lang.ILookup
    clojure.lang.IHashEq
    clojure.lang.IObj
    clojure.lang.IKeywordLookup
    clojure.lang.IPersistentMap
    java.io.Serializable
    clojure.lang.IRecord
    clojure.lang.IMeta
    clojure.lang.Associative
    clojure.lang.Counted
    java.lang.Iterable
    clojure.lang.IPersistentCollection
    clojure.lang.Seqable))

(deftest test-base-ancestors
  (testing "like ancestors ..."
    (is (= (sort-by (memfn getName) (base-ancestors R))
           (sort-by (memfn getName) (ancestors R)))))
  (testing "... but in breadth-first order"
    (is (= (base-ancestors R)
           expected-ancestors)))
  (testing "it does not contains the object's class"
    (is (false? (contains? (set expected-ancestors)
                           R)))))

(defprotocol Prot
  (metxxx [this]))

(defrecord Rec []
  Prot
  (metxxx [this] :yes))

(deftype Typ []
  Prot
  (metxxx [this] :no))

(deftest test-protocols
  (binding [*ns* (find-ns 'clj-delegate.reflect-test)]
    (are [x] (= (-> x protocols first :on)
                'clj_delegate.reflect_test.Prot)
         Rec
         'Rec)))
