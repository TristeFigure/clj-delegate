(ns delegate.type-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [delegate.type :refer :all
             :reload true]))

(defprotocol AProtocol
  (m1 [this])
  (m2 [this arg]))

(defprotocol AnotherProtocol
  (m3 [this]))

(deftype AType [a b]
  AProtocol
  (m1 [this] :m1)
  (m2 [this arg] arg)

  AnotherProtocol
  (m3 [this] :m3)

  Object
  (toString [this] "abc"))

(defrecord ARecord [a b]
  AProtocol
  (m1 [this] :m1)
  (m2 [this arg] arg)

  AnotherProtocol
  (m3 [this] :m3)

  Object
  (toString [this] "abc"))

(defdelegate DelegateType AType [c]
  AProtocol
  (m2 [this arg] :m2))

(pprint
  (macroexpand-1
    (macroexpand-1
      '(defdelegate DelegateRecord ARecord [c]
        AProtocol
        (m2 [this arg] :m2)

        clojure.lang.IPersistentCollection
        (cons [this a]
              (println "-------------------cons------------")
              this
              #_(.cons (clojure.core/merge (into {} this)
                                           (into {} (.delegate this)))
                       a))))))

#_(defdelegate DelegateRecord ARecord [c]
  AProtocol
  (m2 [this arg] :m2)

  clojure.lang.IPersistentCollection
  (cons [this a]
    (println "-------------------cons------------")
    this
    #_(.cons (clojure.core/merge (into {} this)
                               (into {} (.delegate this)))
           a)))

(defn record->map
  [record]
  (let [f #(if (record? %) (record->map %) %)
        ks (keys record)
        vs (map f (vals record))]
    (zipmap ks vs)))

(do
 (clojure.core/defprotocol ARecordFields (a [this]) (b [this]))
 (deftype
  DelegateRecord
  [delegate c]
  ARecordFields
  (a [this] (.a (.delegate this)))
  (b [this] (.b (.delegate this)))
  delegate.type_test.AProtocol
  (m1 [this] (.m1 (.delegate this)))
  (m2 [this arg] :m2)
  delegate.type_test.AnotherProtocol
  (m3 [this] (.m3 (.delegate this)))
  clojure.lang.IKeywordLookup
  (getLookupThunk [this a] (.getLookupThunk (.delegate this) a))
  clojure.lang.Associative
  (entryAt
   [this a]
   (.entryAt (clojure.core/merge (.delegate this) this) a))
  java.util.Map
  (computeIfAbsent
   [this a b]
   (.computeIfAbsent (clojure.core/merge (.delegate this) this) a b))
  (put
   [this a b]
   (.put (clojure.core/merge (.delegate this) this) a b))
  (putIfAbsent
   [this a b]
   (.putIfAbsent (clojure.core/merge (.delegate this) this) a b))
  (keySet [this] (.keySet (clojure.core/merge (.delegate this) this)))
  (getOrDefault
   [this a b]
   (.getOrDefault (clojure.core/merge (.delegate this) this) a b))
  (entrySet
   [this]
   (.entrySet (clojure.core/merge (.delegate this) this)))
  (clear [this] (.clear (clojure.core/merge (.delegate this) this)))
  (containsKey
   [this a]
   (.containsKey (clojure.core/merge (.delegate this) this) a))
  (remove
   [this a b]
   (.remove (clojure.core/merge (.delegate this) this) a b))
  (get [this a] (.get (clojure.core/merge (.delegate this) this) a))
  (replace
   [this a b]
   (.replace (clojure.core/merge (.delegate this) this) a b))
  (containsValue
   [this a]
   (.containsValue (clojure.core/merge (.delegate this) this) a))
  (compute
   [this a b]
   (.compute (clojure.core/merge (.delegate this) this) a b))
  (isEmpty
   [this]
   (.isEmpty (clojure.core/merge (.delegate this) this)))
  (putAll
   [this a]
   (.putAll (clojure.core/merge (.delegate this) this) a))
  (equals
   [this a]
   (.equals (clojure.core/merge (.delegate this) this) a))
  (merge
   [this a b c]
   (map->DelegateRecord (.merge (clojure.core/merge (.delegate this) this) a b c)))
  (size [this] (.size (clojure.core/merge (.delegate this) this)))
  (replaceAll
   [this a]
   (.replaceAll (clojure.core/merge (.delegate this) this) a))
  (hashCode
   [this]
   (.hashCode (clojure.core/merge (.delegate this) this)))
  (values [this] (.values (clojure.core/merge (.delegate this) this)))
  (computeIfPresent
   [this a b]
   (.computeIfPresent (clojure.core/merge (.delegate this) this) a b))
  clojure.lang.ILookup
  (valAt
   [this a]
   (.valAt (clojure.core/merge (.delegate this) this) a))
  clojure.lang.IPersistentCollection
  (cons [this a] (.cons (clojure.core/merge (.delegate this) this) a))
  (empty [this] (.empty (clojure.core/merge (.delegate this) this)))
  (equiv
   [this a]
   (.equiv (clojure.core/merge (.delegate this) this) a))
  (count [this] (.count (clojure.core/merge (.delegate this) this)))
  clojure.lang.IHashEq
  (hasheq [this] (.hasheq (clojure.core/merge (.delegate this) this)))
  clojure.lang.IObj
  (withMeta
   [this a]
   (.withMeta (clojure.core/merge (.delegate this) this) a))
  java.util.Iterator
  (hasNext
   [this]
   (.hasNext (clojure.core/merge (.delegate this) this)))
  (forEachRemaining
   [this a]
   (.forEachRemaining (clojure.core/merge (.delegate this) this) a))
  (next [this] (.next (clojure.core/merge (.delegate this) this)))
  clojure.lang.IMeta
  (meta [this]
        {} #_(.meta (clojure.core/merge (.delegate this) this)))
  clojure.lang.IPersistentMap
  (assocEx
   [this a b]
   (.assocEx (clojure.core/merge (.delegate this) this) a b))
  (without
   [this a]
   (.without (clojure.core/merge (.delegate this) this) a))
  (assoc
   [this a b]
   (.assoc (clojure.core/merge (.delegate this) this) a b))
  clojure.lang.Seqable
  (seq [this]
       (.seq (clojure.core/merge (.delegate this) {:c c} (into {} this))))
  java.lang.Iterable
  (spliterator
   [this]
   (.spliterator (clojure.core/merge (.delegate this) this)))
  (iterator
   [this]
   (.iterator (clojure.core/merge (.delegate this) this)))
  Object
  (toString [this] "AAAAAAAAAAAAA")))

(println (merge (DelegateRecord. (ARecord. 1 2) 3) {:d 4}))
(throw (Exception. "fin"))
; (println ")--------->" (:a (ARecord. 1 2)))
; (println ").........>" (get (DelegateRecord. (ARecord. 1 2) 3)
;                             :a))
; (println ")=========>" (:a (DelegateRecord. (ARecord. 1 2) 3)))
; (throw (Exception. "fin"))

(deftest test-delegate-a-type
  (let [deleg (DelegateType. (AType. 1 2) 3)]
    (testing "methods"
      (testing "multiple interface or protocols"
        (is (= :m1 (.m1 deleg)))
        (is (= :m3 (.m3 deleg))))
      (testing "delegate-local methods override base methods"
        (is (= :m2 (.m2 deleg :not-m2))))
      ; (testing "delegating Object methods"
      ;   (is (= "abc" (.toString deleg))))
      (testing "fields"
        (testing "delegated fields"
          (is (= 1 (.a deleg)))
          (is (= 2 (.b deleg))))
        (testing "delegate-local fields"
          (is (= 3 (.c deleg))))))))

(deftest test-delegate-a-record
  (let [deleg (DelegateRecord. (ARecord. 1 2) 3)]
    (testing "methods"
      (testing "multiple interface or protocols"
        (is (= :m1 (.m1 deleg)))
        (is (= :m3 (.m3 deleg))))
      (testing "delegate-local methods override base methods"
        (is (= :m2 (.m2 deleg :not-m2))))
      ; (testing "delegating Object methods"
      ;   (is (= "abc" (.toString deleg))))
      (testing "fields"
        (testing "delegated fields"
          (is (= 1 (.a deleg)))
          (is (= 2 (.b deleg))))
        (testing "delegate-local fields"
          (is (= 3 (.c deleg)))))
      (testing "it's an associative datastructure"
        (pprint (:c deleg))))))

(run-tests)
