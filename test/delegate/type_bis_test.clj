(ns delegate.type-bis-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [delegate.type-bis :refer :all
             :reload true]
            [delegate.reflect-bis :as reflect]))


(def to-local-format           @#'delegate.type-bis/to-local-format)
(def to-deftype-specs          @#'delegate.type-bis/to-deftype-specs)
(def merge-specs               @#'delegate.type-bis/merge-specs)
(def emit-delegate-fields-accessors
  @#'delegate.type-bis/emit-delegate-fields-accessors)
(def emit-delegate-fields-protocol
  @#'delegate.type-bis/emit-delegate-fields-protocol)

(defprotocol Protocol
  (method [this a]))

(defprotocol ProtocolA
  (method-a [this a]
            [this a b])
  (method-b [this b]))

(defprotocol ProtocolB
  (method-c [this c]))

(definterface Interface
  (m [a])
  (m [a b]))

(defrecord Record [a b c]
  Interface
  (m [this a]   :m1)
  (m [this a b] :m2)
  
  Protocol
  (method [this a] :original)
  
  ProtocolA
  (method-a [this a] :original-a)
  (method-a [this a b] :original-aa)
  (method-b [this b] :original-b))

(pprint
  (macroexpand-1
    '(defrecord Record [a b c]
      Interface
      (m [this a]   :m1)
      (m [this a b] :m2)
      
      Protocol
      (method [this a] :original)
      
      ProtocolA
      (method-a [this a] :original-a)
      (method-a [this a b] :original-aa)
      (method-b [this b] :original-b))))

(newline)(newline)

(deftype Type [a b c]
  Protocol
  (method [this a] :original)
  
  ProtocolA
  (method-a [this a] :original-a)
  (method-a [this a b] :original-aa)
  (method-b [this b] :original-b))

; (pprint
;   (-> '(deftype Type [a b c]
;         Protocol
;         (method [this a] :original)
        
;         ProtocolA
;         (method-a [this a] :original-a)
;         (method-a [this a b] :original-aa)
;         (method-b [this b] :original-b))
;       macroexpand-1))


(deftest test-to-local-format
  (testing "convert from list of clojure.reflect.Methods to local format"
    (is (= (->> (reflect/methods Record)
                (sort-by #(:name %))
                reverse
                (filter #(-> % :protocol
                             (= 'delegate.type_bis_test.Interface)))
                (to-local-format 'Record))
           '{[m [aa]] {:name m,
                       :declaring-class delegate.type_bis_test.Record,
                       :protocol delegate.type_bis_test.Interface,
                       :params [aa ab],
                       :this this,
                       :body ((.m (.delegate this) aa ab))},
             [m []]   {:name m,
                       :declaring-class delegate.type_bis_test.Record,
                       :protocol delegate.type_bis_test.Interface,
                       :params [aa],
                       :this this,
                       :body ((.m (.delegate this) aa))}})))
  (testing "convert defrecord/deftype quoted specs code to local format"
    (is (= (to-local-format 'Record
                            '(Protocol
                              (method [this a] :abc)))
           '{[method []] {:name method,
                          :params [a],
                          :this this,
                          :body (:abc),
                          :declaring-class Record,
                          :protocol delegate.type-bis-test/Protocol}})))
  (testing "edge cases"
    (is (= ( 'R '())))))

(deftest test-to-deftype-specs
  (is (= (to-deftype-specs
           'Record
           '{method {:name method
                     :params [a]
                     :this this
                     :body (:abc)
                     :declaring-class Record
                     :protocol Protocol}})
         '(Protocol (method [this a] :abc))))
  (testing "edge cases"
    (is (= (to-deftype-specs 'R {})
           '()))))

(deftest test-format-round-trip
  (let [initial '(delegate.type-bis-test/Protocol (method [this a] :abc))
        loc   (partial to-local-format 'R)
        defty (partial to-deftype-specs 'R)]
    (testing "local -> deftype -> local"
      (is (= (-> initial
                 loc
                 defty)
             initial)))
    (testing "local -> local"
      (is (= (-> initial loc loc)
             (-> initial loc))))
    (testing "deftype -> deftype"
      (is (= (-> initial loc defty defty)
             (-> initial loc defty))))))

(deftest test-merge-specs
  (is (= (merge-specs 'R
                      (to-local-format 'R '(ProtocolA
                                            (method-a [this a]   :abc)
                                            (method-b [this b]   :def)))
                      (to-local-format 'R '(ProtocolA
                                            (method-b [this b]   :uvw)
                                            ProtocolB
                                            (method-c [this c]   :xyz)
                                            (method-c [this c x] :cx))))
         '(delegate.type-bis-test/ProtocolB
           (method-c [this c] :xyz)
           (method-c [this c x] :cx)
           delegate.type-bis-test/ProtocolA
           (method-a [this a] :abc)
           (method-b [this b] :uvw)))))

(deftest test-emit-delegate-fields-accessors
  (is (= (emit-delegate-fields-accessors 'Record)
         '(RecordFields
           (a [this] (.a (.delegate this)))
           (b [this] (.b (.delegate this)))
           (c [this] (.c (.delegate this)))))))

(deftest test-emit-delegate-fields-protocol
  (is (= (emit-delegate-fields-protocol 'Record)
         '(clojure.core/defprotocol RecordFields
           (a [this])
           (b [this])
           (c [this])))))

(defdelegate DelegateType Type [d]
  Protocol
  (method [this a] :overriden)
  
  ProtocolA
  (method-a [this a] :overriden-a)
  
  ProtocolB
  (method-c [this c] :value)
  
  Object
  (toString [this] "abc"))

(deftest test-delegate-a-type
  (let [deleg (DelegateType. (Type. 1 2 3) 4)]
    (testing "methods"
      (testing "multiple interfaces or protocols"
        (is (= :value (.method-c deleg :_))))
      (testing "delegate-local methods override base methods"
        (is (= :overriden (.method deleg :_)))
        (is (= :overriden-a (.method-a deleg :_)))
        (is (= :original-b (.method-b deleg :_))))
      (testing "delegating Object methods"
        (is (= "abc" (.toString deleg))))
      (testing "fields"
        (testing "delegated fields"
          (is (= 1 (.a deleg)))
          (is (= 2 (.b deleg)))
          (is (= 3 (.c deleg))))
        (testing "delegate-local fields"
          (is (= 4 (.d deleg))))))))

#_(pprint
  (macroexpand-1
    '(defdelegate DelegateRecord Record [d]
      Protocol
      (method [this a] :overriden)
      
      ProtocolA
      (method-a [this a] :overriden-a)
      
      ProtocolB
      (method-c [this c] :value)
      
      Object
      (toString [this] "abc"))))

(defdelegate DelegateRecord Record [d]
  ; Protocol
  ; (method [this a] :overriden)
  
  ProtocolA
  (method-a [this a] :overriden-a)
  
  ProtocolB
  (method-c [this c] :value)
  
  Object
  (toString [this] "abc"))

(deftest test-delegate-a-record
  #_(let [deleg (DelegateRecord. (Record. 1 2 3) 4)]
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
 ;; TODO: test overriding object methods (we dah to strip java.lang.Object off
 ;; emit-deftype's interfaces list)
