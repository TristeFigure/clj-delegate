(ns clj-delegate.specs-test
  (:require [clojure.test :refer :all]
            [clj-delegate.specs :refer :all]
            [clj-delegate.reflect :as reflect]
            [clj-delegate.fixtures
             :refer [TagProtocol Protocol ProtocolA ProtocolB]])
  (:import [clj_delegate.fixtures Record]))

(deftest test-to-local-format
  ;; We need this so that fully-qualify finds its way through this file's
  ;; namespace. By default, when `lein test` is used, the value of *ns* is the
  ;; 'user' namespace.
  (binding [*ns* (find-ns 'clj-delegate.specs-test)]
    (testing "convert from list of clojure.reflect.Methods to local format"
      (is (= '{[m [aa]] {:name m,
                         :declaring-class clj_delegate.fixtures.Record,
                         :protocol clj_delegate.fixtures.Interface,
                         :params [aa ab],
                         :this this,
                         :body ((.m (.delegate this) aa ab))},
               [m []]   {:name m,
                         :declaring-class clj_delegate.fixtures.Record,
                         :protocol clj_delegate.fixtures.Interface,
                         :params [aa],
                         :this this,
                         :body ((.m (.delegate this) aa))}}
             (->> (reflect/methods Record)
                  (sort-by #(:name %))
                  reverse
                  (filter #(-> % :protocol
                               (= 'clj_delegate.fixtures.Interface)))
                  (to-local-format 'Record)))))
    (testing "convert defrecord/deftype quoted specs code to local format"
      (is (= '{[nil []]
               {:declaring-class Record,
                :protocol clj-delegate.fixtures/TagProtocol,
                :no-method true},
               [method []]
               {:name method,
                :params [a],
                :this this,
                :body (:abc),
                :declaring-class Record,
                :protocol clj-delegate.fixtures/Protocol}}
             (to-local-format 'Record
                              '(TagProtocol
                                Protocol
                                (method [this a] :abc))))))
    (testing "edge cases"
      (is (= (to-local-format 'R '())
             {})))))  

(deftest test-to-deftype-specs
  (is (= '(Protocol (method [this a] :abc))
         (to-deftype-specs
           'Record
           '{method {:name method
                     :params [a]
                     :this this
                     :body (:abc)
                     :declaring-class Record
                     :protocol Protocol}})))
  (testing "edge cases"
    (is (= '()
           (to-deftype-specs 'R {})))))

(deftest test-format-round-trip
  (let [initial '(clj-delegate.fixtures/Protocol (method [this a] :abc))
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
  (binding [*ns* (find-ns 'clj-delegate.specs-test)]
    (is (= '(clj-delegate.fixtures/TagProtocol
             clj-delegate.fixtures/ProtocolA
             (method-a [this a] :abc)
             (method-b [this b] :uvw)
             clj-delegate.fixtures/ProtocolB
             (method-c [this c] :xyz)
             (method-c [this c x] :cx))
           (merge-specs 'R
                        (to-local-format 'R '(TagProtocol
                                              ProtocolA
                                              (method-a [this a]   :abc)
                                              (method-b [this b]   :def)))
                        (to-local-format 'R '(ProtocolA
                                              (method-b [this b]   :uvw)
                                              ProtocolB
                                              (method-c [this c]   :xyz)
                                              (method-c [this c x] :cx))))))))
