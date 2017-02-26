(ns clj-delegate.transforms-test
  (:refer-clojure :exclude [methods])
  (:require [clojure.test :refer :all]
            [clj-delegate.specs :refer [to-local-format to-deftype-specs]]
            [clj-delegate.transforms :refer :all]))

(deftest test-apply-transforms
  (let [methods (to-local-format
                  'Record
                  '(Protocol
                    (method [this a] :m)
                    ProtocolA
                    (method-a [this a] :a)
                    (method-a [this a b] :ab)
                    (method-b [this b] :b)
                    Interface
                    (m [this a] :iface)
                    Interface2
                    (mm [this a] :iface2)
                    Interface3
                    (mmm [this a] :iface3)
                    Interface4
                    (mmmm [this a] :iface4)
                    Object
                    (toString [this] "abc")))
        transforms [['Protocol
                     '(method [this] :xyz)]
                    
                    [['[ProtocolA method-a [a]]
                      (fn [m]
                        (and (= (:protocol m) 'ProtocolA)
                             (= (:name m)     'method-b)))]
                     (fn [m]
                       (update-in m [:body] (fn [body]
                                              `((do (~'println "ahah")
                                                    ~@body)))))]
                    [Object
                     (fn [_m]
                       '(toString [this] (println "it works")))]
                    ['Interface nil]
                    ['Interface2 false]
                    ['Interface3 (constantly nil)]
                    ['Interface4 (constantly nil)]]
        result (apply-transforms 'Record
                                 transforms
                                 methods)]
    (is (= '(Protocol
             (method [this] :xyz)
             ProtocolA
             (method-b [this b] (do (println "ahah") :b))
             (method-a [this a b] :ab)
             (method-a [this a] (do (println "ahah") :a))
             java.lang.Object
             (toString [this] (println "it works")))
           result))))
