(ns delegate.reflect-test
  (:use clojure.pprint)
  (:refer-clojure :exclude [methods])
  (:require [clojure.test :refer :all]
            [delegate.reflect :refer :all
             :reload true]))

(defprotocol P
  (m1 [this])
  (m2 [this arg]))

(defrecord R [a b c]
  P
  (m1 [this]
    :m1)
  (m2 [this arg]
    arg)

  Object
  (toString [this]
            :string))

(deftype T [a b c]
  P
  (m1 [this]
    :m1)
  (m2 [this arg]
    arg))

(deftest get-basis-test
  (testing "with a class"
    (let [class R]
      (is (= (get-basis class)
             '[a b c]))))
  (testing "with an instance"
    (let [instance (R. 1 2 3)]
      (is (= (get-basis instance)
             '[a b c])))))

(def expected-methods-data
  [{:name 'm2,
    :return-type 'java.lang.Object,
    :declaring-class 'delegate.reflect_test.P,
    :parameter-types (list 'java.lang.Object),
    :exception-types '(),
    :flags #{:public :abstract}}
   {:name 'm1,
    :return-type 'java.lang.Object,
    :declaring-class 'delegate.reflect_test.P,
    :parameter-types (),
    :exception-types (),
    :flags #{:public :abstract}}])

(deftest methods-test
  (testing "with a class"
    (let [class R]
      (is (= (map #(->> % vec (into {}))
                    (get (methods class) 'delegate.reflect_test.P))
             expected-methods-data))))
  (testing "with an instance"
    (let [instance (R. 1 2 3)]
      (is (= (map #(->> % vec (into {}))
                    (get (methods instance) 'delegate.reflect_test.P))
             expected-methods-data)))))

(run-tests)
