(ns delegate.core
  (:use clojure.pprint)
  (:require [delegate.reflect :refer [methods-to-delegate]]
            [clojure.reflect :refer [reflect]]))

(defn declaring-classes [methods]
  (set (map :declaring-class methods)))

(def delegates
  (atom {}))

(defn generate-delegate-key []
  (loop [i (rand)]
    (if (contains? @delegates i)
      (recur (rand))
      i)))

(def parameter-names
  '[a b c d e f g h i j k l m n o p q r s t u v w x y z])

(defn store-delegate [delegate]
  (let [key (generate-delegate-key)]
    (swap! delegates assoc key delegate)
    key))

(defn get-delegate [key]
  (get @delegates key))

(defn generate-proxy-code [delegate class-and-interfaces fs]
  (let [reflection (reflect delegate)
        methods (methods-to-delegate delegate)
        proxy-class-and-interfaces (-> (concat (declaring-classes methods)
                                               class-and-interfaces)
                                       distinct
                                       vec)
        delegate-key (store-delegate delegate)
        method-impls (for [[name methods] (group-by :name methods)]
                       (let [m (first methods)]
                        `(~(:name m) [& args#]
                          (apply ~(symbol (str "." (:name m)))
                                 (get-delegate ~delegate-key)
                                 args#))))]
    (pprint
      `(proxy ~proxy-class-and-interfaces []
         ~@(concat method-impls fs)))
    `(proxy ~proxy-class-and-interfaces []
       ~@(concat method-impls fs))))

(defmacro delegate [object class-and-interfaces & fs]
  `(eval (generate-proxy-code
           ~object (quote ~class-and-interfaces) (quote ~fs))))
