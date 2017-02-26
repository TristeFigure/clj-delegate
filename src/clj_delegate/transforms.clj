(ns clj-delegate.transforms
  (:require [clj-delegate.specs
             :refer [to-deftype-specs to-local-format parse-method-code]]
            [clj-delegate.reflect :refer [protocol?]]
            [shuriken.core :refer [fully-qualify unqualify index-by or?]]))

(def ^:dynamic *default-transforms*
  [['[[java.lang.Object getClass []]
      [java.lang.Object wait [a]]
      [java.lang.Object wait [a b]]
      [java.lang.Object notify []]
      [java.lang.Object notifyAll []]]
    nil]])

(defn- raise-error [thing reason]
  (throw (ex-info "Delegate transform parsing error"
                  {:type :delegate-transform-parsing-error
                   :thing thing
                   :reason reason})))

(defn- method-signature? [x]
  (and (or (list? x)
           (vector? x))
       (= (count x) 3)
       (symbol? (first x))
       (symbol? (second x))
       (vector? (last x))))

(defn- build-matcher [matcher]
  (if (and (coll? matcher)
           (not (method-signature? matcher)))
    (apply or? (map build-matcher matcher))
    (do (when-not ((or? symbol? class? ifn? method-signature?) matcher)
          (raise-error matcher
                       (str "Not a delegate matcher:\n"
                            (with-out-str (clojure.pprint/pprint matcher))
                            "Must be a class, a symbol, an ifn or a method "
                            "signature triplet [protocol method params].")))
        (cond
          (or (protocol? matcher)
              (class? matcher))
          (let [fully
                (if (class? matcher)
                  (symbol (.getName matcher))
                  (.getName (:on-interface matcher)))
                unfully (unqualify fully)
                matches? (set [fully unfully])]
            #(matches? (:protocol %)))
          
          (method-signature? matcher)
          (let [proto-name (take 2 matcher)
                params-count (count (last matcher))]
            (fn [m]
              (and (= ((juxt  :protocol :name) m)
                      proto-name)
                   (= (count (:params m))
                      params-count))))
          
          (symbol? matcher)
          (let [fully (fully-qualify matcher)
                unfully (unqualify matcher)
                matches? (set [fully unfully])]
            #(matches? (:protocol %)))
          
          (ifn? matcher)
          matcher))))

(defn- match? [matcher method]
  ((build-matcher matcher)
   method))

(defn- build-transformer [transformer]
  (when-not ((or? list? ifn? nil? false?) transformer)
    (raise-error transformer
                 (str "Not a delegate transformer:\n"
                      (with-out-str clojure.pprint/pprint transformer)
                      "Must be quoted clojure code or an ifn.")))
  (let [call-transformer (fn [transformer m]
                           ;; returns nil if transformer is nil or false
                           (let [result (transformer m)]
                               (if (list? result) ;; if it's clojure code
                                 (merge m (parse-method-code result))
                                 result)))]
    (cond
      (not transformer)  (constantly nil)
      (ifn? transformer) (partial call-transformer transformer)
      :else              (fn [m]
                           (merge m (parse-method-code transformer))))))

(defn- transform [transformer method]
  ((build-transformer transformer)
   method))

(defn- compose-matchers-and-transformers [transforms]
  (->> transforms
       (map (fn [[matcher transformer]]
              (fn [m]
                (if (match? matcher m)
                  (transform transformer m)
                  m))))
       (apply comp)))

(defn apply-transforms [delegate transforms methods]
  (let [try-transforming (compose-matchers-and-transformers
                           (concat *default-transforms*
                                   transforms))]
    (->> (to-local-format delegate methods)
         vals
         (map try-transforming)
         (remove (complement identity))
         (index-by (fn [m]
                     [(:name m)
                      (:params m)]))
         (to-deftype-specs delegate))))
