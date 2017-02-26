(ns clj-delegate.record
  (:require [clj-delegate.machinery :refer :all]
            [clj-delegate.specs :refer [merge-specs]]
            [clj-delegate.transforms :refer [apply-transforms]]
            [clj-delegate.reflect :refer [all-methods get-basis]]
            [shuriken.core :refer [fully-qualify unqualify separate]]
            [flatland.ordered.map :refer [ordered-map]]))

(def ^:private imap-cons
  #'clojure.core/imap-cons)

(defn- emit-call [method call-on]
  `(~(symbol (str \. (:name method)))
      ~call-on
      ~@(:params method)))
  
(defn- emit-merge-with-delegate [method x]
  `(merge ~'delegate ~x))

(defn- ordered-map-to-bare-code [om]
  `(ordered-map ~@(apply concat (seq om))))

(defn- produce-fields-map [this-sym fields]
  (into (ordered-map)
        (for [f fields]
          `[~(keyword f)
            (~(symbol (str \. f)) ~this-sym)])))

(defn- emit-fields-map
  [this-sym fields]
  (ordered-map-to-bare-code
    (produce-fields-map this-sym fields)))

(defn- emit-fields-symbols
  [fields]
  (for [f fields]
    (-> f str symbol)))

(defn- emit-fields-keywords
  [fields]
  (for [f fields]
    (-> f str keyword)))

(defn- delegator-factory [recordname]
  (let [recordclass ^Class (resolve (symbol recordname))
        max-arg-count (apply max (map #(count (.getParameterTypes %))
                                      (.getConstructors recordclass)))
        args (mapv #(symbol (str "x" %)) (range (- max-arg-count 3)))]
    (eval `(fn [delegate# ~args] (new ~(symbol recordname)
                                      delegate#
                                      ~@args)))))

(defn- map-factory-name [tag]
  (symbol (str "map->" tag)))

(defn- emit-map-operation [method delegator-name delegate-name fields f]
  `(let [f# ~f
         group-fields# (fn [fields-map#]
                         (group-by
                           (fn [[k# _v#]]
                             (case k#
                               :delegate                      :delegate
                               ~(emit-fields-keywords fields) :for-delegator
                                                              :for-delegate)) 
                           fields-map#))
         original-fields-map# ~(emit-fields-map
                                 (:this method)
                                 (concat
                                   (get-basis (resolve delegate-name))
                                   fields))
         new-fields-map# (f# original-fields-map#
                             ~@(:params method))
         new-grouped# (group-fields# new-fields-map#)
         delegate# (if-let [x# (seq (:delegate new-grouped#))]
                     (val (first x#))
                     ~'delegate)
         new-delegate# (if-let [x# (seq (:for-delegate new-grouped#))]
                         (~(map-factory-name delegate-name) x#)
                         delegate#)]
     (~(map-factory-name delegator-name)
        (conj (:for-delegator new-grouped#)
              [:delegate new-delegate#]))))

(defn- record-transforms [delegator-name delegate-name fields]
  [
   ;; Our main strategy is to forward calls to the merge of the delegator and
   ;; its delegate.
   ['[java.util.Map
      clojure.lang.IHashEq
      clojure.lang.ILookup
      [clojure.lang.IPersistentCollection count []]
      [clojure.lang.IPersistentCollection equiv []]
      java.io.Serializable
      java.lang.Iterable]
    (fn [m]
       (assoc m :body
         `(~(emit-call m
              (emit-merge-with-delegate m
                (:this m))))))]
   
   ; seq and associative (except assoc)
   ['[[clojure.lang.Seqable seq []]
      [clojure.lang.Associative containsKey [k]]
      [clojure.lang.Associative entryAt [k]]]
    (fn [m]
        (assoc m :body
          `(~(emit-call m
               (emit-merge-with-delegate m
                 (emit-fields-map (:this m) fields))))))]
   
   ;; cons
   ['[clojure.lang.IPersistentCollection cons [x]]
    (fn [method]
        (assoc method :body
          `(~(emit-map-operation method delegator-name delegate-name fields
                                 imap-cons))))]
   
   ;; assoc
   ['[clojure.lang.Associative assoc [k v]]
    (fn [method]
        (assoc method :body
          `(~(emit-map-operation method delegator-name delegate-name fields
                                 assoc))))]
   
   ;; without
   ['[clojure.lang.IPersistentMap without [k]]
    (fn [method]
      (assoc method :body
        `((let [k# ~@(:params method)
                delegator-fields# (quote ~fields)
                delegate-fields# (get-basis ~(fully-qualify delegate-name))
                common-fields# (set (map keyword
                                         (concat delegate-fields#
                                                 delegator-fields#)))]
            ;; If the key to dissoc is a delegate or a delegator field ...
            (if (contains? common-fields# k#)
              ;; we return a hashmap
              (dissoc (into {} ~(:this method)) k#)
              ;; otherwise we return a delegator with a delegate without that 
              ;; external key (it is not a field)
              (let [delegator-fields-map# ~(emit-fields-map
                                             (:this method)
                                             (cons 'delegate fields))]
                (~(map-factory-name delegator-name)
                   (assoc delegator-fields-map#
                     :delegate (dissoc ~'delegate
                                       k#)))))))))]
  
   ;; keyword look-up
   ['[clojure.lang.IKeywordLookup getLookupThunk [k]]
    (fn [m]
      (assoc m :body
        `((reify clojure.lang.ILookupThunk
            (get [~'_thunk ~'_target]
                 (get (merge ~'delegate
                             ~'this)
                      ~@(:params m)))))))]
   
   ;; empty
   ['[clojure.lang.IPersistentCollection empty []]
    (fn [m]
      (assoc m :body
        `((throw (UnsupportedOperationException.
                   (str "Can't create empty: " ~(str delegator-name)))))))]
   
   ;; withMeta
   ['[clojure.lang.IObj withMeta [m]]
    (fn [method]
      (assoc method :body
        `((~(map-factory-name delegator-name)
             (assoc ~(emit-fields-map (:this method) fields)
               :delegate (.withMeta ~'delegate ~@(:params method)))))))]
   ])

(defn- emit-defrecord
  [delegator-name delegate-name fields transforms delegator-specs]
  (let [classname delegator-name
        w (with-meta (symbol (str (namespace-munge *ns*) "." delegator-name))
                     (meta delegator-name))
        transforms (concat (record-transforms
                               delegator-name delegate-name fields)
                             transforms)
        all-methods-transformed (apply-transforms
                                  delegate-name
                                  transforms
                                  (all-methods delegate-name))
        generated-specs (merge-specs delegate-name
                                     all-methods-transformed
                                     delegator-specs
                                     '(clojure.lang.IRecord))]
    (emit-with emit-deftype*
               delegator-name classname
               delegate-name (conj fields '__meta '__extmap)
               generated-specs)))

(defn emit-defrecord-delegate
  [delegator-name delegate-name fields transforms delegator-specs]
  `(do ~(emit-factories-declarations delegator-name :map-factory? true)   ;; •
       ~(define-delegate-fields-protocol delegate-name)
       ~(emit-defrecord delegator-name delegate-name fields
                        transforms
                        delegator-specs)
       ~(emit-import-statement delegator-name)
       ~(emit-positional-factory delegator-name fields)
       ~(emit-map-factory delegator-name)                                  ;; +
       ~(emit-derive-statement delegator-name delegate-name)
       ~(emit-return-statement delegator-name)))
