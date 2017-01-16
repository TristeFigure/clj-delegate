(ns clj-delegate.machinery
  (:use [clojure.pprint])
  (:require [clj-delegate.reflect
             :refer [get-basis is-record? protocol?
                     all-methods parameter-names]
             :as reflect]
            [shuriken.core :refer [deep-merge fully-qualify index-by slice]]))

;                   local format   list of clojure.lang.Method   deftype specs
; to-deftype-specs        • -----------------------------------------> •
; to-deftype-specs        • <-------------------- •
; to-local-format         • <----------------------------------------- •


(defn- parse-method-code [specs]
  (let [[name params & body] specs]
    {:name name
     :params (-> params rest vec) ;; get rid of 'this
     :this (first params)
     :body body}))

(defn- delegate-body [name params]
  `((~(symbol (str "." name))
        (.delegate ~'this)
        ~@params)))

(defn partition-deftype-specs [specs]
  (slice symbol? specs
         :include-delimiter :left
         :include-empty true))

(defn- to-local-format [delegate specs]
  (if (-> specs meta :format (= :local-format))
    specs
    (with-meta
      (index-by
        (fn [x]
          [(:name x)
           (-> x :params rest parameter-names)])
        #(last %2)
        (if (instance? clojure.reflect.Method (first specs))
          ;; convert from list of clojure.reflect.Methods to local format
          (->> specs
               (map (fn [{:keys [name declaring-class protocol parameter-types] :as m}]
                      (let [params (parameter-names parameter-types)]
                        {:name name
                         :declaring-class declaring-class
                         :protocol protocol
                         :params params
                         :this 'this
                         :body (delegate-body name params)}))))
          ;; convert defrecord/deftype quoted specs code to local format
          (->> specs
               partition-deftype-specs
               (mapcat (fn [[proto-or-class & methods]]
                         (if (empty? methods)
                           [{:declaring-class delegate
                             :protocol (fully-qualify proto-or-class)
                             :no-method true}]
                           (for [m (map parse-method-code methods)]
                             (assoc m
                               :declaring-class delegate
                               :protocol (fully-qualify proto-or-class)))))))))
      {:format :local-format})))

(defn- to-deftype-specs [delegate formatted-specs]
  ;; convert from local format to defrecord/deftype quoted specs code
  (if (-> formatted-specs meta :format (= :deftype-specs))
    formatted-specs
    (do (with-meta
          (->> formatted-specs
               vals
               (group-by :protocol)
               (mapcat (fn [[proto methods]]
                         (if (-> methods first :no-method)
                           (list proto)
                           `(~proto
                              ~@(for [{:keys [name params this body]} methods]
                                  `(~name ~(vec (cons this params))
                                          ~@body)))))))
          {:format :deftype-specs}))))

(defn- merge-specs [delegate & args]
  (to-deftype-specs
    delegate (apply deep-merge (map (partial to-local-format delegate)
                                    args))))

(def emit-deftype* @#'clojure.core/emit-deftype*)

(defn protocol-symbol-to-class-symbol
  "Converts from a fully qualified symbol denoting a protocol's ivar
  to a fully qualified symbol denoting the corresponding class."
  [sym]
  (let [string (str sym)
        adapt #(-> %
                   (clojure.string/replace "/" ".")
                   (clojure.string/replace "-" "_"))]
    (symbol
      (if (re-find #"/" string)
        (adapt string)
        (-> string symbol fully-qualify str adapt)))))

(defn ensure-namespaced-symbol
  "If the given symbol has no namespace, prepend it with *ns*."
  [sym]
  (let [string (str sym)]
    (symbol
      (if (re-find #"/|\." string)
        string
        (str *ns* \/ string)))))

(defn- emit-deftype [name delegate fields generated-specs]
  (let [local (to-local-format delegate generated-specs)
        protocols (->> local
                       (map (comp :protocol val))
                       distinct
                       (map ensure-namespaced-symbol)
                       (map protocol-symbol-to-class-symbol)
                      (remove '#{java.lang.Object})
                       vec)
        methods (->> local
                     (remove (fn [[[_method-name _params] method-specs]]
                               (:no-method method-specs)))
                     (map (fn [[k v]]
                            `(~(:name v) ~(->> v :params (cons 'this) vec)
                                ~@(:body v)))))
        gname name
        fields (->> fields (cons 'delegate) vec)]
    (emit-deftype* name gname
                   fields protocols
                   methods
                   {})))

(defn- emit-delegate-fields-accessors [delegate]
  (let [fields (-> delegate resolve get-basis)]
    `(~(symbol (str delegate "Fields"))
        ~@(map (fn [field]
                 `(~field [~'this]
                          (~(symbol (str "." field))
                             (.delegate ~'this))))
               fields))))

(defn- emit-delegate-fields-protocol [delegate]
  (let [fields (-> delegate resolve get-basis)]
    `(defprotocol ~(symbol (str delegate "Fields"))
       ~@(map (fn [field]
                `(~field [~'this]))
              fields))))

(defn- emit-import-statement [name]
  `(import ~(symbol (-> (str *ns* "."  name)
                        (clojure.string/replace "-" "_")))))

(def build-positional-factory
  @#'clojure.core/build-positional-factory)

(defn emit-positional-factory [name fields]
  (let [fields (cons 'delegate fields)]
    (build-positional-factory name
                              (symbol (-> (str *ns* "."  name)
                                          (clojure.string/replace "-" "_")))
                              fields)))

(defn emit-map-factory [name]
  (let [fully (clojure.string/replace (str *ns* \. name)
                                      "-" "_")]
    `(defn ~(symbol (str "map->" name))
       ~(str "Factory function for class " fully ", taking a map of keywords "
             "to field values.")
       [m#]
       (~(symbol (str fully "/create"))
          (if (instance? clojure.lang.MapEquivalence m#)
            m#
            (into {} m#))))))

(defn- emit-declare-factories [name & {:keys [map-factory?]}]
  `(do (declare ~(symbol (str "->" name)))
       ~(when map-factory?
          `(declare ~(symbol (str "map->" name))))))

(defn emit-return-statement [name]
  (symbol (-> (str *ns* "."  name)
              (clojure.string/replace "-" "_"))))

(def default-transforms
  {#(and (= (:protocol %) 'java.lang.Object)
         (not= (:name %) 'toString))
   (constantly nil)})

(defn apply-transforms [delegate transforms methods]
  (let [trs (merge default-transforms transforms)]
    (->> (to-local-format delegate methods)
         (map (fn [[key m]]
                (reduce (fn [m [predicate transformer]]
                          (if (predicate m)
                            (transformer m)
                            m))
                        m trs)))
         (remove (complement identity))
         (index-by (fn [m]
                     [(:name m)
                      (:params m)]))
         (to-deftype-specs delegate))))

(defn- emit-declaration [name delegate fields transforms delegator-specs]
  (emit-deftype name delegate fields
                (merge-specs delegate
                      (emit-delegate-fields-accessors delegate)
                      delegator-specs)))

(defn emit-deftype-delegate
  [name delegate fields transforms delegator-specs]
  `(do ~(emit-declare-factories name :map-factory? false)
       ~(emit-delegate-fields-protocol delegate)
       ~(emit-declaration name delegate fields
                          transforms
                          (merge-specs delegate
                                       (apply-transforms delegate transforms
                                                         (all-methods delegate))
                                       delegator-specs))
       ~(emit-import-statement name)
       ~(emit-positional-factory name fields)
       ~(emit-return-statement name)))

(defn record-transforms [fields]
  ;; associative functions
  {#(contains? '#{java.util.Map
                  clojure.lang.IHashEq
                  clojure.lang.ILookup
                  clojure.lang.IKeywordLookup
                  clojure.lang.IPersistentMap
                  java.io.Serializable}
               (:protocol %))
   (fn [m]
     (assoc m :body
       `((~(symbol (str \. (:name m)))
            (merge (.delegate ~'this)
                   ~'this)
            ~@(:params m)))))
   
   ;; seq
   #(contains? '#{clojure.lang.Seqable}
               (:protocol %))
   (fn [m]
     (assoc m :body
       `((~(symbol (str \. (:name m)))
            (seq (merge
                   (.delegate ~'this)
                   (into {} ~(mapv (fn [f]
                                     `[~(keyword f)
                                       (~(symbol (str \. f)) ~'this)])
                                   fields))))))))
   
   ;; keyword look-up
   #(contains? '#{clojure.lang.IKeywordLookup}
               (:protocol %))
   (fn [m]
     (assoc m :body
       `((reify clojure.lang.ILookupThunk
           (get [~'_thunk ~'_target]
                (get (merge (.delegate ~'this)
                            ~'this)
                     ~@(:params m)))))))})

(defn emit-defrecord-delegate
  [name delegate fields transforms delegator-specs]
  `(do ~(emit-declare-factories name :map-factory? true)   ;; •
       ~(emit-delegate-fields-protocol delegate)
       ~(emit-declaration
          name delegate (concat fields '[__meta __extmap]) ;; •
          transforms
          (cons
            'clojure.lang.IRecord                          ;; •
            (merge-specs delegate              
                         (apply-transforms delegate
                                           (merge (record-transforms fields)
                                                  transforms)
                                           (all-methods delegate))
                         delegator-specs)))     
       ~(emit-import-statement name)
       ~(emit-positional-factory name fields)
       ~(emit-map-factory name)                            ;; +
       ~(emit-return-statement name)))
