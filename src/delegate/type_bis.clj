(ns delegate.type-bis
  (:use [clojure.pprint])
  (:require [delegate.reflect-bis :refer [get-basis is-record? protocol?
                                          all-methods parameter-names
                                          record-native-interfaces]
             :as reflect]
            [shuriken.core :refer [deep-merge fully-qualify index-by]]))

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

(defn- to-local-format [delegate specs]
  (if (-> specs meta :format (= :local-format))
    specs
    (with-meta
      (index-by
        (fn [x]
          [(:name x)
           (-> x :params rest parameter-names)])
        #(last %2)
        (if (instance? clojure.reflect.Method (-> specs first))
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
               (partition-by class)
               (partition 2)
               (mapcat (fn [[[proto-or-class] methods]]
                         (for [m (map parse-method-code methods)]
                           (assoc m
                             :declaring-class delegate
                             :protocol (fully-qualify proto-or-class))))))))
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
                         `(~proto
                            ~@(for [{:keys [name params this body]} methods]
                                `(~name ~(vec (cons this params))
                                   ~@body))))))
          {:format :deftype-specs}))))

(defn- merge-specs [delegate & args]
  (to-deftype-specs
    delegate (apply deep-merge (map (partial to-local-format delegate)
                                    args))))

(def emit-deftype* @#'clojure.core/emit-deftype*)

(defn protocol-symbol-to-class-symbol [sym]
  (let [string (str sym)
        adapt #(-> %
                   (clojure.string/replace "/" ".")
                   (clojure.string/replace "-" "_"))]
    (symbol
      (if (re-find #"/" string)
        (adapt string)
        (-> string symbol fully-qualify str adapt)))))

(defn- emit-deftype [name delegate fields generated-specs]
  (let [local (to-local-format delegate generated-specs)
        protocols (->> local
                       (map (comp :protocol val))
                       distinct
                       (map protocol-symbol-to-class-symbol)
                      (remove '#{java.lang.Object})
                       vec)
        methods (map (fn [[k v]]
                       `(~(:name v) ~(->> v :params (cons 'this) vec)
                           ~@(:body v)))
                     local)
        gname name
        fields (->> fields (cons 'delegate) vec)]
    (emit-deftype* name gname
                   fields protocols
                   methods
                   {}))
  ; `(deftype ~name ~(vec (cons 'delegate fields))
  ;    ~@generated-specs)
  )

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

(def record-native-methods
  (->> record-native-interfaces
       (mapcat reflect/methods)
       (filter #(instance? clojure.reflect.Method %))
       (index-by (fn [x]
                   [(:name x)
                    (-> x :parameter-types parameter-names)])
                 #(last %2))))

(def default-transforms
  {#(= (:protocol %) 'java.lang.Object)              (constantly nil)
   #(= (:protocol %) 'clojure.lang.IFn)              (constantly nil)
   #(= (:protocol %) 'java.util.concurrent.Callable) (constantly nil)
   #(= (:protocol %) 'java.lang.Runnable)            (constantly nil)
   #(= (:protocol %) 'clojure.lang.AFn)              (constantly nil)
   
   #(contains? record-native-methods
               [(:name %) (:params %)])
   (fn [v]
     (let [signature [(:name v)
                      (-> v :params)]]
       (assoc v :protocol
            (:protocol (get record-native-methods
                            signature)))))})

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

(defn- emit-deftype-delegate
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

(defn- emit-defrecord-delegate
  [name delegate fields transforms delegator-specs]
  `(do ~(emit-declare-factories name :map-factory? true) ;; •
       ~(emit-delegate-fields-protocol delegate)
       ~(emit-declaration name delegate fields
                          transforms
                          (merge-specs delegate              
                                       (apply-transforms delegate transforms
                                                         (all-methods delegate))
                                       (cons 'clojure.lang.IRecord
                                             delegator-specs)))     
       ; ~(emit-import-statement name)
       ~(emit-positional-factory name fields)
       ~(emit-map-factory name)                          ;; +
       ~(emit-return-statement name)))

(defmacro defdelegate [name delegate fields map-or-symbol & more]
  (let [[transforms delegator-specs] (if (map? map-or-symbol)
                                        [map-or-symbol more]
                                        [{} (cons map-or-symbol more)])]
    (if (is-record? delegate)
      (emit-defrecord-delegate
        name delegate fields transforms delegator-specs)
      (emit-deftype-delegate
        name delegate fields transforms delegator-specs))))
