(ns clj-delegate.machinery
  (:require [clj-delegate.reflect
             :refer [get-basis is-record? protocol?
                     parameter-names
                     native-record-interfaces]
             :as reflect]
            [clj-delegate.derive :refer [derive-delegate]]
            [clj-delegate.specs :refer [merge-specs to-local-format]]
            [shuriken.core :refer [fully-qualify or?]]))



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

(def emit-deftype* @#'clojure.core/emit-deftype*)

(defn emit-delegate-fields-accessors [delegate]
  (let [fields (-> delegate resolve get-basis)]
    `(~(symbol (str delegate "Fields"))
        ~@(map (fn [field]
                 `(~field [~'this]
                          (~(symbol (str "." field))
                             (.delegate ~'this))))
               fields))))

(defn emit-with [f name gname delegate fields delegator-specs]
  (let [generated-specs (merge-specs delegate
                                     (emit-delegate-fields-accessors delegate)
                                     delegator-specs)
        local (->> (to-local-format delegate generated-specs)
                   (remove (fn [m]
                             (let [proto (:protocol m)]
                               (or (native-record-interfaces proto)
                                   ('#{java.lang.Object} proto))))))
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
        fields (->> fields (cons 'delegate) vec)]
    (f name gname
       fields protocols
       methods
       {})))

(defn define-delegate-fields-protocol [delegate]
  (let [fields (-> delegate resolve get-basis)]
    (eval `(defprotocol ~(symbol (str delegate "Fields"))
             ~@(map (fn [field]
                      `(~field [~'this]))
                    fields)))
    nil))

(defn emit-import-statement [name]
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

(defn emit-factories-declarations [name & {:keys [map-factory?]}]
  `(do (declare ~(symbol (str "->" name)))
       ~(when map-factory?
          `(declare ~(symbol (str "map->" name))))))

(defn emit-return-statement [name]
  (symbol (-> (str *ns* "."  name)
              (clojure.string/replace "-" "_"))))

(defn emit-derive-statement [delegator delegate]
  `(derive-delegate ~(-> delegator
                         ensure-namespaced-symbol
                         protocol-symbol-to-class-symbol)
                    ~delegate))


