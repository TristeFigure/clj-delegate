(ns clj-delegate.reflect
  (:refer-clojure :exclude [methods satisfies? find-protocol-impl])
  (:require [clojure.reflect :refer [reflect]]
            [shuriken.core :refer [fully-qualify and? not? or? tree-seq-breadth
                                   index-by]]))

(def native-record-interfaces
  '#{clojure.lang.IRecord
     clojure.lang.IHashEq
     clojure.lang.IObj
     clojure.lang.ILookup
     clojure.lang.IKeywordLookup
     clojure.lang.IPersistentMap
     clojure.lang.Associative
     java.util.Map
     java.io.Serializable})

(defn ensure-class [x]
  (if (class? x)
    x
    (class x)))

(defn ensure-symbol [x]
  (if (class? x)
    (symbol (.getName x))
    x))

(defn maybe-resolve [class-or-symbol]
  (if (symbol? class-or-symbol)
    (resolve class-or-symbol)
    class-or-symbol))

(def ^:private alphabet
  '[a b c d e f g h i j k l m n o p q r s t u v w x y z])

(defn parameter-names [parameter-types]
  (vec (take (count parameter-types)
             (for [x alphabet
                   xx alphabet]
               (symbol (str x xx))))))

(defn protocol? [symbol-or-proto]
  (let [proto (if (symbol? symbol-or-proto)
                (try
                  (-> symbol-or-proto resolve deref)
                  (catch Throwable t))
                symbol-or-proto)]
    (try
      (= (-> proto keys sort)
         [:method-builders :method-map :on :on-interface :sigs :var])
      (catch Throwable t
        false))))

(def pref
  @#'clojure.core/pref)

(def reduce1
  @#'clojure.core/reduce1)

(def super-chain
  @#'clojure.core/super-chain)

(defn- find-protocol-impl [protocol x]
  (let [proto-interface (:on-interface protocol)
        x (ensure-class (maybe-resolve x))]
    (if (or (instance? proto-interface x)
            (contains? (ancestors x)
                       proto-interface))
      x
      (let [c x
            impl #(get (:impls protocol) %)]
        (or (impl c)
            (and c (or (first (remove nil? (map impl (butlast (super-chain c)))))
                       (when-let [t (reduce1 pref (filter impl (disj (supers c) Object)))]
                         (impl t))
                       (impl Object))))))))

;; so that satisfies? uses the version of find-protocol-impl from above
(defn- satisfies? [proto-or-sym x]
  (boolean (find-protocol-impl (maybe-resolve proto-or-sym) x)))

(defn ns-protocols [ns]
  (->> (ns-map ns)
       (keep (fn [[k v]]
               (when (var? v)
                 (deref v))))
       (filter protocol?)))

(defn protocols [class-or-symbol]
  (filter #(satisfies? % (maybe-resolve class-or-symbol))
          (ns-protocols *ns*)))

(defn get-basis [x]
  (. (. (ensure-class x) getMethod "getBasis" nil) invoke nil nil))

(defn is-record? [symbol]
  (contains? (:bases (reflect (resolve symbol)))
             'clojure.lang.IRecord))

(defn public-method? [method]
  (contains? (:flags method) :public))

(defn static-method? [method]
  (contains? (:flags method) :static))

(defn abstract-method? [method]
  (contains? (:flags method) :abstract))

(defn base-ancestors
  "Like ancestors, but in breadth-first order, from low to high in the class
  hierarchy."
  [class-or-symbol]
  (distinct
    (rest (tree-seq-breadth
            ;; branch?
            (fn [class]
              (let [bases (:bases (reflect class))]
                (not (or (nil? bases) (empty? bases)))))
            ;; children
            (fn [class]
              (->> class reflect :bases (map resolve)))
            ;; start-point
            (maybe-resolve class-or-symbol)))))


(defn methods [class-or-symbol]
  (let [protos (index-by :on (protocols class-or-symbol))
        methods-to-adapt (->> (base-ancestors class-or-symbol)
                              (mapcat (comp :members reflect))
                              (map (fn [m]
                                     (let [k [(:name m)
                                              (-> m :parameter-types
                                                  parameter-names)]
                                           c (:declaring-class m)]
                                       [k (or (some-> (get protos c)
                                                      :var
                                                      .sym
                                                      fully-qualify)
                                              c)])))
                              (into {}))]
    (->> (maybe-resolve class-or-symbol)
         reflect :members
         (filter #(instance? clojure.reflect.Method %))
         (map (fn [method]
                (-> method
                    (update-in [:name] #(-> % str
                                            (clojure.string/replace \_ \-)
                                            symbol))
                    (assoc :protocol
                      (get methods-to-adapt
                           [(:name method)
                            (-> method :parameter-types parameter-names)]
                           (:declaring-class method)))))))))

(defn all-methods [class-or-symbol]
  (->> (base-ancestors class-or-symbol)
       (cons class-or-symbol)
       reverse
       (mapcat methods)
       (filter (and? #(instance? clojure.reflect.Method %)
                     public-method?
                     (not? static-method?)
                     (not? abstract-method?)))))
