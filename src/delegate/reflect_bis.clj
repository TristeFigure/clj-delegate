(ns delegate.reflect-bis
  (:use clojure.pprint)
  (:refer-clojure :exclude [methods satisfies? find-protocol-impl])
  (:require [clojure.reflect :refer [reflect]]
            [shuriken.core :refer [fully-qualify and? not? tree-seq-breadth
                                   index-by]]))

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
  (filter #(satisfies? % class-or-symbol)
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

(defn declared-by?
  ([class-or-sym x]
   ((declared-by? class-or-sym)
    x))
  ([class-or-sym]
    (fn [x]
      (= (:declaring-class x) (-> class-or-sym
                                  ensure-symbol
                                  fully-qualify)))))

(def record-native-interfaces
  '#{clojure.lang.MapEquivalence
     clojure.lang.IPersistentCollection
     clojure.lang.IObj
     clojure.lang.Seqable
     clojure.lang.IHashEq
     clojure.lang.ILookup
     java.util.Map
     java.util.Iterator
     clojure.lang.IPersistentMap
     clojure.lang.IMeta
     clojure.lang.Associative
     clojure.lang.IKeywordLookup
     Iterable
     ; clojure.lang.Countable
     })

(defn native-record-method? [method]
  ((set record-native-interfaces)
   (:declaring-class method)))

(defn base-ancestors
  "like ancestors, but in breadth-first order, from low to high in the class
  hierarchy"
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
            (ensure-class class-or-symbol)))))

(defn protocols-methods [class-or-symbol]
  (->> (protocols class-or-symbol)
       (mapcat (fn [proto]
                 (mapcat (fn [[k m]]
                           (for [params (:arglists m)]
                             {:key [(:name m)
                                    (-> params rest parameter-names)]
                              :protocol-class (->> (:var proto)
                                                   str
                                                   (drop 2)
                                                   (apply str)
                                                   symbol)}))
                         (:sigs proto))))
       (index-by :key)
       (map (fn [[k v]]
              [k (:protocol-class v)]))
       (into {})))

(defn adapt-method [class-sym methods-to-adapt m]
  (assoc m :protocol
    (if-let [proto (and (declared-by? class-sym m)
                        (get methods-to-adapt
                             [(:name m)
                              (-> m :parameter-types parameter-names)]))]
      proto
      (:declaring-class m))))

(defn methods [class-or-symbol]
  (let [sym (ensure-symbol class-or-symbol)
        to-adapt (merge (->> (base-ancestors class-or-symbol)
                             (mapcat (comp :members reflect))
                             (filter #(instance? clojure.reflect.Method %))
                             (index-by
                               (fn [m]
                                 [(:name m)
                                  (-> m :parameter-types parameter-names)])
                               #(first %2))
                             (map (fn [[[name params] method]]
                                    [[name params] (:declaring-class method)]))
                             (into {}))
                        (protocols-methods class-or-symbol))]
    (->> (maybe-resolve class-or-symbol)
         reflect :members
         (filter #(instance? clojure.reflect.Method %))
         (map (fn [method]
                (update-in method [:name] #(-> % str
                                               (clojure.string/replace "_" "-") 
                                               symbol))))
         (map (partial adapt-method sym to-adapt))
         (remove (and? #(-> % :protocol resolve class?)
                       #(-> % :protocol resolve
                            reflect :flags
                            :interface nil?))))))

; (defn record-methods [sym]
;   {:pre [(symbol? sym)]}
;   (->> (methods sym)
;        (filter (and? (declared-by? sym)
;                      public-method?
;                      (not? static-method?)
;                      (not? abstract-method?)
;                      #_(not? get-basis-method?)
;                      #_native-record-method?
;                      #_(not? native-record-method?)))))

; (defn ancestors-methods [sym]
;   {:pre [(symbol? sym)]}
;   (->> (mapcat methods (reverse (base-ancestors sym)))
;        (filter (and? public-method?
;                      (not? static-method?)
;                      (not? abstract-method?)))))
(defn all-methods [class-or-symbol]
  (->> (base-ancestors class-or-symbol)
       (cons class-or-symbol)
       reverse
       (mapcat methods)
       (filter (and? #(instance? clojure.reflect.Method %)
                     public-method?
                     (not? static-method?)
                     (not? abstract-method?)))))
