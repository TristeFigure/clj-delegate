(ns delegate.reflect
  (:use clojure.pprint)
  (:refer-clojure :exclude [methods])
  (:require [clojure.reflect :refer [reflect]]
            [shuriken.core :refer [and? not?]]))

(defn ensure-class [x]
  (if (instance? Class x)
    x
    (class x)))

(defn public-method? [method]
  (contains? (:flags method) :public))

(defn static-method? [method]
  (contains? (:flags method) :static))

(defn abstract-method? [method]
  (contains? (:flags method) :abstract))

(defn get-basis-method? [method]
  (= 'getBasis (:name method)))

(defn methods-to-delegate [delegate]
  (->> delegate
       reflect
       :members
       (filter (and? public-method?
                     (not? static-method?)
                     (not? abstract-method?)))))

(defn get-basis [x]
  (. (. (ensure-class x) getMethod "getBasis" nil) invoke nil nil))

(def native-record-interfaces
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
     })

(defn native-record-method? [method]
  ((set native-record-interfaces)
   (:declaring-class method)))

(defn bases-methods [x]
  (->> (ensure-class x)
       reflect
       :bases
       (map resolve)
       (filter (memfn isInterface))
       (mapcat (comp :members reflect))))

(defn forbidden-method? [method]
  (#{'forEach}
     (:name method)))

(defn methods [x]
  (->> (ensure-class x)
       bases-methods
       (filter (and? public-method?
                     (not? static-method?)
                     (not? get-basis-method?)
                     (not? native-record-method?)
                     #(instance? clojure.reflect.Method %)))
       (group-by :declaring-class)))

(defn base-ancestors [x]
  (tree-seq
    (fn [class]
      (let [bases (:bases (reflect class))]
        (not (or (nil? bases) (empty? bases)))))
    (fn [class]
      (->> class reflect :bases (map resolve)))
    (ensure-class x)))

(defn base-ancestors-methods [xs]
  (-> (mapcat (comp base-ancestors resolve))
      reverse ;; from highest to lowest in the inheritance hierarchy
      (reduce (fn [acc method]
                (merge acc {(:name method) method}))
              {})
      vals))

(defn record-native-methods []
  (->> native-record-interfaces
       (mapcat (comp base-ancestors resolve))
       reverse ;; from highest to lowest in the inheritance hierarchy
       (mapcat (comp :members reflect))
       (reduce (fn [acc method]
                 (merge acc {(:name method) method}))
               {})
       vals
       (filter (and? public-method?
                     (not? static-method?)
                     (not? forbidden-method?)
                     #(instance? clojure.reflect.Method %)))
       (group-by :declaring-class)))

(defn is-record? [symbol]
  (contains? (:bases (reflect (resolve symbol)))
             'clojure.lang.IRecord))
