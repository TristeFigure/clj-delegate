(ns clj-delegate.core
  (:require [clj-delegate.reflect :refer [is-record?]]
            [clj-delegate.machinery
             :refer [emit-deftype-delegate emit-defrecord-delegate]]))

(defmacro defdelegate [name delegate fields map-or-symbol & more]
  (let [[transforms delegator-specs] (if (map? map-or-symbol)
                                        [map-or-symbol more]
                                        [{} (cons map-or-symbol more)])]
    (if (is-record? delegate)
      (emit-defrecord-delegate
        name delegate fields transforms delegator-specs)
      (emit-deftype-delegate
        name delegate fields transforms delegator-specs))))
