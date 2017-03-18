(ns clj-delegate.core
  (:require [clj-delegate.reflect :refer [is-record?]]
            [clj-delegate.type   :refer [emit-deftype-delegate]]
            [clj-delegate.record :refer [emit-defrecord-delegate]]))

(defmacro defdelegate [name delegate fields & [maybe-vec & more]]
  (let [more (if more more [])
        [transforms delegator-specs] (if (vector? maybe-vec)
                                       [maybe-vec more]
                                       [[]
                                        (if (nil? maybe-vec)
                                          more
                                          (cons maybe-vec more))])]
    (if (is-record? delegate)
      (emit-defrecord-delegate
        name delegate fields transforms delegator-specs)
      (emit-deftype-delegate
        name delegate fields transforms delegator-specs))))
