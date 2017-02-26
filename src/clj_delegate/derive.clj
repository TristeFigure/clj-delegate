(ns clj-delegate.derive
  (:require [robert.hooke :refer [add-hook with-hooks-disabled]]
            [shuriken.core :refer [silence]]))

(def delegate-hierarchy
  (atom (make-hierarchy)))

(defn derive-delegate [tag parent]
  (swap! delegate-hierarchy
         derive
         (symbol (.getName tag))
         (symbol (.getName parent))))

(defn delegates? [tag parent]
  (with-hooks-disabled isa?
    (isa? @delegate-hierarchy
          (symbol (.getName tag))
          (symbol (.getName parent)))))

(defn isa?-delegate-hook [f & args]
  (let [[parent child] (reverse args)]
    (or (silence Throwable (delegates? child parent))
        (apply f args))))

(add-hook #'isa? #'isa?-delegate-hook)
