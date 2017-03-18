(ns clj-delegate.type
  (:require [clj-delegate.machinery :refer :all]
            [clj-delegate.specs :refer [merge-specs]]
            [clj-delegate.transforms :refer [apply-transforms]]
            [clj-delegate.reflect :refer [all-methods]
             :as reflect]
            [shuriken.core :refer [unqualify]]))

(defn- emit-deftype [name delegate fields transforms delegator-specs]
  (emit-with emit-deftype*
             name name
             delegate fields
             (merge-specs delegate
                          (apply-transforms delegate transforms
                                            (all-methods delegate))
                          delegator-specs)))

(defn emit-deftype-delegate
  [delegator-name delegate-name fields transforms delegator-specs]
  (let [delegate-name (unqualify delegate-name)]
    `(do ~(emit-factories-declarations delegator-name :map-factory? false)
         ~(define-delegate-fields-protocol delegate-name)
         ~(emit-deftype delegator-name delegate-name fields
                        transforms
                        delegator-specs)
         ~(emit-import-statement delegator-name)
         ~(emit-positional-factory delegator-name fields)
         ~(emit-derive-statement delegator-name delegate-name)
         ~(emit-return-statement delegator-name))))
