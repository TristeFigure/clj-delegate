(ns clj-delegate.type
  (:require [clj-delegate.machinery :refer :all]
            [clj-delegate.specs :refer [merge-specs]]
            [clj-delegate.transforms :refer [apply-transforms]]
            [clj-delegate.reflect :refer [all-methods]
             :as reflect]))

(defn- emit-deftype [name delegate fields transforms delegator-specs]
  (emit-with emit-deftype*
             name name
             delegate fields
             (merge-specs delegate
                          (apply-transforms delegate transforms
                                            (all-methods delegate))
                          delegator-specs)))

(defn emit-deftype-delegate
  [name delegate fields transforms delegator-specs]
  `(do ~(emit-factories-declarations name :map-factory? false)
       ~(define-delegate-fields-protocol delegate)
       ~(emit-deftype name delegate fields
                      transforms
                      delegator-specs)
       ~(emit-import-statement name)
       ~(emit-positional-factory name fields)
       ~(emit-derive-statement name delegate)
       ~(emit-return-statement name)))
