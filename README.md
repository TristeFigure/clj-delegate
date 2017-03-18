# clj-delegate

Define delegates around types and records.

## Usage

```clojure
[clj-delegate "0.1.1"]
```


```clojure
(ns my-ns
  (:require [clj-delegate.core :refer [defdelegate]]))
```

## Example

```clojure
(defprotocol MyProtocol
  (my-method [this])
  (my-other-method [this]))

(defrecord MyRecord [a b c]
  MyProtocol
  (my-method [this]
    :bonjour)
  (my-other-method [this]
    :au-revoir))

(defdelegate MyDelegate MyRecord [d]
  MyProtocol
  (my-other-method [this]
    :goodbye))

(let [record   (MyRecord. 1 2 3)
      delegator (MyDelegate. record 4)]
  ;; A record delegate is a record like any other record that stores a delegate
  ;; instance and supplementary fields.
  (println delegator)
  ;; => #clj_delegate.core_test.MyDelegate{:a 1, :b 2, :c 3, :d 4}
  
  ;; The underlying delegate instace stays accessible as the 'delegate' field
  (println (.delegate delegator))
  ;; => #clj_delegate.explorations.MyRecord{:a 1, :b 2, :c 3}
  
  ;; Note that this special field is not accessible via a keyword lookup
  (println (:delegate delegator))
  ;; => nil
  
  ;; Original methods are callable
  (println (.my-method delegator))
  ;; => bonjour
  
  ;; And can be redefined at will
  (println (.my-other-method delegator))
  ;; => goodbye
  
  ;; The same goes for fields : delegate fields are directly accessible to the
  ;; the delegator.
  (println (.a delegator))
  (println (:a delegator))
  ;; => 1
  
  ;; And additionnal fields can be defined as well.
  (println (.d delegator))
  (println (:d delegator))
  ;; => 4
  
  ;; Any modification to the delegator (with the exception of its own fields)
  ;; impacts the underlying delegate. Here is an example using 'assoc'. The same
  ;; logic follows with dissoc, conj, cons, with-meta, etc...
  (println (.delegate (assoc delegator :x :y)))
  ;; => #clj_delegate.explorations.MyRecord{:a 1, :b 2, :c 3, :x :y}
  
  ;; More importantly, delegator equivalate their delegate
  (println (= delegator record))
  ;; => true
  
  ;; And are derive from it in the default isa? hierarchy
  (println (isa? MyDelegate MyRecord))
  ;; => true
  
  ;; By default all method is forwarded to the delegate
  (println (meta (MyDelegate. (with-meta (MyRecord. 1 2 3)
                                         {:a :aa})
                              4)))
  ;; => {:a :aa}
  
  ;; Except if the method has been redefined in the body of the delegate of
  ;; if it has been subject to a transform
  )
```

## Transforms

An additionnal `transforms` argument can be passed to `defdelegate`:

```clojure
(defdelegate MyDelegate MyRecord [field1 field2]
  [[matcher1 transformer1]
   [matcher2 transformer2]])
```

A matcher can be :
- a list of matchers
- the fully qualified symbol of a class name
- a method signature triplet of the form:
  `[fully-qualified-class-symbol method-name param-names]`
  `'[clojure.lang.IObj withMeta [m]]`
  Note that only the number of parameters and not their name matters in the
  signature.
- a function accepting one method-descriptor as argument that should return true
  when it matches the method at hand.

A transformer can either be :
- a function accepting a method-descriptor as argument that modifies then
  returns it.
- quoted clojure code as a replacement for the method's existing implementation.
- the nil or false value

These matcher/transformer pairs will be used to process the existing methods of
the delegate in order to adapt them. Whenever a matcher will match a method, the
corresponding transformer will be run against it.
If the transformer is  or returns nil/false, then the method will be removed
from the delegate implementation.
Not that transformers return clojure quoted code and should return a seq user
as body of the adapted method.

Example:

```clojure
[
   ;; Our main strategy is to forward calls to the merge of the delegator and
   ;; its delegate.
   ['[java.util.Map
      clojure.lang.IHashEq
      clojure.lang.ILookup
      [clojure.lang.IPersistentCollection count []]
      [clojure.lang.IPersistentCollection equiv []]
      java.io.Serializable
      java.lang.Iterable]
    (fn [m]
       (assoc m :body
         `(~(emit-call m
              (emit-merge-with-delegate m
                (:this m))))))]
   
   ; seq and associative (except assoc)
   ['[[clojure.lang.Seqable seq []]
      [clojure.lang.Associative containsKey [k]]
      [clojure.lang.Associative entryAt [k]]]
    (fn [m]
        (assoc m :body
          `(~(emit-call m
               (emit-merge-with-delegate m
                 (emit-fields-map (:this m) fields))))))]
]
```
