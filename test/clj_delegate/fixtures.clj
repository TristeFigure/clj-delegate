(ns clj-delegate.fixtures)

(defprotocol Protocol
  (method [this a]))

(defprotocol ProtocolA
  (method-a [this a]
            [this a b])
  (method-b [this b]))

(defprotocol ProtocolB
  (method-c [this c]))

(defprotocol TagProtocol)

(definterface Interface
  (m [a])
  (m [a b]))

(deftype Type [w x y]
  Protocol
  (method [this a] :original)
  
  ProtocolA
  (method-a [this a] :original-a)
  (method-a [this a b] :original-aa)
  (method-b [this b] :original-b)
  
  Object
  (toString [this] "xyz"))

(defrecord Record [a b c]
  Interface
  (m [this a]   :original-m1)
  (m [this a b] :original-m2)
  
  Protocol
  (method [this a] :original)
  
  ProtocolA
  (method-a [this a] :original-a)
  (method-a [this a b] :original-aa)
  (method-b [this b] :original-b))

(defrecord ParentRecord [])
(defrecord ChildRecord [])
