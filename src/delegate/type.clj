(ns delegate.type
  (:use clojure.pprint)
  (:refer-clojure :exclude [methods])
  (:require [delegate.reflect
             :refer [methods record-native-methods get-basis is-record?]]
            [shuriken.core :refer [fully-qualify]]))

(def parameter-names
  '[a b c d e f g h i j k l m n o p q r s t u v w x y z])

(defn parse-specs [specs]
  (let [adapt (fn [s]
                (let [fq (str (fully-qualify s))]
                  (symbol
                    (if (re-find #"/" fq)
                      (-> fq
                          (clojure.string/replace "-" "_")
                          (clojure.string/replace "/" "."))
                      fq))))]
    (loop [[s & more] specs
           current-class nil
           acc {}]
      (let [current-class (or current-class
                              (when (symbol? s) (adapt s)))]
        (if (nil? s)
          acc
          (if (symbol? s)
            (recur more current-class (assoc acc (adapt s) []))
            (recur more current-class (update-in acc [current-class]
                                                 conj s))))))))

(defn get-spec-for
  ([raw-specs declaring-class]
   (get (parse-specs raw-specs) (fully-qualify declaring-class)))
  ([raw-specs declaring-class-sym method-name]
   (let [result (->> (get (parse-specs raw-specs)
                          (fully-qualify declaring-class-sym))
                     reverse
                     (filter #(= (first %) method-name))
                     first
                     )]
     result)))

(defn generate-delegate-method [method delegate-specs delegate-code-generator]
  (let [override (get-spec-for delegate-specs
                               (:declaring-class method)
                               (:name method))
        fields (take (if override
                       ; since 'this is in the fields
                       (dec (count (second override)))
                       (count (:parameter-types method)))
                     parameter-names)
        name (:name method)]
    (if override
      override
      (delegate-code-generator override fields name))))

(defn generate-specs [methods delegate-specs delegate-code-generator]
  (apply concat
         (for [[declaring-class ms] methods]
           `(~declaring-class
              ~@(for [m ms]
                  (generate-delegate-method m delegate-specs
                                            delegate-code-generator))))))

(defn def-fields-protocol [delegate]
  `(defprotocol ~(symbol (str delegate "Fields"))
     ~@(for [f (get-basis (resolve delegate))]
         `(~f [~'this]))))

(defn generate-field-accessor-specs [delegate]
  `(~(symbol (str delegate "Fields"))
      ~@(for [f (get-basis (resolve delegate))]
          `(~f [~'this]
               (~(symbol (str "." f))
                (.delegate ~'this))))))

(defn generate-declaration [defining-macro name delegate fields specs]
  `(do
     ~(when-not (empty? fields) ; TODO: there is a bug here. fields.
        (def-fields-protocol delegate))
     (~defining-macro ~name ~(vec (cons 'delegate fields))
        ~@(generate-field-accessor-specs delegate)
        ~@specs
        ~@(when-not (get-spec-for specs name delegate)
            '(Object
              (toString [this] "AAAAAAAAAAAAA"))))
     #_(defmethod print-method ~name [v# ^java.io.Writer w#]
       (.write w# "<<-XYZ->>"))))

(defmacro deftype-delegate [name delegate fields & delegate-specs]
  (generate-declaration
    'deftype name delegate fields
    (concat
      (generate-specs
        (methods (resolve delegate))
        delegate-specs
        (fn [override fields name]
          `(~name ~(vec (cons 'this fields))
                  (~(symbol (str "." name))
                     (.delegate ~'this)
                     ~@fields)))))))


(defmacro defrecord-delegate [name delegate fields & delegate-specs]
  (let [deleg (resolve delegate)]
    (generate-declaration
      'deftype name delegate fields
      (concat
        (generate-specs
          (methods deleg)
          delegate-specs
          (fn [override fields name]
            `(~name ~(vec (cons 'this fields))
                    (~(symbol (str "." name))
                       (.delegate ~'this)
                       ~@fields))))
        (generate-specs
          (record-native-methods)
          delegate-specs
          (fn [override fields name]
            `(~name ~(vec (cons 'this fields))
                    (~(symbol (str "." name))
                       (merge (.delegate ~'this) ~'this )
                       ~@fields))))))))

(defmacro defdelegate [name delegate fields & delegate-specs]
  (if (is-record? delegate)
    `(defrecord-delegate ~name ~delegate ~fields ~@delegate-specs)
    `(deftype-delegate ~name ~delegate ~fields ~@delegate-specs)))
