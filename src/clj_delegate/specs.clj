(ns clj-delegate.specs
  (:require [clj-delegate.reflect :refer [parameter-names]]
            [shuriken.core :refer [fully-qualify index-by slice]]))

;                   local format   list of clojure.lang.Method   deftype specs
; to-deftype-specs        • -----------------------------------------> •
; to-local-format         • <-------------------- •
; to-local-format         • <----------------------------------------- •

(defn parse-method-code [specs]
  (let [[name params & body] specs]
    {:name name
     :params (-> params rest vec) ;; get rid of 'this
     :this (first params)
     :body body}))

(defn partition-deftype-specs [specs]
  (slice symbol? specs
         :include-delimiter :left
         :include-empty true))

(defn to-local-format [delegate specs]
  (if (-> specs meta :format (= :local-format))
    specs
    (with-meta
      (index-by
        (fn [x]
          [(:name x)
           (-> x :params rest parameter-names)])
        #(last %2)
        (if (instance? clojure.reflect.Method (first specs))
          ;; convert from list of clojure.reflect.Methods to local format
          (->> specs
               (map (fn [{:keys [name declaring-class protocol parameter-types] :as m}]
                      (let [params (parameter-names parameter-types)]
                        {:name name
                         :declaring-class declaring-class
                         :protocol protocol
                         :params params
                         :this 'this
                         :body `((~(symbol (str "." name))
                                    (.delegate ~'this)
                                    ~@params))}))))
          ;; convert defrecord/deftype quoted specs code to local format
          (->> specs
               partition-deftype-specs
               (mapcat (fn [[proto-or-class & methods]]
                         (if (empty? methods)
                           [{:declaring-class delegate
                             :protocol (fully-qualify proto-or-class)
                             :no-method true}]
                           (for [m (map parse-method-code methods)]
                             (assoc m
                               :declaring-class delegate
                               :protocol (fully-qualify proto-or-class)))))))))
      {:format :local-format})))

(defn to-deftype-specs [delegate formatted-specs]
  ;; convert from local format to defrecord/deftype quoted specs code
  (if (-> formatted-specs meta :format (= :deftype-specs))
    formatted-specs
    (do (with-meta
          (->> formatted-specs
               vals
               (group-by :protocol)
               (mapcat (fn [[proto methods]]
                         (if (-> methods first :no-method)
                           (list proto)
                           `(~proto
                              ~@(for [{:keys [name params this body]} methods]
                                  `(~name ~(vec (cons this params))
                                          ~@body)))))))
          {:format :deftype-specs}))))

(defn merge-specs [delegate & args]
  (to-deftype-specs
    delegate (apply merge (map (partial to-local-format delegate)
                               args))))
