(ns delegate.core-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [delegate.core :refer :all
             :reload true]))

(definterface Machin
  (machin []))

; (deftest main-test
;   (let [object [1 2 3]
;         deleg (delegate object [Machin]
;                 (machin []
;                   :abc))]
;     (is (= (.machin deleg) :abc))))


(pprint
  (clojure.core/eval
    (delegate.core/generate-proxy-code
      [1 2 3]
      '[Machin]
      '((machin [] :abc)))))

(pprint
  (macroexpand-1
    '(delegate object [Machin]
      (machin []
              :abc))))

(run-tests)
