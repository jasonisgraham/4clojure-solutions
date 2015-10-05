(ns four-clojure.java-interop-test
  (:require [clojure.test :refer :all]
            [four-clojure.java-interop :refer :all]))

;; (. object-expr-or-classname-symbol method-or-member-symbol optional-args*)

(deftest test-macroexpansions
  (is (= (macroexpand-1 '(Math/abs -59))
         '(. Math abs -59)))

  (is (= (macroexpand-1 '(.toUpperCase "dogman"))
         '(. "dogman" toUpperCase)))

  (is (= (macroexpand-1 '(String.))
         '(new String)))
  )
