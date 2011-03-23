(ns timmcHW5.test.utils
  "Common testing utilities."
  (:use [clojure.test])
  (:import (java.math BigDecimal RoundingMode)))

(defn cut
  "Round floating point value to specified decimal places."
  ([val] (cut 6 val))
  ([places val]
     (.. (BigDecimal. val)
         (setScale places RoundingMode/HALF_UP)
         (doubleValue))))

(deftest own-utils
  (is (= (cut 2 0.23428368262) 0.23))
  (is (= (cut   0.23428368262) 0.234284))
  (is (= (cut   0.23428348262) 0.234283))
  (is (not (= (cut 2 0.12345) 0.123)))
  (is (not (= (cut 2 0.12345) 0.1))))

