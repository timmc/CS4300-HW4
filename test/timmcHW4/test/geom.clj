(ns timmcHW4.test.geom
  (:use [timmcHW4.geom] :reload)
  (:use [clojure.test])
  (:import (java.math BigDecimal RoundingMode)))

;;;; Testing utilities

(defn cut
  "Round floating point value to specified decimal places."
  ([val] (cut 6 val))
  ([places val]
     (.. (BigDecimal. val)
         (setScale places RoundingMode/HALF_EVEN)
         (doubleValue))))

(deftest own-utils
  (is (= (cut 2 0.23428368262) 0.23))
  (is (= (cut   0.23428368262) 0.234284))
  (is (= (cut   0.23428348262) 0.234283))
  (is (not (= (cut 2 0.12345) 0.123)))
  (is (not (= (cut 2 0.12345) 0.1))))

;;;; Main tests

(deftest utilities
  (is (= (wrap-pairs '[a b c d]) '[[a b] [b c] [c d] [d a]])))

(deftest vectmath
  (is (= (project 2 [1 2 3 4]) [1 2]))
  (is (= (project 4 [1 2]) [1 2 0 0]))
  (is (= (mag [3 4]) 5))
  (is (= (dot [1 2 3] [-4 5 -6]) -12))
  (is (= (cross3 [3 4 0] [-4 3 0]) [0 0 25]))
  (is (= (unit [3 4]) [3/5 4/5]))
  (is (= (normal2 [3 4]) [-4 3])))

(def right-tri
  [[1 1] [3 1] [1 3]])

(deftest distance
  (is (== (dist-to-line2 [3 102] [0 100] [5 100]) 2))
  (is (== (dist-to-line2 [3 102] [5 100] [0 100]) -2)))

(deftest picking
  (is (not (in-poly2? [2 3] right-tri 0)))
  (is (not (in-poly2? [0 0] right-tri 0)))
  (is (not (in-poly2? [2 0] right-tri 0)))
  (is (== (in-poly2? [1.5 1.5] right-tri 0) 0.5))
  (is (== (in-poly2? [1.25 1.5] right-tri 0) 0.25))
  (is (== (in-poly2? [1.5 1.25] right-tri 0) 0.25))
  (is (is-CCW2? right-tri))
  (is (not (is-CCW2? (rseq right-tri)))))

