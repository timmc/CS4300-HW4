(ns timmcHW4.test.geom
  (:use [timmcHW4.geom] :reload)
  (:use [clojure.test]))

(deftest utilities
  (is (= (wrap-pairs '[a b c d]) '[[a b] [b c] [c d] [d a]])))

(deftest vectmath
  (is (= (mag [3 4]) 5))
  (is (= (dot [1 2 3] [-4 5 -6]) -12))
  (is (= (normal2 [3 4]) [-4 3])))

(def right-tri
  [[1 1] [3 1] [1 3]])

(deftest distance
  (is (= (dist-to-line2 [3 102] [0 100] [5 100]) 2))
  (is (= (dist-to-line2 [3 102] [5 100] [0 100]) -2)))

(deftest picking
  (is (not (in-poly2? [2 3] right-tri)))
  (is (not (in-poly2? [0 0] right-tri)))
  (is (not (in-poly2? [2 0] right-tri)))
  (is (= (in-poly2? [1.5 1.5] right-tri) 0.5))
  (is (= (in-poly2? [1.25 1.5] right-tri) 0.25))
  (is (= (in-poly2? [1.5 1.25] right-tri) 0.25)))

