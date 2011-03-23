(ns timmcHW5.test.tri
  (:use [timmcHW5.tri] :reload)
  (:use [clojure.test]))

(deftest accessors
  (let [basic-tri (make [0 1] [2 3] [4 5] [0 0 1] [0 1 0] [1 0 0])]
    (is (= (vertices basic-tri) [[0 1] [2 3] [4 5]]))
    (is (= (colors basic-tri) [[0 0 1] [0 1 0] [1 0 0]]))
    (is (= (edges basic-tri) [[[0 1] [2 3]] [[2 3] [4 5]] [[4 5] [0 1]]]))))

(deftest bounding
  (is (= (candidate-points2 (make [0.1 0.1] [2.5 0.1] [0.1 2.5]
                                  [0 0 0] [0 0 0] [0 0 0]))
         [[0 0] [0 1] [0 2]
          [1 0] [1 1] [1 2]
          [2 0] [2 1] [2 2]])))

(deftest orienting
  (let [unit-ccw (orient (make [0 0 0] [1 0 0] [0 1 0] [] [] []))]
    (is (= (orientation unit-ccw) [0 0 1]))
    (is (front-face3z? unit-ccw)))
  (let [unit-cw (orient (make [0 0 0] [0 1 0] [1 0 0] [] [] []))]
    (is (= (orientation unit-cw) [0 0 -1]))
    (is (not (front-face3z? unit-cw)))))
