(ns timmcHW4.test.geom
  (:use [timmcHW4.geom] :reload)
  (:use [incanter.core :only (matrix identity-matrix)])
  (:use [clojure.test])
  (:use [timmcHW4.test.utils :only (cut)]))

(deftest utilities
  (is (= (wrap-pairs '[a b c d]) '[[a b] [b c] [c d] [d a]])))

(deftest vectmath
  (is (= (project 2 [1 2 3 4]) [1 2]))
  (is (= (project 4 [1 2]) [1 2 0 0]))
  (is (= (mag [3 4]) 5))
  (is (= (sum [[1 2] [10 20] [100 200]]) [111 222]))
  (is (= (scale -1 [1 2 3]) [-1 -2 -3]))
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
  ;; outside
  (is (not (in-poly2? [2 3] right-tri 0)))
  (is (not (in-poly2? [0 0] right-tri 0)))
  (is (not (in-poly2? [2 0] right-tri 0)))
  ;; inside
  (is (== (in-poly2? [1.5 1.5] right-tri 0) 0.5))
  (is (== (in-poly2? [1.25 1.5] right-tri 0) 0.25))
  (is (== (in-poly2? [1.5 1.25] right-tri 0) 0.25))
  ;; allowance
  (is (== (cut 6 (in-poly2? [0.9 0.9] right-tri 0.1)) 0))
  (is (not (in-poly2? [0.89 0.89] right-tri 0.1))))

(deftest bounding
  (is (= (bounds [[0 1] [10 11] [100 101]]) [[0 100] [1 101]]))
  (is (= (bounds [[0 20 -300]]) [[0 0] [20 20] [-300 -300]]))
  (is (= (bounds [[10 20 -300] [1000 20 3] [100 200 30]])
         [[10 1000] [20 200] [-300 30]])))

(deftest rotscale
  ;; full rotations on each axis -- sanity check
  (is (= (rotscale-ZXYs (* 2 Math/PI) 0 0 1) (identity-matrix 3)))
  (is (= (rotscale-ZXYs 0 (* 2 Math/PI) 0 1) (identity-matrix 3)))
  (is (= (rotscale-ZXYs 0 0 (* 2 Math/PI) 1) (identity-matrix 3)))
  ;; sample 90 degree rotation
  (is (= (rotscale-ZXYs (/ Math/PI 2) 0 0 1)
         (matrix [[0 -1 0]
                  [1 0 0]
                  [0 0 1]])))
  ;; aliasing
  (is (= (rotscale-ZXYs (/ Math/PI 2) (/ Math/PI 2) (/ Math/PI 2) 1)
         (rotscale-ZXYs 0             (/ Math/PI 2) 0             1)))
  ;; rotations of points
  (is (= (map (partial cut 9)
              (xform3 (rotscale-ZXYs 0 (/ Math/PI 2) 0 1)
                      [1 5 2]))
         [1 -2 5]))
  (is (= (map (partial cut 9)
              (xform3 (rotscale-ZXYs 0 0 0 2)
                      [1 5 2]))
         [2 10 4])))

