(ns timmcHW4.test.core
  (:use [timmcHW4.core] :reload)
  (:import [java.awt Color])
  (:use [clojure.test])
  (:use [timmcHW4.test.utils :only (cut)]))

(deftest clamping
  (is (= (clamp -2 5 -6) -2))
  (is (= (clamp -2 5 0) 0))
  (is (= (clamp -2 5 10) 5))
  (is (= (clamp -2.3 6.5 3) 3.0)))

(deftest mixing
  (let [sample (Color. (mix-color [[0 1 1] [1 0 1] [1 1 0]] [0.2 0.3 0.5]))]
    (is (= (.getRed sample)   (int (* 0.8 255))))
    (is (= (.getGreen sample) (int (* 0.7 255))))
    (is (= (.getBlue sample)  (int (* 0.5 255))))))
