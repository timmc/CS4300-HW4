(ns timmcHW5.test.core
  (:use [timmcHW5.core] :reload)
  (:use [clojure.test])
  (:use [timmcHW5.test.utils :only (cut)]))

(deftest mixing
  (let [sample (mix-color [[0 1 1] [1 0 1] [1 1 0]] [0.2 0.3 0.5])]
    (is (= (.getRed sample)   (cut 0 (* 0.8 255))))
    (is (= (.getGreen sample) (cut 0 (* 0.7 255))))
    (is (= (.getBlue sample)  (cut 0 (* 0.5 255))))))
