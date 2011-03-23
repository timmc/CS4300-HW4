(ns timmcHW5.parse
  (:require [timmcHW5.tri :as t]))

(defn parse-line
  "Parse one line of a .tri file into a triangle."
  [line lnum]
  (binding [*read-eval* false]
    (let [nums (read-string (str "(" line ")"))]
      (when-not (and (coll? nums)
                     (= (count nums) 18)
                     (every? number? nums))
        (throw (Exception. (format "Line %d invalid." lnum))))
      (apply t/make (partition 3 nums)))))

(defn parse
  "Parse a seq of lines into a seq of triangles."
  [line-seq]
  (loop [lines line-seq
         tris []
         lnum 0]
    (if (seq lines)
      (let [line (first lines)
            remain (next lines)]
        (if (or (zero? (count line))
                (= (first line) \#))
          (recur remain
                 tris
                 (inc lnum))
          (recur remain
                 (conj tris (parse-line line lnum))
                 (inc lnum))))
      tris)))

