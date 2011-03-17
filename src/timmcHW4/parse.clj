(ns timmcHW4.parse
  (:import [java.awt Color]))

(defrecord ^{:doc "Colored vertex."}
    CVert
  [x y z ^Color c])

(defn ^CVert make-cvert
  "Make a colored vertex from a [x y z] coll and a [r g b] coll."
  [v c]
  (let [[x y z] v
        [r g b] c]
    (CVert. x y z (Color. (float r) (float g) (float b)))))

;;; A triangle is a collection of 3 CVert records in CCW order.

(defn make-tri
  "Make a triangle from three [x y z] colls and three [r g b] colls."
  [v0 v1 v2 c0 c1 c2]
  [(make-cvert v0 c0)
   (make-cvert v1 c1)
   (make-cvert v2 c2)])

(defn parse-line
  "Parse one line of a .tri file into a triangle."
  [line lnum]
  (binding [*read-eval* false]
    (let [nums (read-string (str "(" line ")"))]
      (when-not (and (coll? nums)
                     (= (count nums) 18))
        (throw (Exception. (format "Line %d invalid." lnum))))
      (apply make-tri (partition 3 nums)))))

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

