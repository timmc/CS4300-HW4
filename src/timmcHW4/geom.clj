(ns timmcHW4.geom
  "Vector geometry. Except where otherwise marked, functions take n-vectors.
   Functions that expect a specific dimension will call project on the inputs."
  (:use [incanter.core :only (matrix mmult mult)]))

;;;; Utility

;; TODO: move to separate utility class?
(defn wrap-pairs
  "Given a collection [a b c ... y z],
   return seq [[a b] [b c] ... [y z] [z a]]"
  [coll]
  (take (count coll) (partition 2 1 (cycle coll))))

(defn bounds
  "Get bounding ranges for all dimensions of a collection of coordinates.
   A collection of [x y] points would result in [[xmin xmax] [ymin ymax]]."
  [points]
  (when (empty? points)
    (throw (Exception. "Bounds undefined for 0 points.")))
  (let [by-dim (apply map vector points)]
    (map (partial apply (juxt min max)) by-dim)))

;;;; Vector basics

(defn project
  "Project an n-vector to an m-vector by dropping the higher coordinates.
   Missing coordinates will be replaced with 0."
  [m v]
  (vec (take m (concat v (repeat 0)))))

(defn mag
  "Compute magnitude of a vector."
  [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn sum
  "Compute sum of vectors. Behavior only defined for indices present in all
   vectors."
  [vs]
  (apply map + vs))

(defn scale
  "Multiply a vector by a scalar."
  [scalar v]
  (map (partial * scalar) v))

(defn dot
  "Compute dot product of two vectors."
  [a b]
  (apply + (map * a b)))

(defn cross3
  "Compute cross product of two 3-vectors."
  [a b]
  (let [[ax ay az] (project 3 a)
        [bx by bz] (project 3 b)]
    [(- (* ay bz) (* az by))
     (- (* az bx) (* ax bz))
     (- (* ax by) (* ay bx))]))

(defn unit
  "Normalize to unit vector. Zero vector is returned unchanged."
  [v]
  (let [m (mag v)]
    (if (zero? m)
      v
      (map #(/ % m) v))))

(defn vect
  "Compute a vector from start point to end point."
  [s e]
  (vec (map - e s)))

;;;; Plane geometry

(defn normal2
  "Compute the left normal of a 2D vector."
  [[x y]]
  [(- y) x])

(defn dist-to-line2
  "Get the signed distance from the point (p) to the line (from s through e)
   in the [x y] plane. Points to the left get a positive value."
  [p s e]
  (dot (project 2 (vect s p))
       (unit (normal2 (vect s e)))))

;;;; 3D geometry

(defn rotscale-ZXYs
  "Return a matrix suitable for rotating 3-vectors about the Z, X, and Y axes
   in order and scaling by s."
  [zrad xrad yrad s]
  (let [cosx (Math/cos xrad)
        sinx (Math/sin xrad)
        cosy (Math/cos yrad)
        siny (Math/sin yrad)
        cosz (Math/cos zrad)
        sinz (Math/sin zrad)]
    (mult s
          (mmult (matrix [[cosy 0 siny]
                          [0 1 0]
                          [(- siny) 0 cosy]])
                 (matrix [[1 0 0]
                          [0 cosx (- sinx)]
                          [0 sinx cosx]])
                 (matrix [[cosz (- sinz) 0]
                          [sinz cosz 0]
                          [0 0 1]])))))

;; Even though this is currently general to n dimensions, leaving it specified
;; in 3D to allow optimizations.
(defn xform3
  "Rotate, scale, and shear a point in 3D using a 3x3 matrix."
  [mat pt]
  (into [] (mmult mat pt)))

