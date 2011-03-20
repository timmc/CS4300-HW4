(ns timmcHW4.geom
  "Vector geometry. Except where otherwise marked, functions take n-vectors.
   Functions that expect a specific dimension will call project on the inputs.")

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

(defn in-poly2?
  "Return distance to closest edge if inside, or else logical false.
   Allowance parameter specifies distance from actual edge to consider interior.
   Only works for simple, convex polygons.
   Vertices will be projected to the [x y] plane."
  [pt verts allowance]
  (let [edges (wrap-pairs (map (partial project 2) verts))
        dists (map #(apply dist-to-line2 pt %) edges)
        dists (map (partial + allowance) dists)]
    (if (some neg? dists)
      nil
      (reduce min dists))))

(defn is-CCW2?
  "Check if the [x y] vertices go in CCW order."
  [verts]
  (let [edges (wrap-pairs (map (partial project 2) verts))
        evects (map #(apply vect %) edges)
        evpairs (wrap-pairs evects)]
    (every? #(pos? (nth % 2)) (map #(apply cross3 %) evpairs))))
