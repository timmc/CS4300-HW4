(ns timmcHW4.geom
  "Vector geometry. Except where otherwise marked, functions take n-vectors.")

;;;; Utility

(defn wrap-pairs
  "Given a collection [a b c ... y z],
   return seq [[a b] [b c] ... [y z] [z a]]"
  [coll]
  (take (count coll) (partition 2 1 (cycle coll))))

(defn mag
  "Compute magnitude of a vector."
  [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn dot
  "Compute dot product of two vectors."
  [v1 v2]
  (apply + (map * v1 v2)))

(defn normal2
  "Compute the left normal of a 2D vector."
  [[x y]]
  [(- y) x])

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

(defn project
  "Project an n-vector to an m-vector by dropping the higher coordinates.
   Missing coordinates will be replaced with 0."
  [m v]
  (vec (take m (concat v (repeat 0)))))

(defn dist-to-line2
  "Get the signed distance from the point (p) to the line (from s through e)
   in the [x y] plane. Points to the left get a positive value."
  [p s e]
  (dot (vect s p)
       (unit (normal2 (vect s e)))))

(defn in-poly2?
  "Return distance to closest edge if inside, or else logical false.
   Only works for simple, convex polygons.
   Vertices will be projected to the [x y] plane."
  [pt verts]
  (let [edges (wrap-pairs (map (partial project 2) verts))
        dists (map #(apply dist-to-line2 pt %) edges)]
    (if (some neg? dists)
      nil
      (reduce min dists))))
