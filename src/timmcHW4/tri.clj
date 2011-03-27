(ns timmcHW4.tri
  "Triangle definition and processing."
  (:require [timmcHW4.geom :as g])
  (:use [incanter.core :only (matrix solve mmult)])
  (:import [java.awt Color]
           [java.awt.geom AffineTransform Point2D Point2D$Double]))

;;;; Definitions

;;; - Vertices may be either 2D or 3D -- a vector of length 2 or 3.
;;; - A triangle is a map of :v to a collection of 3 [x y z] vertex colls in
;;;   CCW order and :c to a corresponding collection of 3 [r g b] float colls
;;;   for vertex colors.

;;;; Constructors

(defn make
  "Make a triangle from three vertex colls and three [r g b] colls."
  [v0 v1 v2 c0 c1 c2]
  {:v [v0 v1 v2]
   :c [c0 c1 c2]})

;;;; Accessors

(defn vertices
  "Get the vertices of a triangle."
  [tri]
  (:v tri))

(defn edges
  "Get the edge pairs of a triangle."
  [tri]
  (g/wrap-pairs (vertices tri)))

(defn colors
  "Get the colors of a triangle's vertices."
  [tri]
  (:c tri))

;;;; Geometry

(defn ^Point2D pt
  "Make a Point2D object from x and y coordinates."
  [x y]
  (Point2D$Double. x y))

(defn de-pt
  "Read a Point2D object into a 2-vector of x, y."
  [^Point2D p]
  [(.getX p) (.getY p)])

(defn orient
  "Add an orientation vector to the triangle."
  [tri]
  (let [[v0 v1 v2] (vertices tri)
        e01 (g/vect v0 v1)
        e12 (g/vect v1 v2)]
    (assoc tri :orient (g/cross3 e01 e12))))

(defn orientation
  "Retrieve the orientation vector from the triangle, or nil."
  [oriented-tri]
  (:orient oriented-tri))

(defn front-face3z?
  "Determine if an oriented triangle has a positive z component to its
   orientation vector, and is therefore a front-face to a [0 0 1] observer."
  [oriented-tri]
  (let [[_ _ z] (orientation oriented-tri)]
    (pos? z)))

(defn- xform2-single
  "Take [x y] to [w z] via at."
  [^AffineTransform at, [x y]]
  (de-pt (.transform at (pt x y) nil)))

(defn xform2
  "Transform a triangle projected into [x y] using the given transform."
  [^AffineTransform at, tri]
  (update-in tri [:v] #(map (partial xform2-single at) %)))

(defn xform3
  "Transform a triangle's vertices by the given 3x3 non-homogenous matrix."
  [tri mat]
  (update-in tri [:v] g/xform3 mat))

(defn candidate-points2
  "Returns a lazy seq of points that are possibly on the triangle's
   2D projection. Currently returns all points in a triangle's axis-aligned
   bounding box and also within the view's bounding edges."
  [tri vxmin vxmax vymin vymax]
  (let [[[xmin xmax] [ymin ymax]] (g/bounds (vertices tri))
        vs (vertices tri)]
    (for [x (range (int (max xmin vxmin)) (inc (int (min xmax vxmax))))
          y (range (int (max ymin vymin)) (inc (int (min ymax vymax))))]
      [x y])))

(defn make-to-bary2
  "Return a function that computes the barycentric coordinates [α β γ]
   of provided [x y] points."
  [tri]
  (let [[[x1 y1] [x2 y2] [x3 y3]] (vertices tri)
        tform (matrix [[(- x1 x3) (- x2 x3)]
                       [(- y1 y3) (- y2 y3)]])
        invtrans (solve tform)]
    (fn [[x y]]
      (let [diff (matrix [[(- x x3)]
                          [(- y y3)]])
            [α β] (mmult invtrans diff)
            γ (- 1 α β)]
        ;; having fun with Unicode char names -- nix this down the road, maybe
        [α β γ]))))
