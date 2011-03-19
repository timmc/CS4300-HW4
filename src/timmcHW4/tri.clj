(ns timmcHW4.tri
  "Triangle definition and processing."
  (:require [timmcHW4.geom :as g])
  (:import [java.awt Color]))

;;;; Definitions

;;; - Vertices may be either 2D or 3D -- a vector of length 2 or 3.
;;; - A triangle is a map of :v to a collection of 3 [x y z] vertex colls in
;;;   CCW order and :c to a corresponding collection of 3 [r g b] float colls
;;;   for vertex colors.

(defn make
  "Make a triangle from three vertex colls and three [r g b] colls."
  [v0 v1 v2 c0 c1 c2]
  {:v [v0 v1 v2]
   :c [c0 c1 c2]})

(defn vertices
  "Get the vertices of a triangle."
  [tri]
  (:v tri))

(defn colors
  "Get the colors of a triangle's vertices."
  [tri]
  (:c tri))
