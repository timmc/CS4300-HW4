(ns timmcHW4.flat
  "2D triangle renderer program with barycentric interpolation."
  (:require [timmcHW4.geom :as g]
            [timmcHW4.tri :as t])
  (:import [java.awt Color Graphics2D]))


(defn render
  "Render a flat scene using view-coordinate backface-culled triangles."
  [^Graphics2D g2, tris]
  (doseq [t tris]
    (.setPaint g2 (Color. (float (rand)) (float (rand)) (float (rand))))
    (doseq [[x y] (t/interior-points2 t)]
      (.drawLine g2 x y x y))))

