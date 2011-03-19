(ns timmcHW4.wire
  "3D wireframe triangle renderer."
  (:require [timmcHW4.geom :as g]
            [timmcHW4.tri :as t])
  (:import [java.awt Color Graphics2D]
           [java.awt.geom Line2D$Double]))

(defn render
  "Render a wireframe scene using view-coordinate backface-culled triangles."
  [^Graphics2D g2, tris]
  (.setPaint g2 Color/WHITE)
  (doseq [t tris]
    (doseq [e (t/edges t)]
      (let [[x1 y1 x2 y2] (flatten e)]
        (.draw g2 (Line2D$Double. x1 y1 x2 y2))))))

