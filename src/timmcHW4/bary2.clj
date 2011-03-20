(ns timmcHW4.bary2
  "2D triangle renderer program with barycentric interpolation."
  (:require [timmcHW4.geom :as g]
            [timmcHW4.tri :as t])
  (:import [java.awt Color Graphics2D]))

(defn ^Color mix-color
  "Create a Color object from a barycentric coordinates of a point and the
   float [r g b] colors at the vertices."
  [colors bary-coords]
  (apply #(Color. (float %1) (float %2) (float %3))
         (g/sum (map g/scale bary-coords colors))))

(defn render
  "Render a flat scene using view-coordinate backface-culled triangles."
  [^Graphics2D g2, tris]
  (doseq [t tris]
    (let [to-bary (t/make-to-bary2 t)
          colors (t/colors t)]
      (doseq [[x y] (t/aarect-points2 t)]
        (let [[α β γ] (to-bary [x y])]
          (when (and (< 0 α 1)
                     (< 0 β 1)
                     (< 0 γ 1))
            (doto g2
              (.setPaint (mix-color colors [α β γ]))
              ;; TODO: Check this on CCIS machine -- may not draw properly
              (.drawLine x y x y))))))))

