(ns timmcHW4.core
  (:import [java.io BufferedReader FileReader]
           [java.awt Color Graphics2D Dimension]
           [java.awt.geom AffineTransform Line2D$Double]
           [javax.swing JFrame JComponent SwingUtilities UIManager])
  (:require [timmcHW4.parse :as p]
            [timmcHW4.tri :as t]
            [timmcHW4.geom :as g])
  (:gen-class))

;;;; State

(def *tris* (ref nil))

;;;; Transformations

(def view-w 512)
(def view-h 512)

(def ^AffineTransform xform-view-to-canvas
  (doto (AffineTransform.)
    (.translate (/ view-w 2) (/ view-h 2))
    (.scale 1 -1)))

(def *xform-world-to-view*
  (ref (AffineTransform.)))

(defn tri-maybe-on-canvas
  "Return true if at least part of a triangle is possibly on the canvas.
   Tests for intersection of triangle's bounding box with canvas."
  [tri]
  (let [[[xmin xmax] [ymin ymax]] (g/bounds (t/vertices tri))]
    ;; standard axis-aligned rectangle intersection test.
    (and (<= 0 xmax)
         (< xmin view-w)
         (<= 0 ymax)
         (< ymin view-h))))

(defn tris-for-viewpoint
  "Map world triangles to viewpoint, adding orientation vectors and culling
   backface elements."
  [tris]
  (filter t/front-face3z?
          ;; TODO write and use xform3
          (map t/orient tris)))

(defn tris-for-canvas
  "Map viewpoint triangles to canvas, culling offscreen elements."
  [tris]
  (filter tri-maybe-on-canvas
          (map (partial t/xform2 xform-view-to-canvas)
               tris)))

;;;; Renderers

(defn ^Color mix-color
  "Create a Color object from a barycentric coordinates of a point and the
   float [r g b] colors at the vertices."
  [colors bary-coords]
  (apply #(Color. (float %1) (float %2) (float %3))
         (g/sum (map g/scale bary-coords colors))))

;; All renderers take a collection of viewpoint triangles. The triangles have
;; been backface culled and carry an orientation vector at :orient.

(defn render-bary2
  "Render a 2D scene."
  [^Graphics2D g2, vtris]
  (doseq [t (tris-for-canvas vtris)]
    (let [to-bary (t/make-to-bary2 t)
          colors (t/colors t)]
      (doseq [[x y] (t/candidate-points2 t)]
        (let [[α β γ] (to-bary [x y])]
          (when (and (< 0 α 1)
                     (< 0 β 1)
                     (< 0 γ 1))
            (doto g2
              (.setPaint (mix-color colors [α β γ]))
              ;; TODO: Check this on CCIS machine -- may not draw properly
              (.drawLine x y x y))))))))

(defn render-wire
  "Render a wireframe 3D scene."
  [^Graphics2D g2, vtris]
  (.setPaint g2 Color/WHITE)
  (doseq [t (tris-for-canvas vtris)]
    (doseq [e (t/edges t)]
      (let [[x1 y1 x2 y2] (flatten e)]
        (.draw g2 (Line2D$Double. x1 y1 x2 y2))))))

(defn render-shade
  "Render a directionally shaded 3D scene."
  [^Graphics2D g2, vtris]
  (doseq [view-t vtris]
    (let [;; computations on view
          orientation-hat (g/unit (t/orientation view-t))
          zcomp (g/dot orientation-hat [0 0 1])
          colors (map (partial g/scale zcomp) (t/colors view-t))
          ;; computations on canvas
          for-canvas (t/xform2 xform-view-to-canvas view-t)
          to-bary (t/make-to-bary2 for-canvas)]
      (when (tri-maybe-on-canvas for-canvas)
        (doseq [[x y] (t/candidate-points2 for-canvas)]
          (let [[α β γ] (to-bary [x y])]
            (when (and (< 0 α 1)
                       (< 0 β 1)
                       (< 0 γ 1))
              (.setPaint g2 (mix-color colors [α β γ]))
              (.drawLine g2 x y x y))))))))

;;;; Display

(defn render
  "Render the scene using the given mode."
  [^Graphics2D g2, mode, tris]
  (doto g2
    (.setPaint Color/BLACK)
    (.fillRect 0 0 (dec view-w) (dec view-h)))
  (let [viewpoint-tris (tris-for-viewpoint tris)]
    (println (count viewpoint-tris) "triangles not culled from viewpoint.")    
    ((:renderer mode) g2 viewpoint-tris)))

;;;; Interaction

;;TODO keyboard rotation

;;;; Modes

(def modes
  {"bary2" {:move false
           :renderer render-bary2
           :title "Barycentric interpolation in 2D"}
   "wire" {:move true
           :renderer render-wire
           :title "Interactive wireframe"}
   "shade" {:move true
            :renderer render-shade
            :title "Interactive directional flat-shaded"}})

;;;; Launch

(defn fail
  "Fail with an error message."
  [msg]
  (.println System/err msg)
  (System/exit 1))

(defn ^JComponent new-canvas
  "Make a canvas with the given size."
  [mode w h]
  (doto (proxy [JComponent] []
          (paint [^Graphics2D g] (render g mode @*tris*))
          (update [^Graphics2D g]))
    (.setDoubleBuffered true)
    (.setPreferredSize (Dimension. w h))))

(defn launch
  [mode tris]
  (dosync (ref-set *tris* tris))
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [canvas (new-canvas mode view-w view-h)]
    (doto (JFrame. (str (:title mode) " / TimMc HW4 - CS4300"))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add canvas)
      (.pack)
      (.setResizable false)
      (.setVisible true)))
  (println "Read" (count tris) "triangles."))

(defn read-dot-tri
  "Read a .tri file and return a collection of triangles."
  [filename]
  (try
    (with-open [rdr (BufferedReader. (FileReader. filename))]
      (p/parse (line-seq rdr)))
    (catch Exception e
      (fail (str "Error while parsing. " e)))))

(defn -main
  "Dispatch to one of the 3 programs."
  [& all-args]
  (let [[which infile & more-args] all-args]
    (when (seq more-args)
      (fail (str "Too many arguments. Unrecognized: " more-args)))
    (when-not (and which infile)
      (fail (str "Too few arguments. Usage: timmcHW4 bary2|wire|shade file")))
    (if-let [mode (modes which)]
      (SwingUtilities/invokeLater #(launch mode (read-dot-tri infile)))
      (fail (str "Unrecognized program name: " which)))))

