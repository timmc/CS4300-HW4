(ns timmcHW4.core
  (:require timmcHW4.flat timmcHW4.wire timmcHW4.shade)
  (:import [java.io BufferedReader FileReader]
           [java.awt Color Graphics2D Dimension]
           [java.awt.geom AffineTransform]
           [javax.swing JFrame JComponent SwingUtilities UIManager])
  (:require [timmcHW4.parse :as p]
            [timmcHW4.tri :as t]
            [timmcHW4.geom :as g])
  (:gen-class))

;;;; Geometry

(defn de-dim
  "Read a Dimension object into a 2-vector of width, height."
  [^Dimension d]
  [(.width d) (.height d)])

;;;; Canvas geometry

(def view-w 512)
(def view-h 512)

(def ^AffineTransform xform-view-to-canvas
  (doto (AffineTransform.)
    (.translate (/ view-w 2) (/ view-h 2))
    (.scale 1 -1)))

(def *xform-world-to-view*
  (ref (AffineTransform.)))

;;;; State

(def *tris* (ref nil))

;;;; Display

(defn tri-maybe-on-canvas
  "Return true if at least part of a triangle is possibly on the canvas.
   Tests for intersection of triangle's bounding box with canvas."
  [tri]
  (let [[[xmin xmax] [ymin ymax]] (t/bounds2 tri)]
    ;; standard axis-aligned rectangle intersection test.
    (and (<= 0 xmax)
         (< xmin view-w)
         (<= 0 ymax)
         (< ymin view-h))))

(defn render
  "Render the scene using the given mode."
  [^Graphics2D g2, mode, tris]
  (doto g2
    (.setPaint Color/BLACK)
    (.fillRect 0 0 (dec view-w) (dec view-h)))
  ;; TODO: Also filter out triangles outside the display area
  ;; This will require doing the viewpoint->canvas transform.
  (let [displayable (filter (comp g/is-CCW2? t/vertices)
                            (map (partial t/xform2 @*xform-world-to-view*)
                                 @*tris*))
        viewable (filter tri-maybe-on-canvas
                         (map (partial t/xform2 xform-view-to-canvas)
                              displayable))]
    (println (count displayable) "triangles not culled from viewpoint.")
    (println (count viewable) "triangles not culled from canvas.")
    ((:renderer mode) g2 viewable)))

;;;; Interaction

;;TODO keyboard rotation

;;;; Modes

(def modes
  {"flat" {:move false
           :renderer timmcHW4.flat/render
           :title "Flat projection"}
   "wire" {:move true
           :renderer timmcHW4.wire/render
           :title "Interactive wireframe"}
   "shade" {:move true
            :renderer timmcHW4.shade/render
            :title "Interactive flat-shaded"}})

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
          (paint [^Graphics2D g] (render g mode @*tris*)))
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
      (fail (str "Too few arguments. Usage: timmcHW4 flat|wire|shade file")))
    (if-let [mode (modes which)]
      (SwingUtilities/invokeLater #(launch mode (read-dot-tri infile)))
      (fail (str "Unrecognized program name: " which)))))

