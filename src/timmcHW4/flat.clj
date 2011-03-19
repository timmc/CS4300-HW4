(ns timmcHW4.flat
  "2D triangle renderer program with barycentric interpolation."
  (:require [timmcHW4.geom :as g]
            [timmcHW4.tri :as t])
  (:import [java.awt Color Dimension Graphics2D]
           [java.awt.geom AffineTransform Point2D Point2D$Double]
           [javax.swing JFrame JComponent SwingUtilities UIManager]))


;;;; Geometry

(defn de-dim
  "Read a Dimension object into a 2-vector of width, height."
  [^Dimension d]
  [(.width d) (.height d)])

(defn ^Point2D pt
  "Make a Point2D object from x and y coordinates."
  [x y]
  (Point2D$Double. x y))

(defn de-pt
  "Read a Point2D object into a 2-vector of x, y."
  [^Point2D p]
  [(.getX p) (.getY p)])

(def view-w 512)
(def view-h 512)

(def ^AffineTransform xform-to-view
  (doto (AffineTransform.)
    (.translate (/ view-w 2) (/ view-h 2))
    (.scale 1 -1)))
(def ^AffineTransform xform-to-world
  (.createInverse xform-to-view))

(defn to-world
  "Take an [x y] from view to world coords."
  [[x y & _]]
  (de-pt (.transform xform-to-world (pt x y) nil)))

(defn to-view
  "Take an [x y] from world to view coords."
  [[x y & _]]
  (de-pt (.transform xform-to-view (pt x y) nil)))

;;;; State

(def *tris* (ref nil))

;;;; Display

(defn render-flat
  [^Graphics2D g2, tris]
  nil)

(defn render-wire
  "Render a wireframe scene. Provided triangles should be in view coordinates
   and already have been backface-culled."
  [^Graphics2D g2, tris]
  (.setPaint g2 Color/WHITE)
  (doseq [t tris]
    (doseq [e (t/edges t)]
      (let [[x1 y1 x2 y2] (flatten e)]
        (.draw g2 (java.awt.geom.Line2D$Double. x1 y1 x2 y2))))))

(defn render-shade
  [^Graphics2D g2, tris]
  nil)

(defn render
  "Render the scene."
  [^Graphics2D g2]
  (doto g2
    (.setPaint Color/BLACK)
    (.fillRect 0 0 (dec view-w) (dec view-h)))
  (let [displayable (filter (comp g/is-CCW2? t/vertices)
                            (map (partial t/xform2 to-view) @*tris*))]
    (println "After backface culling:" (count displayable))
    (render-wire g2 displayable)))

;;;; Modes

(def modes
  {:flat {:move false :renderer render-flat}
   :wire {:move true :renderer render-wire}
   :shade {:move true :renderer render-shade}})

;;;; Interaction

(defn ^JComponent new-canvas
  "Make a canvas with the given size."
  [w h]
  (doto (proxy [JComponent] []
          (paint [^Graphics2D g] (render g)))
    (.setDoubleBuffered true)
    (.setMinimumSize (Dimension. w h))
    (.setPreferredSize (Dimension. w h))))

(defn launch
  [tris]
  (dosync (ref-set *tris* tris))
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [canvas (new-canvas view-w view-h)]
    (doto (JFrame. "TimMc HW4 - CS4300")
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add canvas)
      (.pack)
      (.setResizable false)
      (.setVisible true)))
  (println "Recieved" (count tris) "triangles"))

(defn start
  "Start an instance on the event loop."
  [tris]
  (SwingUtilities/invokeLater #(launch tris)))
