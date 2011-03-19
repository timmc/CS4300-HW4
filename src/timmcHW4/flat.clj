(ns timmcHW4.flat
  "2D triangle renderer program with barycentric interpolation."
  (:import [java.awt Dimension]
           [java.awt.geom AffineTransform Point2D Point2D$Double]
           [javax.swing SwingUtilities UIManager]))


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

(def xform-to-view
  (doto (AffineTransform.)
    (.translate (/ view-w 2) (/ view-h 2))
    (.scale 1 -1)))
(def xform-to-world
  (.createInverse xform-to-view))

(defn to-world
  "Take an [x y] from view to world coords."
  [[x y]]
  (de-pt (.transform xform-to-world (pt x y))))

(defn to-view
  "Take an [x y] from world to view coords."
  [[x y]]
  (de-pt (.transform xform-to-view (pt x y))))

;;;; Interaction

(defn launch
  [tris]
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (println (count tris)))

(defn start
  "Start an instance on the event loop."
  [tris]
  (SwingUtilities/invokeLater #(launch tris)))
