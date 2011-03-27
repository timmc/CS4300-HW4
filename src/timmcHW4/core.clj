(ns timmcHW4.core
  "Implements triangle mesh rendering as specified in
   <http://www.ccs.neu.edu/course/cs4300/HW5/HW5.html>.
   See README for usage."
  (:import [java.io BufferedReader FileReader]
           [java.awt Color Graphics2D Dimension]
           [java.awt.image BufferedImage]
           [java.awt.event KeyEvent KeyAdapter]
           [java.awt.geom AffineTransform Line2D$Double]
           [javax.swing JFrame JComponent SwingUtilities UIManager])
  (:require [timmcHW4.parse :as p]
            [timmcHW4.tri :as t]
            [timmcHW4.geom :as g])
  (:gen-class))

;; Thanks, amalloy! https://gist.github.com/c93c37345c49c47dcfa2
(defmacro ?
  "A useful debugging tool when you can't figure out what's going on:
wrap a form with ?, and the form will be printed alongside
its result. The result will still be passed along."
  [val]
  `(let [x# ~val]
     (prn '~val '~'is x#)
     x#))

;;;; State

(def ^{:doc "Original triangles as loaded by parser."}
  *orig-tris* (ref nil))
;; These must be appropriate for bary2 mode.
(def ^{:doc "Rotation around the Z axis."}
  *rot-Z* (ref 0))
(def ^{:doc "Rotation around the X axis."}
  *rot-X* (ref 0))
(def ^{:doc "Rotation around the Y axis."}
  *rot-Y* (ref 0))
(def ^{:doc "Scaling from origin."}
  *scale* (ref 1))
(def ^{:doc "Triangles after rotation. Nil if rotation has changed."}
  *view-tris* (ref nil))
(def ^{:doc "Light source vector (opposite of direction of rays)."}
  *light-vect* (ref (g/unit [0.3 1 0.5])))

;;;; Transformations

(def view-w 512)
(def view-h 512)

(def ^AffineTransform xform-view-to-canvas
  (doto (AffineTransform.)
    (.translate (/ view-w 2) (/ view-h 2))
    (.scale 1 -1)))

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

;; TODO: For performance, experiment with separating colors and vertices,
;;       then using transients or even arrays for faster recomputation.
(defn tris-for-viewpoint
  "Map world triangles to viewpoint, adding orientation vectors and culling
   backface elements."
  [tris]
  ;; TODO: investigate why this negation is required for CCW z rotation
  (let [tmat (g/rotscale-ZXYs (- @*rot-Z*) @*rot-X* @*rot-Y* @*scale*)]
    (filter t/front-face3z?
            (map #(t/orient (t/xform3 % tmat)) tris))))

(defn tris-for-canvas
  "Map viewpoint triangles to canvas, culling offscreen elements."
  [tris]
  (filter tri-maybe-on-canvas
          (map (partial t/xform2 xform-view-to-canvas)
               tris)))

;;;; Updating

(defn ensure-view-tris
  "Get viewpoint triangles, recalculating if necessary."
  []
  (dosync
   (ensure *rot-Z*)
   (ensure *rot-X*)
   (ensure *rot-Y*)
   (ensure *scale*)
   (if-let [view @*view-tris*]
     view
     (ref-set *view-tris*
              (tris-for-viewpoint @*orig-tris*)))))

;;;; Renderers

(defn clamp
  "Clamp a number to within the specified closed interval."
  [minv maxv v]
  (min maxv (max minv v)))

(defn mix-color
  "Create an RGB int from a barycentric coordinates of a point and the
   float [r g b] colors at the vertices."
  [colors bary-coords]
  ;; Clamping is for possible floating point errors driving floats out of [0 1].
  ;; This is also the only check on input colors.
  ;; TODO: Investigate performance benefits of removing clamps.
  (let [rgb-floats (g/sum (map g/scale bary-coords colors))
        [ri gi bi] (map #(clamp 0 255 (int (* 255 %))) rgb-floats)]
    (bit-or (bit-shift-left ri 16)
            (bit-or (bit-shift-left gi 8)
                    bi))))

(defn render
  "Render the scene using the given mode."
  [^Graphics2D g2, mode]
  (let [bi (BufferedImage. view-w view-h BufferedImage/TYPE_INT_RGB)
        vtris (ensure-view-tris)]
    (doseq [view-t vtris]
      (let [orientation-hat (g/unit (t/orientation view-t))
            zcomp (/ (inc (g/dot orientation-hat @*light-vect*)) 2) ;; 0 to 1
            colors (map (partial g/scale zcomp) (t/colors view-t))
            for-canvas (t/xform2 xform-view-to-canvas view-t)
            to-bary (t/make-to-bary2 for-canvas)]
        (when (tri-maybe-on-canvas for-canvas)
          (doseq [[x y] (t/candidate-points2 for-canvas)]
            (let [[α β γ] (to-bary [x y])]
              (when (and (< 0 α 1)
                         (< 0 β 1)
                         (< 0 γ 1))
                (.setRGB bi x y (mix-color colors [α β γ]))))))))
    (.drawImage g2 bi nil 0 0)))

;;;; Interaction

(def press-handlers
  {KeyEvent/VK_UP #(commute *rot-X* + 0.1)
   KeyEvent/VK_DOWN #(commute *rot-X* - 0.1)
   KeyEvent/VK_LEFT #(commute *rot-Y* + 0.1)
   KeyEvent/VK_RIGHT #(commute *rot-Y* - 0.1)})

(defn handle-key-pressed
  "Handle a key pressed on the canvas. Return logical true when needs repaint."
  [^KeyEvent ke]
  (if-let [thunk (press-handlers (.getKeyCode ke))]
    (dosync (thunk)
            true)
    false))

(def type-handlers
  {\r #(do (ref-set *rot-X* 0)
           (ref-set *rot-Z* 0)
           (ref-set *rot-Y* 0))
   \1 #(ref-set *scale* 1)
   \- #(commute *scale* / 1.1)
   \+ #(commute *scale* * 1.1)})

(defn handle-key-typed
  "Handle a char typed on the canvas. Return logical true when needs repaint."
  [^KeyEvent ke]
  (if-let [thunk (type-handlers (.getKeyChar ke))]
    (dosync (thunk)
            true)
    false))

;;;; Modes

(def modes
  {"simple" {:painter false
             :zbuffer false
             :title "No special handling for concave."}
   "painter" {:painter true
              :zbuffer false
              :title "Painter's Algorithm"}
   "zbuffer" {:painter false
              :zbuffer true
              :title "zbuffer algorithm"}})

;;;; Launch

(defn fail
  "Fail with an error message."
  [msg]
  (.println *err* msg)
  (System/exit 1))

(defn invalidate-view
  "Force the view triangles to be recomputed."
  [canvas]
  (dosync (ref-set *view-tris* nil))
  (.repaint canvas))

(defn ^JComponent new-canvas
  "Make a canvas with the given size."
  [mode w h]
  (let [canvas (proxy [JComponent] []
                 (paint [^Graphics2D g]
                   (let [start (System/currentTimeMillis)]
                     (render g mode)
                     (let [end (System/currentTimeMillis)]
                       (println "Rendered in" (- end start) "ms"))))
                 (update [^Graphics2D g]))]
    (doto canvas
      (.setDoubleBuffered true)
      (.setPreferredSize (Dimension. w h))
      (.addKeyListener (proxy [KeyAdapter] []
                         (keyPressed [^KeyEvent ke]
                           (when (handle-key-pressed ke)
                             (invalidate-view canvas)))
                         (keyTyped [^KeyEvent ke]
                           (when (handle-key-typed ke)
                             (invalidate-view canvas))))))))

(defn launch
  [mode tris]
  ;; TODO: Make mode a global so we can switch renderers mid-program?
  (dosync (ref-set *orig-tris* tris))
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [canvas (new-canvas mode view-w view-h)]
    (doto (JFrame. (str (:title mode) " / TimMc HW4 - CS4300"))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add canvas)
      (.pack)
      (.setResizable false)
      (.setVisible true))
    (.requestFocus canvas))
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
      (fail (str "Too few arguments. "
                 "Usage: timmcHW5 simple|painter|zbuffer file")))
    (if-let [mode (modes which)]
      (SwingUtilities/invokeLater #(launch mode (read-dot-tri infile)))
      (fail (str "Unrecognized program name: " which)))))

