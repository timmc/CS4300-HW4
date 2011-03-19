(ns timmcHW4.shade
  "3D flat-shaded triangle renderer."
  (:import [javax.swing SwingUtilities UIManager]))

(defn launch
  [tris]
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (println (count tris)))

(defn start
  "Start an instance on the event loop."
  [tris]
  (SwingUtilities/invokeLater #(launch tris)))
