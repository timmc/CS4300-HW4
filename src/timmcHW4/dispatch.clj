(ns timmcHW4.dispatch
  (:require timmcHW4.flat timmcHW4.wire timmcHW4.shade)
  (:import [java.io BufferedReader FileReader])
  (:require [timmcHW4.parse :as p])
  (:gen-class))

(def programs
  {"flat" timmcHW4.flat/start
   "wire" timmcHW4.wire/start
   "shade" timmcHW4.shade/start})

(defn fail
  "Fail with an error message."
  [msg]
  (.println System/err msg)
  (System/exit 1))

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
    (if-let [starter (programs which)]
      (starter (read-dot-tri infile))
      (fail (str "Unrecognized program name: " which)))))

