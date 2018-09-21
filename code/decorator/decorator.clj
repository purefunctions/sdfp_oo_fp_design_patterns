(ns decorator-clj)

(defn contrast-filter
  [factor image]
  (str image " + Contrast factor " factor))

(defn monochrome-filter
  [image]
  (str image " + Monochrome conversion"))

(defn edge-detection-filter
  [image]
  (str image " + Edge detection"))

(defn combine-filters
  [filters]
  (apply comp (reverse filters)))

(defn demo
  [& args]
  ;; Static version
  (println
    (-> "Mosaic image"
        monochrome-filter
        ((partial contrast-filter 2.5))
        edge-detection-filter)))
