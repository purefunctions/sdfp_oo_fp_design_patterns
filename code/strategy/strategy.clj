(ns strategy-clj)

;; A function maker (higher order function)... returns an archive function depending on the strategy
(defn archive [extension archive-fn files archive-name]
  (str (archive-fn files) ", " archive-name "." extension))

(def archive-to-zip (partial archive "zip" (fn [files] "compressed to zip")))
(def archive-to-tar (partial archive "tar" (fn [files] "archived to tar")))

(defn demo
  [& args]
  (println (archive-to-zip ["file1" "file2" "file3"] "zipfile"))
  (println (archive-to-tar ["file1" "file2" "file3"] "tarfile")))
