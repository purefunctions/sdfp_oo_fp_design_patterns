(ns visitor-clj.multimethod-based)

;; Typically data is represented as maps in Clojure

;; Define multimethods for display and like

(defmulti display (fn [media] (:media-type media)))
(defmulti like (fn [media] (:media-type media)))

(defn do-media-seq [media-seq action]
  (doseq [media (:media-seq media-seq)](action media)))

;; Implementations of display operation

(defmethod display :image
  [image] (println "Displaying Image"))

(defmethod display :audio
  [audio] (println "Displaying Audio"))

(defmethod display :video
  [video] (println "Displaying Video"))

(defmethod display :media-list
  [media-seq]
  (do-media-seq media-seq display)
  (println "Displaying Media List: " (:list-name media-seq)))

;; Implementations of like operation
(defmethod like :image
  [image] (println "Liking Image"))

(defmethod like :audio
  [audio] (println "Liking Audio"))

(defmethod like :video
  [video] (println "Liking Video"))

(defmethod like :media-list
  [media-seq]
  (do-media-seq media-seq like)
  (println "Liking Media List: " (:list-name media-seq)))

;; Somewhere later: Implementation of display operation for 3d media type
(defmethod display :3dmedia
  [video] (println "Displaying 3d media"))

(defn demo []
  (display {:media-type :media-list
            :media-seq [{:media-type :audio} {:media-type :video} {:media-type :image} {:media-type :3dmedia}]
            :list-name "Awesome curated media list"})
  (like {:media-type :media-list
            :media-seq [{:media-type :audio} {:media-type :video} {:media-type :image}]
            :list-name "Awesome curated media list"}))
