(ns visitor-clj.protocol-based)

;; Protocols
(defprotocol MediaDisplay
  (display [this] "Displays media"))

(defprotocol MediaLike
  (like [this] "Registers media like"))

;; Data types
(defrecord MediaList [media-seq list-name])
(defrecord ImageMedia [])
(defrecord AudioMedia [])
(defrecord VideoMedia [])

;; Somewhere else in the code, without having to change the protocol or data types
;; Helper function to run an action on each media in the media list
(defn do-media-seq [media-seq action]
  (doseq [media (:media-seq media-seq)](action media)))


;; Independently implement MediaDisplay protocol for one or more types
(extend-type ImageMedia
  MediaDisplay
  (display [this] (println "Displaying Image")))

(extend-type AudioMedia
  MediaDisplay
  (display [this] (println "Displaying Audio")))

(extend-type VideoMedia
  MediaDisplay
  (display [this] (println "Displaying Video")))

(extend-type MediaList
  MediaDisplay
  (display [this]
    (do-media-seq this display)
    (println "Displaying Media List: " (:list-name this))))

;; Independently implement MediaLike protocol for one or more types
(extend-type ImageMedia
  MediaLike
  (like [this] (println "Liking Image")))

(extend-type AudioMedia
  MediaLike
  (like [this] (println "Liking Audio")))

(extend-type VideoMedia
  MediaLike
  (like [this] (println "Liking Video")))

(extend-type MediaList
  MediaLike
  (like [this]
    (do-media-seq this like)
    (println "Liking Media List: " (:list-name this))))

;; Now, we want to add another data type called 3D that is only displayable, not likable
(defrecord ThreeDMedia [])

(extend-type ThreeDMedia
  MediaDisplay
  (display [this] (println "Displaying 3d media")))

(defn demo []
  (display (MediaList. [(ImageMedia.) (AudioMedia.) (ImageMedia.) (VideoMedia.) (ThreeDMedia.)] "An Awesome curated media list"))
  (like (MediaList. [(ImageMedia.) (AudioMedia.) (ImageMedia.) (VideoMedia.)] "An Awesome curated media list")))

