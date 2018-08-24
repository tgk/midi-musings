(ns midi-musings.mother-32
  (:require [midi-musings.levenshtein :as l])
  (:import [javax.sound.midi MidiSystem ShortMessage]))

(defn mother-32-device
  []
  (let [device-info (last (filter #(= "CH345 [hw:1,0,0]" (.getName %))
                                  (MidiSystem/getMidiDeviceInfo)))]
    (MidiSystem/getMidiDevice device-info)))

(defn play-notes
  [notes]
  (with-open [d (doto (mother-32-device) .open)]
      (let [receiver (.getReceiver d)]
        (doseq [note notes]
          (when note
            (.send receiver
                   (doto (ShortMessage.)
                     (.setMessage ShortMessage/NOTE_ON note 127))
                   -1))
          (Thread/sleep 100)
          (when note
            (.send receiver
                   (doto (ShortMessage.)
                     (.setMessage ShortMessage/NOTE_OFF note 0))
                   -1))
          (Thread/sleep 100)))))

(defn midi-offsets
  [tone-pattern]
    (->> tone-pattern
         (map {:semitone 1
               :tone 2
               :minor-third 3})
         cycle
         (cons 0)
         (take 64)
         (reductions +)))

(def major-scale
  (midi-offsets
   [:tone :tone :semitone :tone :tone :tone :semitone]))
(def harmonic-minor-scale
  (midi-offsets
   [:tone :semitone :tone :tone :semitone :minor-third :semitone]))
(def natural-minor-scale
  (midi-offsets
   [:tone :semitone :tone :tone :semitone :tone :tone]))
(def locrian-mode
  (midi-offsets
   [:semitone :tone :tone :semitone :tone :tone :tone]))
(def mixolydian-mode
  (midi-offsets
   [:tone :tone :semitone :tone :tone :semitone :tone]))

(defn triad-chords
  "Returns colls of midi notes in the scale."
  [scale]
  (partition 3 1 scale))

(triad-chords major-scale)

(defonce melody
  (atom nil))

;; todo: don't deref twice
(defn start-player
  [a]
  (future
    (while @a
      (play-notes @a))))

(comment

  ;; completely random notes and gaps
  (reset! melody
          (take 10 (random-sample 0.01
                                  (cycle (concat
                                          (repeat 15 nil)
                                          (range 128))))))

  ;; random from major-scale
  (reset! melody
          (take 16 (random-sample 0.01
                                 (cycle (concat
                                         (repeat 10 nil)
                                         major-scale)))))

  (def a (take 8 (random-sample 0.01
                                (cycle (concat
                                        (repeat 10 nil)
                                        major-scale)))))

  (def b (take 8 (random-sample 0.01
                                (cycle (concat
                                        (repeat 10 nil)
                                        major-scale)))))

  (def a (take 8 (random-sample 0.01
                                (cycle (concat
                                        (repeat 5 nil)
                                        (subvec (vec major-scale) 30 40))))))

  (def b (take 8 (random-sample 0.01
                                (cycle (concat
                                        (repeat 5 nil)
                                        (subvec (vec major-scale) 30 40))))))

  (reset! melody a)
  (reset! melody b)

  (reset! melody
          (flatten (concat
                    (reverse (l/edit-trace a b))
                    (l/edit-trace a b))))

  (reset! melody nil)

  (start-player melody)

  (defn safe--
    [num1 num2]
    (when (and num1 num2)
      (- num1 num2)))

  (defn safe-+
    [num1 num2]
    (when (and num1 num2)
      (+ num1 num2)))

  (let [bass-note 32]
    (reset! melody [bass-note nil
                    bass-note nil
                    bass-note nil
                    bass-note bass-note]))

  (swap! melody (partial map #(safe-+ % 2)))
)

(comment
  (doseq [chord (triad-chords (map (partial + (* 12 2)) major-scale))]
    (play-notes chord)
    (Thread/sleep 200))

  (def a
    (let [scale mixolydian-mode]
            (concat
             (nth (triad-chords (map (partial + (* 12 4)) scale)) 1)
             [nil]
             (nth (triad-chords (map (partial + (* 12 4)) scale)) 1)
             [nil]
             (nth (triad-chords (map (partial + (* 12 3)) scale)) 6)
             [nil]
             (nth (triad-chords (map (partial + (* 12 3)) scale)) 4)
             [nil])))

  (def b
    (let [scale harmonic-minor-scale]
            (concat
             (nth (triad-chords (map (partial + (* 12 4)) scale)) 1)
             [nil]
             (nth (triad-chords (map (partial + (* 12 4)) scale)) 1)
             [nil]
             (nth (triad-chords (map (partial + (* 12 3)) scale)) 6)
             [nil]
             (nth (triad-chords (map (partial + (* 12 3)) scale)) 4)
             [nil])))

  (reset! melody a)
  (reset! melody b)

  (reset! melody
          (let [scale mixolydian-mode]
            (flatten (concat
                    (reverse (l/edit-trace a b))
                    (l/edit-trace a b)))))

  (start-player melody)
  (reset! melody [])
  (reset! melody nil)
  )

(comment
  (play-notes (map (partial + (* 12 2) 6)  major-scale)))

(comment

  (clojure.pprint/pprint (for [d (MidiSystem/getMidiDeviceInfo)]
                           {:name (.getName d)
                            :description (.getDescription d)
                            :string (.toString d)}
                           ))

  )
