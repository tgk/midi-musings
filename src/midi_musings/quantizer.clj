(ns midi-musings.quantizer
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
          (Thread/sleep 150)
          (when note
            (.send receiver
                   (doto (ShortMessage.)
                     (.setMessage ShortMessage/NOTE_OFF note 0))
                   -1))
          (Thread/sleep 150)))))

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

(def scales
  [major-scale harmonic-minor-scale natural-minor-scale locrian-mode mixolydian-mode])

(defonce melody
  (atom nil))

;; todo: don't deref twice
(defn start-player
  [a]
  (future
    (while @a
      (play-notes @a))))

(defn quantize
  [scale note]
  (apply min-key (fn [scale-note] (Math/abs (- note scale-note))) scale))

(comment

  ;; completely random notes and gaps
  (reset! melody
          (take 8 (random-sample 0.01 (cycle (range 128)))))
  
  ;; quantized
  (defn sample
    []
    (->> (cycle (range 35 48))
         (random-sample 0.01)
         (take 8)
         (map (partial quantize (rand-nth scales)))))
  
  (reset! melody (sample))

  (def a (sample))
  (def b (sample))

  (reset! melody a)
  (reset! melody b)

  (reset! melody [76 76 76 0])

  (reset! melody
          (flatten (concat
                    a
                    a
                    a
                    (reverse (l/edit-trace a b))
                    b
                    b
                    b
                    (l/edit-trace a b))))

  (reset! melody nil)

  (start-player melody)

  (reset! melody
          (map (set major-scale)
               (repeatedly 8 (partial rand-nth (range 40 50)))))

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
