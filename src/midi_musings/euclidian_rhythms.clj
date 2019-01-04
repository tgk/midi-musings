(ns midi-musings.euclidian-rhythms
  (:import [javax.sound.midi MidiSystem ShortMessage]))

(defn mother-32-device
  []
  (let [device-info (last (filter #(= "CH345 [hw:1,0,0]" (.getName %))
                                  (MidiSystem/getMidiDeviceInfo)))]
    (MidiSystem/getMidiDevice device-info)))

(defn play-notes
  [notes-seq]
  (with-open [d (doto (mother-32-device) .open)]
      (let [receiver (.getReceiver d)]
        (doseq [notes notes-seq]
          (doseq [note notes]
            (.send receiver
                   (doto (ShortMessage.)
                     (.setMessage ShortMessage/NOTE_ON note 127))
                   -1))
          (Thread/sleep 150)
          (doseq [note notes]
            (when note
              (.send receiver
                     (doto (ShortMessage.)
                       (.setMessage ShortMessage/NOTE_OFF note 0))
                     -1)))
          (Thread/sleep 150)))))

(defonce melody
  (atom nil))

;; todo: don't deref twice
(defn start-player
  [a]
  (future
    (while @a
      (play-notes @a))))

(defn shift
  [n melody]
  (take (count melody) (drop n (cycle melody))))

(defn fill-pattern
  [beats]
  (cons 1 (repeat (dec beats) nil)))

(defn euclidian-pattern
  [beats n]
  (let [segment (quot beats n)
        extras (rem beats n)]
    (flatten
     (concat
      (for [i (range extras)]
        (fill-pattern (inc segment)))
      (for [i (range (- n extras))]
        (fill-pattern segment))))))

(defn safe
  [op]
  (fn [& args]
    (when (every? (comp not nil?) args)
      (apply op args))))

(defn pattern->midi-notes
  [pattern note]
  (map (partial (safe *) note) pattern))

(defn midi-notes->colls
  [notes]
  (for [note notes] (if note [note] [])))

(defn merge-note-colls
  [note-colls]
  (apply map concat note-colls))

(defn euclidian-melody
  [root ns shifts]
  (merge-note-colls
   (let [notes [root (+ root 4) (+ root 7) (+ root 11)]]
     (for [[note n s] (map vector notes ns shifts)]
       (midi-notes->colls
        (shift
         s
         (pattern->midi-notes
          (euclidian-pattern 16 n)
          note)))))))

(def c-root
  [60 64 67 71])

(comment

  (start-player melody)

  (reset! melody nil)

  ;; completely random notes and gaps
  (reset! melody
          (take 8 (random-sample 0.01 (cycle (range 128)))))

  (reset! melody (euclidian-melody 60 [7 6 5 4] [0 1 2 3]))

  (reset! melody (euclidian-melody 40 [4 3 2 1] [0 1 2 5]))


  (reset! melody
          (merge-note-colls
           [(midi-notes->colls
              (pattern->midi-notes
               (euclidian-pattern 16 4)
               (first c-root)))
            (midi-notes->colls
             (pattern->midi-notes
              (euclidian-pattern 16 3)
              (second c-root)))
            (midi-notes->colls
             (pattern->midi-notes
              (euclidian-pattern 16 2)
              (nth c-root 3)))])
          )


)
