(ns alphabet-cipher.coder)

(declare decycle)

;; most things have keyword/message while in this case though I call something keyword mess there is no keyword I am guessing it, therefore
;; i am inclined to rename the variables to message and cipher
(defn decipher-letter
  "decrypt the key keyword is really unencrypted stream and mess is really encrypted stream"
  [tuple]
  (let [[keyword mess] tuple]
    (let [keychar (- (int keyword) (int \a))
          messchar (- (int mess) (int \a))]
      (char (+
             (int \a)
             (mod (- (+ messchar 26) keychar) 26))))))


(defn decode-letter
  "decode individual character of message"
  [tuple]
  (let [[keyword mess] tuple]
    (let [keychar (- (int keyword) (int \a))
          messchar (- (int mess) (int \a))]
      (char (+
             (int \a)
             (mod (- (+ messchar 26) keychar) 26))))))


(defn encode-letter
  "encode individiual character of message"
  [tuple]
  (let [[keyword mess] tuple]
    (let [keychar (- (int keyword) (int \a))
          messchar (- (int mess) (int \a))]
      (char (+
             (int \a)
             (mod (+ keychar messchar) 26))))))

(defn packageStream [keyword message]
  "package things up nicely"
  (map vector (cycle keyword) message))

(defn encode [keyword message]
  (let [stream (packageStream keyword message)]
    (apply str (map encode-letter stream))))

(defn decode [keyword message]
  (let [stream (packageStream keyword message)]
    (apply str (map decode-letter stream))))

(defn decipher [cipher message]
  (let [stream (packageStream message cipher)]
    (decycle
     (apply str (map decipher-letter stream)))))

(defn find-all [string character]
  (filter
    #(> % -1)
   (map-indexed (fn [idx str-char]
                  (if (= str-char character)
                    idx
                    -1))
                string)))

(defn partition-string [string indexSeq]
  (let [end-substring-seq (lazy-cat (rest indexSeq) [(count string)])
        start-substring-seq indexSeq]
    (map (fn [startIndex endIndex]
           (subs string startIndex endIndex))
         start-substring-seq
         end-substring-seq)))

(defn check-cycle [string cycattempt]
  (let [mis-match (filter (fn [a] (not a)) (map (fn [a b] (= a b)) string (cycle cycattempt)))]
    (if (seq mis-match)
      false
      true)))

(defn try-cycles [string indexed-seq]
  (loop [thusFar (first indexed-seq)
         remaining (rest indexed-seq)]
    (if (empty? remaining)
      thusFar
      (if (check-cycle string thusFar)
        thusFar
        (recur (str thusFar (first remaining)) (rest remaining))))))

(defn decycle [cycled-message]
  (if (clojure.string/blank? cycled-message)
    cycled-message
    (let [firstSeq (find-all cycled-message (first cycled-message))
          partitioned-string (partition-string cycled-message firstSeq)]
      (try-cycles cycled-message partitioned-string))))

