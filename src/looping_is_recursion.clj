(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
   (let [helper (fn [fseq previ]
                 (if (empty? fseq)
                   previ
                   (recur (rest fseq) (first fseq))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                   (and (empty? a-seq) (empty? b-seq)) true
                   (or (empty? a-seq) (empty? b-seq)) false
                   (not= (first a-seq) (first b-seq)) false
                   :else (recur (rest a-seq) (rest b-seq))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         sq a-seq]
    (cond
      (empty? sq) nil
      (pred (first sq)) acc
      :else (recur (inc acc) (rest sq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         sq a-seq]
    (if (empty? sq)
      (/ sum n)
      (recur (+ sum (first sq)) (inc n) (rest sq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [output #{}
         sq a-seq]
    (if (empty? sq)
      output
      (recur (toggle output (first sq)) (rest sq)))))

(defn fast-fibo [n]
  (loop [i 2
         i-1 1
         i-2 0]
    (cond
      (= n 0) 0
      (= n 1) 1
      (= n i) (+ i-2 i-1)
      :else (recur (inc i) (+ i-2 i-1) i-1))))

(defn cut-at-repetition [a-seq]
  (loop [output []
         sq a-seq]
    (if (or (empty? sq) (contains? (set output) (first sq)))
      output
      (recur (conj output (first sq)) (rest sq)))))

