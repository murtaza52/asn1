(ns asn1.oid
  (:require [clojure.string :as str]
            [asn1.buffer :refer [read-lines]]
            [asn1.common :refer [bit-7?]]))

(defn next-node-is-in-vlq-seq?
  [current-node]
  (and (bit-7? current-node) (> current-node 127)))


;; based on java code from rosetta stone
(defn decode-vlq
  [coll]
  (reduce (fn [v encoded-val]
            (bit-or (bit-shift-left v 7) (bit-and encoded-val 0x7f)))
          0
          coll))

(decode-vlq [0x86 0x48])
;; => 840

(decode-vlq [0x82 0x37])
;; => 311

(decode-vlq [0x82 0x37 0x21]) ;; this is wrong, maybe the encoing is incorrect as given on the site.

(Long/toBinaryString 0x37)

(def oid-node-combinations
  (for [first-node (range 3) second-node (range 0 40)]
    [first-node second-node]))

(defn oid-nodes->byte
  "The logic for calculating the base 10 byte value given the first two oid nodes."
  [first-node second-node]
  (+ (* first-node 40) second-node))

(defn get-first-two-oid-nodes
  "Given an integer, returns the oid combination that satisfies it."
  [v]
  (some (fn [[first-node second-node]]
          (when (= v (oid-nodes->byte first-node second-node))
            [first-node second-node]))
        oid-node-combinations))

(get-first-two-oid-nodes 0x55)
;; => [2 5]

(defn split-with-vlq
  [coll]
  (loop [[f & r] coll
         acc     []]
    (if (or (not (seq r))
            (not (bit-7? f)))
      [(conj acc f) r]
      (recur r (conj acc f)))))

(split-with-vlq [0x82 0x37 0x82 0x15 0x14])
;; => [[130 55] (130 21 20)]

(defn get-oid
  [coll]
  (loop [[f & r] (rest coll)
         nodes (get-first-two-oid-nodes (first coll))]
    (if (not f)
      (str/join "." nodes)
      ;; check if vlq encoding has been applied
      (if (> f 127)
        (let [[vlq-nodes remaining-nodes] (split-with-vlq (cons f r))]
          (recur remaining-nodes (conj nodes (decode-vlq vlq-nodes))))
        (recur r (conj nodes f))))))

(get-oid [0x2b 0x06 0x01 0x04 0x01 0x82 0x37 0x15 0x14])
;; => "1.3.6.1.4.1.2.55.21.20"
;; => "1.3.6.1.4.1.311.21.20"

(get-oid [0x06 0x06 0x2a 0x86 0x48 0x86 0xf7 0x0d])
;; => "0.6.6.42.840.113549"

;; code mappings taken from https://holtstrom.com/michael/tools/asn1decoder.php
(def oid-code->oid-name*
  (->> "resources/oids.txt"
       read-lines
       (map (fn [line]
              (clojure.string/split line #",")))
       ((fn [lines]
          (reduce (fn [acc [oid-code oid-name]]
                    (assoc acc oid-code (keyword oid-name)))
                  {}
                  lines)))))

(defn oid-code->oid-name
  [code]
  (get oid-code->oid-name* code :mapping-not-available))

(oid-code->oid-name "1.3.6.1.4.1.311.21.20")

(defn get-oid-mapping
  [coll]
  (let [oid (get-oid coll)]
    {:oid oid
     :oid-text (oid-code->oid-name oid)}))
