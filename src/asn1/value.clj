(ns asn1.value
  (:require [asn1.common :refer [to-hex]]
            [asn1.oid :as oid]))

(defn parse-integer
  [coll]
  (biginteger (byte-array coll)))

(defn parse-string
  [coll]
  (String. (byte-array coll)))

(defn parse-bit-string
  [coll]
  (apply str (map #(Long/toBinaryString %) coll)))

(defn get-this-year
  []

  ;; to be used when java 8 dependency can be pulled in
  ;; (Integer/parseInt (.toString (java.time.Year/now)))

  2019)


;; code taken from https://github.com/dankreek/clj-asn1/blob/master/src/clj_asn1/core.clj
(defn calculate-century
  [year-end]
  (let [this-year  (get-this-year)
        high-bound (+ this-year 49)
        this-cent  (Integer/parseInt (.substring (str this-year) 0 2))]
    (if (< high-bound (+ (* 100 this-cent) year-end))
      (dec this-cent)
      this-cent)))

(defn format-date
  [coll]
  (let [year-end (Integer/parseInt (first coll))
        century (calculate-century year-end)]
    (apply format "%s%s-%s-%sT%s:%s:%s.Z" (cons century coll))))

(defn parse-utc-time
  [coll]
  (->> coll
       (partition 2)
       (map #(parse-string %))
       format-date))

(defn parse-octet-string
  [coll]
  (->> coll
       to-hex
       (apply str)))

(comment
  (parse-utc-time '(50 48 49 48 50 51 49 52 49 53 52 49 90))
  (calculate-century 19)
  (parse-string '(75 97 109 112 97 108 97))
  (parse-string [0x55 0x47])
  (parse-integer [0x29 0x42 0xde 0x86 0xc2 0x0a 0xcc 0xfb 0x96 0x9f 0x24 0x5f 0xa6 0x8c 0x65 0x1b 0x5d 0x1f 0x88 0xf1])
  (parse-octet-string '(87 241 49 245 166 29 26 131 118 204 195 116 144 85 15 4 120 55 28 195 29 18 181 73 238 181 186 205 123 193 94 227)))
