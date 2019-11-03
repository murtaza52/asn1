(ns asn1.tags
  (:import java.time.Instant
           java.time.Year))

(def basic-types
  {0x03 :bit-string
   0x01 :boolean
   0x02 :integer
   0x05 :null
   0x06 :object-identifier
   0x04 :octet-string})

(def string-types
  {0x1E :bmp-string
   0x16 :ia5-string
   0x13 :printable-string
   0x14 :teletex-string
   0x0C :utf8-string})

(def constructed-types
  {0x30 :sequence
   0x31 :set})

(def misc
  {0x17 :utc-time})

(defn int->tag-type
  [n]
  (get
   (merge basic-types string-types constructed-types misc)
   n
   :tag-not-found))

(int->tag-type 0x30)

(defn bit-6-0?
  "Returns true if the 6th bit is set to 0."
  [b]
  (= 0 (bit-and 2r00000100 b)))

(bit-6-0? 2r10101010)
;; => true

(bit-6-0? 2r10101110)
;; => false

(defn tag-value
  [b]
  (bit-and 2r11111000 b))

(Long/toBinaryString (bit-shift-right 2r10101000 3))

(defn get-tag-bytes
  "Returns the tag value represented by the first 5 bits"
  [b]
  (bit-shift-right b 3))

(Long/toBinaryString (get-tag-bytes 2r10101000))

(Long/toHexString (get-tag-bytes 0x30))

;;;; primitive definite length encoding for simple types ;;;;

;;; identifier octets

;; low-tag-number form (the high-tag-number-form is not coded, because in simple types we do not have tags above 30)

(defn add-big-endians
  [v1 v2]
  (bit-or (bit-shift-left v1 8) v2))

(add-big-endians 0x03 0x0d)

(defn short-form-length?
  "If the 8th bit is 0 then it is short form."
  [v]
  (= 0 (bit-shift-right v 7)))

(defn long-form-length?
  "If the 8th bit is 1 then its long form."
  [v]
  (= 1 (bit-shift-right v 7)))

(long-form-length? 0x82)

(defn long-form-num-of-octets
  "The 1-7 bits represent the num of octets"
  [v]
  (bit-and v 2r01111111))

(long-form-num-of-octets 0x82)
;; => 2

(defn calculate-long-form-length
  [length-octets]
  (reduce add-big-endians length-octets))

(calculate-long-form-length [0x03 0x0D])

(defn get-length
  [[f & r]]
  (cond
    (short-form-length? f) {:length-octets 1
                            :length f}
    (long-form-length? f) (let [num-of-octets (long-form-num-of-octets f)
                                length (calculate-long-form-length (take num-of-octets r))]
                            {:length-octets (+ 1 num-of-octets)
                             :length length})))
;; account for indefinite length
(get-length [0x82 0x03 0x0D])

;; account for a tag that is not universal


(defn parse-integer
  [coll]
  (biginteger (byte-array coll)))

(def integer-data [0x29 0x42 0xde 0x86 0xc2 0x0a 0xcc 0xfb 0x96 0x9f 0x24 0x5f 0xa6 0x8c 0x65 0x1b 0x5d 0x1f 0x88 0xf1])

(parse-integer integer-data)
;; => 235559855570120771496712017082493683142938822897

(def printable-string-data [0x55 0x47])

(defn parse-string
  [coll]
  (String. (byte-array coll)))

(parse-string printable-string-data);; => "UG"

(def utf8-string-data '(75 97 109 112 97 108 97))

(parse-string utf8-string-data)
;; => "Kampala"

(defn parse-bit-string
  [coll]
  (apply str (map #(Long/toBinaryString %) coll)))

(defn get-this-year
  []
  (Integer/parseInt (.toString (Year/now))))

;; code taken from https://github.com/dankreek/clj-asn1/blob/master/src/clj_asn1/core.clj
(defn calculate-century
  [year-end]
  (let [this-year  (get-this-year)
        high-bound (+ this-year 49)
        this-cent  (Integer/parseInt (.substring (str this-year) 0 2))]
    (if (< high-bound (+ (* 100 this-cent) year-end))
      (dec this-cent)
      this-cent)))

(calculate-century 19)

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
       format-date
       (Instant/parse)))

(def utc-time-data '(50 48 49 48 50 51 49 52 49 53 52 49 90))

(parse-utc-time utc-time-data)
