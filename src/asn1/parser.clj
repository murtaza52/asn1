(ns asn1.parser
  (:require [clojure.pprint :refer [pprint]]
            [asn1.common :refer [bit-7?]]
            [asn1.tags :refer :all]
            [asn1.oid :refer :all]
            [asn1.buffer :refer :all])
  (:gen-class))

(defn get-triplet
  [[f & r]]
  (let [{:keys [length-octets length]} (get-length r)
        triplet                        {:tag     (int->tag-type f)
                                        :length  length
                                        :value (take length (drop length-octets r))}
        remaining-octets               (drop (+ length-octets length) r)
        triplet-octets                 (take (+ 1 length-octets length) (cons f r))]
    [triplet remaining-octets triplet-octets]))

(get-triplet '(6 9 42 134 72 134 247 13 1 1 11 5 0))
;; => [{:tag :object-identifier, :length 9, :value (42 134 72 134 247 13 1 1 11)} (5 0) (6 9 42 134 72 134 247 13 1 1 11)]

(declare parse-tlv)

(defn get-triplet-seq
  [coll]
  (loop [coll coll
         acc  []]
    (let [[{:keys [tag length] :as m} remaining-octets triplet-octets] (get-triplet coll)
          triplet                                                      (if (constructed-type? tag)
                                                                         [(update m :value parse-tlv)]
                                                                         (parse-tlv triplet-octets))]
      (if (not (seq remaining-octets))
        (concat acc triplet)
        (recur remaining-octets (concat acc triplet))))))

;; (get-triplet-seq '(6 9 42 134 72 134 247 13 1 1 11 5 0))
;; => ({:tag :object-identifier, :length 9, :value {:oid "1.2.840.113549.1.1.11", :oid-text :sha256WithRSAEncryption}} {:tag :null, :length 0, :value nil})

(defn parse-value
  [tag value]
  (case tag
    :sequence (get-triplet-seq value)
    :integer (parse-integer value)
    :object-identifier (get-oid-mapping value)
    :printable-string (parse-string value)
    :utf8-string (parse-string value)
    :bit-string (parse-bit-string value)
    :utc-time (parse-utc-time value)
    :octet-string (parse-octet-string value)
    :null nil
    value))

(defn parse-tlv
  [coll]
  (loop [coll coll
         acc  []]
    (let [[{:keys [tag length value]} remaining-octets] (get-triplet coll)]
      (let [triplet (assoc {:tag tag :length length} :value (parse-value tag value))]
        (if (not (seq remaining-octets))
          (conj acc triplet)
          (recur remaining-octets (conj acc triplet)))))))

:t    :l     :v
             :t   :l   :v
                       :t :l :v
(def d [0x30 8     0x30  6   0x02 1 5  0x02 1 53])

(parse-tlv d)
;; => [{:tag :sequence, :length 8, :value {:tag :sequence, :length 8, :value [{:tag :sequence, :length 6, :value [{:tag :integer, :length 1, :value (5)} {:tag :integer, :length 1, :value (53)}]}]}}]

(comment (clojure.pprint/pprint (parse-tlv data-3)))

(def parse-asn1 (comp parse-tlv file->int-seq))

(def parse-and-print (comp pprint parse-asn1))

(comment (parse-and-print "resources/keys/rsa.key")
         (parse-and-print "resources/keys/ec.pem")
         (parse-and-print "resources/keys/rsa_public.crt"))

(comment (parse-and-print "resources/keys/rsa.key")
         (parse-asn1 "resources/keys/ec.pem"))

(parse-asn1 "resources/keys/ec.pem")
;; => [{:tag :sequence, :length 119, :value ({:tag :integer, :length 1, :value 1} {:tag :octet-string, :length 32, :value "57f131f5a61d1a8376ccc3749055f478371cc31d12b549eeb5bacd7bc15ee3"} {:tag :context-specific-tag, :length 10, :value [{:tag :object-identifier, :length 8, :value {:oid "1.2.840.10045.3.1.7", :oid-text :P-256}}]} {:tag :context-specific-tag, :length 68, :value [{:tag :bit-string, :length 66, :value "0100110100010011110001001110001110010100010011001100110111101111011001011000100010011010101100111101111001011110110100110101001110111001111001001110000010101110111011111010010011100101100001100101001001001000010110000111100100011110010011101001111001011110010110111001010010111100011111101010011001111010010010011011000110001100110101111110111011001101011111100001110101100111010111010001010100101100101110110110101111111001111001101010011101011"}]})}]


(defn -main [& args]
  (if-let [key-path (first args)]
    (pprint (parse-asn1 (comp bytes->buffer key-path)))
    (binding [*out* *err*]
      (println "no path given")
      (System/exit 1))))

