(ns asn1.buffer
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [clojure.pprint  :refer [pprint]])
  (:import java.io.RandomAccessFile
           java.nio.ByteBuffer))

(defn read-lines
  [path]
  (doall (line-seq (io/reader path))))

(defn comment?
  [line]
  (str/starts-with? line "----"))

(defn extract-base64-string
  [coll]
  (->> coll
       (remove comment?)
       (apply str "")))

;; for java8
(defn decode-base64-str
  [^String b64-str]
  (.decode (java.util.Base64/getDecoder) b64-str))

;; for java7
;; (defn decode-base64-str
;;   [^String b64-str]
;;   (javax.xml.bind.DatatypeConverter/parseBase64Binary b64-str))

(defn bytes->buffer
  [b64-bytes]
  (ByteBuffer/wrap b64-bytes))

(defn buffer->int-seq
  [bb]
  (for [i (range (.limit bb))]
    (.get bb)))

(defn file->seq
  [path]
  (->> path
       read-lines
       extract-base64-string
       decode-base64-str
       bytes->buffer
       buffer->int-seq
       (map (fn [v] (bit-and v 0xff)))))

(comment
  (read-lines "resources/keys/rsa_public.crt")
  (extract-base64-string (read-lines "resources/keys/rsa_public.crt"))
  (decode-base64-str (remove-comments (read-lines "resources/keys/rsa_public.crt")))
  (file->seq "resources/keys/rsa_public.crt"))
