(ns asn1.buffer
  (:require [clojure.string  :as str]
            [clojure.java.io :as io]
            [clojure.pprint  :refer [pprint]])
  (:import java.io.RandomAccessFile
           java.nio.ByteBuffer
           java.util.Base64))

(defn read-lines
  [path]
  (doall (line-seq (io/reader path))))

(comment
  (read-lines "resources/keys/rsa_public.crt"))

(defn comment?
  [line]
  (str/starts-with? line "----"))

(defn extract-base64-string
  [coll]
  (->> coll
       (remove comment?)
       (apply str "")))

(comment
  (extract-base64-string (read-lines "resources/keys/rsa_public.crt")))

(defn base64-bytes
  [^String b64-str]
  (.decode (java.util.Base64/getDecoder) b64-str))

(comment
  (base64-bytes (remove-comments (read-lines "resources/keys/rsa_public.crt"))))

(defn bytes->buffer
  [b64-bytes]
  (ByteBuffer/wrap b64-bytes))

(defn buffer->int-seq
  [bb]
  (for [i (range (.limit bb))]
    (.get bb)))

(defn file->int-seq
  [path]
  (->> path
       read-lines
       remove-comments
       base64-bytes
       bytes->buffer
       buffer->int-seq
       (map (fn [v] (bit-and v 0xff)))))

(comment
  (def data (file->int-seq "resources/keys/rsa_public.crt")))

