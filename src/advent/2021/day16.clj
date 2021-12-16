(ns advent.2021.day16
  (:require [clojure.pprint :as pprint]
            [plumbing.core :refer :all]))

(def one-literal "D2FE28")
(def subpackets-by-num-bits "38006F45291200")
(def subpackets-by-num-subpackets "EE00D40C823060")
(def input "005410C99A9802DA00B43887138F72F4F652CC0159FE05E802B3A572DBBE5AA5F56F6B6A4600FCCAACEA9CE0E1002013A55389B064C0269813952F983595234002DA394615002A47E06C0125CF7B74FE00E6FC470D4C0129260B005E73FCDFC3A5B77BF2FB4E0009C27ECEF293824CC76902B3004F8017A999EC22770412BE2A1004E3DCDFA146D00020670B9C0129A8D79BB7E88926BA401BAD004892BBDEF20D253BE70C53CA5399AB648EBBAAF0BD402B95349201938264C7699C5A0592AF8001E3C09972A949AD4AE2CB3230AC37FC919801F2A7A402978002150E60BC6700043A23C618E20008644782F10C80262F005679A679BE733C3F3005BC01496F60865B39AF8A2478A04017DCBEAB32FA0055E6286D31430300AE7C7E79AE55324CA679F9002239992BC689A8D6FE084012AE73BDFE39EBF186738B33BD9FA91B14CB7785EC01CE4DCE1AE2DCFD7D23098A98411973E30052C012978F7DD089689ACD4A7A80CCEFEB9EC56880485951DB00400010D8A30CA1500021B0D625450700227A30A774B2600ACD56F981E580272AA3319ACC04C015C00AFA4616C63D4DFF289319A9DC401008650927B2232F70784AE0124D65A25FD3A34CC61A6449246986E300425AF873A00CD4401C8A90D60E8803D08A0DC673005E692B000DA85B268E4021D4E41C6802E49AB57D1ED1166AD5F47B4433005F401496867C2B3E7112C0050C20043A17C208B240087425871180C01985D07A22980273247801988803B08A2DC191006A2141289640133E80212C3D2C3F377B09900A53E00900021109623425100723DC6884D3B7CFE1D2C6036D180D053002880BC530025C00F700308096110021C00C001E44C00F001955805A62013D0400B400ED500307400949C00F92972B6BC3F47A96D21C5730047003770004323E44F8B80008441C8F51366F38F240")

(defn int2binary [i]
  (pprint/cl-format nil "~4,'0',B" i))

(defn hex2binary [hex]
  (->> hex
       seq
       (map str)
       (map #(Integer/parseInt % 16))
       (map int2binary)
       (apply str)))

(defn binary2decimal [binary]
  (Long/parseLong binary 2))

(defn read-bits [message skip n]
  (let [message (subs message skip)]
    (subs message 0 n)))

(defn read-packet-bits [message {:keys [length] :as packet} n]
  [(read-bits message length n)
   (update packet :length + n)])

;; Parse
;; =====

(defn ->packet [message]
  {:length 6
   :type (-> message (read-bits 3 3) binary2decimal)
   :version (-> message (read-bits 0 3) binary2decimal)})

(defmulti parse-packet-type
  (fn [packet _message]
    (:type packet)))

(defn parse-packet [message]
  (-> message ->packet (parse-packet-type message)))

(defn parse-hex-packet [hex]
  (parse-packet (hex2binary hex)))

;; Literal
;; -------

(defn read-literal-value [packet message]
  (let [[five-bits] (read-packet-bits message packet 5)
        last?-bit (read-bits five-bits 0 1)]
    {:bits (read-bits five-bits 1 4)
     :last? (= last?-bit "0")}))

(defmethod parse-packet-type 4
  [packet message]
  (loop [p packet]
    (let [{:keys [bits last?]} (read-literal-value p message)
          next-packet (-> p
                          (update :length + 5)
                          (update :value str bits))]
      (if (not last?)
        (recur next-packet)
        (update next-packet :value binary2decimal)))))

;; Subpackets
;; ----------

(defn done? [bits]
  (every? #(= \0 %) bits))

(defn parse-packets [all-bits]
  (loop [bits all-bits
         packets []]
    (if-not (done? bits)
      (let [{:keys [length] :as packet} (parse-packet bits)]
        (recur (subs bits length) (conj packets packet)))
      packets)))

(defn parse-subpackets-by-num-bits [packet message]
  (let [[num-bits-str packet] (read-packet-bits message packet 15)
        num-bits (binary2decimal num-bits-str)
        [subpacket-bits packet] (read-packet-bits message packet num-bits)]
    (println (str "...by num bits (" num-bits ")."))
    (-> packet
        (assoc :length-type :num-bits)
        (assoc :packets (parse-packets subpacket-bits)))))

(defn parse-n-packets [all-bits num-packets]
  (loop [bits all-bits
         packets []
         subpackets-left num-packets]
    (if (pos? subpackets-left)
      (let [{:keys [length] :as packet} (parse-packet bits)]
        (recur (subs bits length)
               (conj packets packet)
               (dec subpackets-left)))
      packets)))

(defn parse-subpackets-by-num-subpackets [packet message]
  (let [[num-subpackets-str packet] (read-packet-bits message packet 11)
        num-subpackets (binary2decimal num-subpackets-str)
        packets (parse-n-packets (subs message (:length packet))
                                 num-subpackets)
        packets-length (->> packets (map :length) (reduce +))]
    (println (str "...by num subpackets (" num-subpackets ")."))
    (-> packet
        (update :length + packets-length)
        (assoc :length-type :num-subpackets)
        (assoc :packets packets))))

(defmethod parse-packet-type :default
  [packet message]
  (println "Parsing subpackets...")
  (let [[length-type-bit packet] (read-packet-bits message packet 1)]
    (if (= "0" length-type-bit)
      (parse-subpackets-by-num-bits packet message)
      (parse-subpackets-by-num-subpackets packet message))))

;; Run
;; ===

(defn version-sum [{:keys [packets version]
                    :or {packets []
                         version 0}}]
  (apply + version (map version-sum packets)))

(defn run []
  (->> input
       parse-hex-packet
       version-sum))
