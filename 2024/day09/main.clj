(ns main
  (:require
   [clojure.pprint :as pp]))

(defn todo [] (throw (RuntimeException. "still work in progress")))

;; tag either :file :empty
(defrecord Char [tag ^Integer num])

(defn get-tag [i] (if (= 0 (mod i 2)) :empty :file))

(def example "2333133121414131402")

(def input (slurp "./../input/day09.txt"))

(defn to-int [x] (Character/getNumericValue x))

(defn insert [v val at]
  (into []
        (concat
         (subvec v 0 at)
         [val]
         (subvec v at))))

(def zipped-alt
  (let [x (atom 0)]
    (to-array (map #(->Char (get-tag (swap! x inc)) (to-int %))
                   example))))

;; length      -- the number in the string
;; id          -- the idx in the string
;; cum-idx     -- the cummulative index
;; cum-idx-end -- the *exclusive* end of the range
(defrecord Entry [^Integer length
                  ^Integer id
                  ^Integer cum-idx
                  ^Integer cum-idx-end
                  ^Integer orig-id])

(defn parse [[files empties] ^Entry entry]
  (if (= (mod (:id entry) 2) 0)
    (list (conj files (assoc entry :id (/ (:id entry) 2)))
          empties)
    (list files
          (conj empties entry))))

(def zipped-alt-2
  ;; reduce f val col
  ;; reduce f col
  (let [files nil,
        empties [],
        [_, ret] (reduce (fn [[cumsum entries] [c idx]]
                           (let [i (to-int c),
                                 newsum (+ cumsum i)]
                             [newsum (conj entries (->Entry i idx cumsum newsum idx))]))
                         [0, []]
                         (map vector example (range)))
        [files empties] (reduce parse
                                [files empties]
                                ret)]
    ; [files (into '() empties)]))
    [files (filter #(not= 0 (:length %)) empties)])) ;; we'll see

;; n       -- Integer
;; emtpies -- Array
;; returns [(list empties), (list (remaining empties))]
(defn take-n-empties [n [{len :length cum-idx :cum-idx cum-idx-end :cum-idx-end :as head} & rest]]
  (if (empty? rest)
    [[head] nil]
    (case (compare n len)
    ;; less
      -1 [(list (assoc head
                       :length n
                       :cmp-res -1
                       :cum-idx-end (inc (- cum-idx-end n))))
          (cons (assoc head
                       :length (- len n)
                       :cum-idx (+ cum-idx n))
                rest)]
    ;; equal
      0 [(list (assoc head :cmp-res 0)) rest]
    ;; greater
      1 (let [[es new-rests] (take-n-empties (- n len) rest)]
        ;; first the head, because it has to be filled first, then the rest
          [(concat [(assoc head :cmp-res 1)] es), new-rests])
    ;; default
      (throw (IllegalStateException. "this should never happen"))
    ;;
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1. Detect that you're done (one block of continious memory)
;; 2. Make sure you free (by appending) the moved files 
;;    - appending is slow again, I dont like this
;;    - We can probably use a second list to keep track of the new freed ones
;;      and then check which list has the lower free index
;;
;; Wait can't we just use a map/hash of pos -> status (no thats to slow ?)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; files   -- List  -- list of the reversed files
;; emtpies -- Array -- the empty slots in correct slots in correct order
(defn solve-a [[file-hd & files-tl] empties empty-len]
  (if (empty? empties)
    nil
    (let [[es rest-empties] (take-n-empties (:length file-hd)
                                            empties)]
      (concat (map #(assoc % :file-ref file-hd) es)
              (if (empty? files-tl)
                nil
                (solve-a files-tl rest-empties))))))

(defn- main []
  ; (pp/pprint (map #(->Entry (to-int %1) %2 nil nil) example (range)))
  ; (println)
  (pp/pprint (let [[files empties] zipped-alt-2]
               {:files files,
                :empties empties}))

  (println)

  (pp/pprint (let [[files empties] zipped-alt-2]
               (solve-a files empties (reduce #(+ %1 (:length %2)) empties))))

  ;;
  )
;;
(main)



