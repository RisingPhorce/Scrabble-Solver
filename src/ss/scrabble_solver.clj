(use 'clojure.contrib.combinatorics)

(def dict (set (.split (slurp "./src/ss/enable1.txt") "\r\n")))

(def letter-vals {\a 1 \b 4 \c 4 \d 2 \e 1 \f 4 \g 3 \h 3 \i 1 \j 1 \k 1 \l 1 \m 1 \n 1 \o 1 \p 1 \q 1 \r 1 \s 1 \t 1 \u 1 \v 1 \w 1 \x 1 \y 1 \z 1 nil 0})

(def played-ltr [(vec (.toCharArray "_______________"))
		 (vec (.toCharArray "_______________"))
		 (vec (.toCharArray "_______________"))
		 (vec (.toCharArray "_______________"))
		 (vec (.toCharArray "_______________"))
		 (vec (.toCharArray "_______________"))
		 (vec (.toCharArray "_______g_______"))
		 (vec (.toCharArray "_______o_______"))
		 (vec (.toCharArray "_______n_b_____"))
		 (vec (.toCharArray "_____steel_____"))
		 (vec (.toCharArray "_____e___a_____"))
		 (vec (.toCharArray "___faxed_n_____"))
		 (vec (.toCharArray "_________k_____"))
		 (vec (.toCharArray "_________s_____"))
		 (vec (.toCharArray "_______________"))])
		 

(def mult-table [[[1,1] [1,1] [1,1] [1,3] [1,1] [1,1] [3,1] [1,1] [3,1] [1,1] [1,1] [1,3] [1,1] [1,1] [1,1]]
		 [[1,1] [1,1] [2,1] [1,1] [1,1] [1,2] [1,1] [1,1] [1,1] [1,2] [1,1] [1,1] [2,1] [1,1] [1,1]]
		 [[1,1] [2,1] [1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1] [2,1] [1,1]]
		 [[1,3] [1,1] [1,1] [3,1] [1,1] [1,1] [1,1] [1,2] [1,1] [1,1] [1,1] [3,1] [1,1] [1,1] [1,3]]
		 [[1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [2,1] [1,1] [2,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1]]
		 [[1,1] [1,2] [1,1] [1,1] [1,1] [3,1] [1,1] [1,1] [1,1] [3,1] [1,1] [1,1] [1,1] [1,2] [1,1]]
		 [[3,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [3,1]]
		 [[1,1] [1,1] [1,1] [1,2] [1,1] [1,1] [1,1] [1,1] [1,1] [1,1] [1,1] [1,2] [1,1] [1,1] [1,1]]
		 [[3,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [3,1]]
		 [[1,1] [1,2] [1,1] [1,1] [1,1] [3,1] [1,1] [1,1] [1,1] [3,1] [1,1] [1,1] [1,1] [1,2] [1,1]]
		 [[1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [2,1] [1,1] [2,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1]]
		 [[1,3] [1,1] [1,1] [3,1] [1,1] [1,1] [1,1] [1,2] [1,1] [1,1] [1,1] [3,1] [1,1] [1,1] [1,3]]
		 [[1,1] [2,1] [1,1] [1,1] [2,1] [1,1] [1,1] [1,1] [1,1] [1,1] [2,1] [1,1] [1,1] [2,1] [1,1]]
		 [[1,1] [1,1] [2,1] [1,1] [1,1] [1,2] [1,1] [1,1] [1,1] [1,2] [1,1] [1,1] [2,1] [1,1] [1,1]]
		 [[1,1] [1,1] [1,1] [1,3] [1,1] [1,1] [3,1] [1,1] [3,1] [1,1] [1,1] [1,3] [1,1] [1,1] [1,1]]])

(defn make-sq-board [dim]
  (vec (for [x (range dim)]
    (vec (for [y (range dim)]
	   (let [letter ((played-ltr x) y)
		 actual-ltr (if (= letter \_) nil letter)
		 letter-mult (((mult-table x) y) 0)
		 word-mult (((mult-table x) y) 1)]
		 [actual-ltr letter-mult word-mult]))))))

(def board-dim 15)
(def board (make-sq-board board-dim))
(def my-tiles [\z \o \u \a \a \i \h])
(def all-plays (mapcat permutations (mapcat #(combinations my-tiles %1) (range 1 (inc (count my-tiles))))))

(defn valid-word [word]
  (contains? dict word))

(defn not-nil? [x] (not (nil? x)))

(defn cell-word-mult [cell] (nth cell 2))

(defn cell-score [cell] (nth cell 3))

(defn nil-coalesce [result maybe-nil replacements]
	    (if (empty? maybe-nil)
		(if (empty? replacements)
		  result
		  nil)
		(let [x (first maybe-nil) xs (rest maybe-nil)]
		(if (nil? x)
		    (recur (conj result (first replacements)) xs (rest replacements))
		    (recur (conj result x) xs replacements)))))

(defn row-coalesce [result board-row replacements]
	    (if (empty? board-row)              ; we're recursively walked the entire row
		(if (empty? replacements)       ; and we've played all the letters
		  result                        ; return the row
		  nil)                          ; otherwise we ran out of row cells before playing all tiles
		(let [x (first board-row) xs (rest board-row)]
		(if (nil? (first x))
		    (recur (conj result [(first replacements) (x 1) (x 2)]) xs (rest replacements))
		    (recur (conj result [(x 0) (x 1) (x 2)]) xs replacements)))))

(defn extract-nil-bound-str-info [vec pos]
	    (if (nil? (nth vec pos))
		[1 1 ()]
	    (let [left (reverse (take-while not-nil? (reverse (subvec vec 0 pos))))
		  right (take-while not-nil? (subvec vec pos))]
	    [(- pos (count left)) (+ pos (count right)) (concat left right)])))

(defn get-value [letter letter-mult]
    (* (get letter-vals letter) letter-mult))

(defn get-score [played-word-info]
  (let [letter-vals (map #(get-value (% 0) (% 1)) played-word-info)
	base-score (reduce + letter-vals)
        word-multiplier (reduce * (map cell-word-mult played-word-info))]
    (* base-score word-multiplier)))

(defn print-scores [tiles]
(remove nil?
  (for [y (range (count board)) x (range (count (nth board y))) :let [row (nth board y)] ]
	    (let [a (subvec row 0 x)         
	  b (subvec row x)
	  played-row (or (row-coalesce a b tiles) row)
	  horiz-word-info (extract-nil-bound-str-info (vec (map first played-row)) x)
	  played-word (String. (char-array (horiz-word-info 2)))]
;      (do (println [x y played-row]))
      (if (valid-word played-word)
	(let [score (get-score (subvec played-row (horiz-word-info 0) (horiz-word-info 1)))]
	[x y played-word score]))))))

(def all-valid-plays (mapcat #(print-scores %) all-plays))

;(mapcat #(print-scores %) all-plays))

(take 25 (sort-by #(- (cell-score %)) all-valid-plays))