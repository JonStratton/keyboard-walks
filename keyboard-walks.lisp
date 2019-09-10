#!/usr/bin/sbcl --script

(defparameter base-count 8)

(defun get-directions()
  '(:n :ne :e :se :s :sw :w :nw))

(defun factor(n &optional (start 1) (factors (list n)))
  (if (= 0 (mod n start))
    (push start (cdr (last factors))))
  (if (< start (/ n 2))
    (setf factors (factor n (+ start 1) factors)))
  factors)

(defun get-combos(cnt &optional (options 2))
  "return a list of lists of all possible options"
  (let ((combos-returned))
    (dotimes (flip options)
      (if (<= cnt 1)
        (setq combos-returned (append combos-returned (list (list flip))))
        (dolist (combos-recurned (get-combos (- cnt 1) options))
          (setq combos-returned (append combos-returned (list (append (list flip) combos-recurned)))))))
    combos-returned))

(defun shift-list(in-list shift-hash)
  "Take a list of chars and apply the shift-hash to them"
  (let ((return-list))
    (dolist (in-char in-list)
      (if (gethash in-char shift-hash)
        (setq return-list (append return-list (list (gethash in-char shift-hash))))))
    return-list))

(defun shift-list-mask(in-list mask shift-hash)
  "Take a list, a mask lining up with the list containing 1 and 0, and a function. Do the function on the things with a 1."
  (let ((in-list-copy (copy-list in-list)))
    (dotimes (mask-index (list-length mask))
      (if (= (nth mask-index mask) 1)
        (setf (nth mask-index in-list-copy) (shift-list (nth mask-index in-list-copy) shift-hash))))
    in-list-copy))

(defun shift-combo(matrix shift-hash)
  "returns a matrix where every combo of the second level has been run against the input function"
  (let ((return-list))
    (dolist (mask (get-combos (list-length matrix)))
      (let ((new-matrix (shift-list-mask (copy-list matrix) mask shift-hash)))
	(setq return-list (append return-list (list new-matrix)))))
    return-list))

(defun get-char-by-dir(keyboards central-char dir)
  "Takes a keyboard and a direction, and returns that char or nil"
  (let ((dir-pair (get-char-line keyboards 2 central-char dir)))
    (if (>= (list-length dir-pair) 2)
      (nth 1 dir-pair))))

(defun get-chars-around(keyboards central-char)
  "Get the characters around another character on the keyboard"
  (let ((char-list)
	(dir-pairs))
    (dolist (dir (get-directions))
      (push (get-char-by-dir keyboards central-char dir) char-list))
  char-list))

(defun fold-list(keyboards shift-hash cnt first-char)
  "Half the chars to a point, then another half going possibly another direction"
  (let ((half-cnt (/ cnt 2))
	(first-list (list))
	(second-list (list))
	(flat-list (list))
	(last-char))
    (dolist (dir1 (get-directions))
      (setf first-list (get-char-line keyboards half-cnt first-char dir1))
      (when (>= (list-length first-list) half-cnt)
	(setf last-char (nth 0 (last first-list)))
	(dolist (second-first-char (get-chars-around keyboards last-char))
          (dolist (dir2 (get-directions))
	    (setf second-list (get-char-line keyboards half-cnt second-first-char dir2))
            (dolist (temp-shift-list (shift-combo (list first-list second-list) shift-hash))
	      (let ((flat-list (apply #'append temp-shift-list))) ; Flatten matrix to list
	      (if (>= (list-length flat-list) cnt)
	        (format t "Got Fold: ~D~%" flat-list))))))))))

(defmacro twist-list(keyboards shift-hash cnt first-char chunker new-start-end)
  "Foreach chunksize foreach direction 1 foreach direction2; get chunk of chars at direction and set the new starting point to ,whatever"
  `(dolist (chunk-size ,chunker)
    (dolist (dir1 (get-directions))
      (dolist (dir2 (get-directions))
        (let ((temp-list (list))
	      (flat-list (list))
	      (temp-char first-char))
	  (dotimes (chunks-count-num (/ cnt chunk-size))
	    (push nil temp-list)) ; I have no idea why I cannot do this in the below dotimes...
          (dotimes (chunks-count-num (/ cnt chunk-size))
	    (setf (nth chunks-count-num temp-list) (get-char-line keyboards chunk-size temp-char dir1))
	    (setf temp-char (get-char-by-dir keyboards (,new-start-end (nth chunks-count-num temp-list)) dir2))
	  )
          (dolist (temp-shift-list (shift-combo temp-list shift-hash))
	    (let ((flat-list (apply #'append temp-shift-list))) ; Flatten matrix to list
	    (if (>= (list-length flat-list) cnt)
              (format t "Got Pattern: ~D~%" flat-list)))))))))

(defun last-item(arr)
  "Like last, but not a list. Just one item"
  (nth 0 (last arr)))

(defun start-walk-keyboard(keyboards shift-hash cnt)
    "Foreach key(letter) in keyboards."
      (maphash #'(lambda (k v) (starting-point keyboards shift-hash cnt k)) keyboards))

(defun starting-point(keyboards shift-hash cnt first-char)
  "Foreach possile direction. Does it have base-count keys?"
  ;(twist-list keyboards cnt first-char (list (/ cnt 2)) last-item) ; fold list in half. TODO: This doesnt seem to be getting all of then
  (fold-list keyboards shift-hash cnt first-char)
  (twist-list keyboards shift-hash cnt first-char (rest (factor cnt 2)) first) ; pattern
  )

(defun get-char-line(keyboards cnt current dir &optional (char-list (list current) char-list-supplied-p))
  "if there is a char to the dir under cnt, get it"
  (if char-list-supplied-p
    (push current (cdr (last char-list))))
  (if (and (getf (gethash current keyboards) dir) (> cnt 1))
    (setf char-list (get-char-line keyboards (- cnt 1) (getf (gethash current keyboards) dir) dir char-list)))
  char-list)

; Logic for building keyboards and shift hash
(defun get-next-key(keyboard-matrix row col delta)
  "Get the next key in the matrix with the current row and col and the delta list(row col)"
  (let ((row-next-key (+ row (nth 0 delta)))
        (col-next-key (+ col (nth 1 delta))))
    (if (and (>= row-next-key 0)
             (>= col-next-key 0)
             (nth row-next-key keyboard-matrix)
             (nth col-next-key (nth row-next-key keyboard-matrix)))
      (nth col-next-key (nth row-next-key keyboard-matrix)))))

(defun make-keyboard(keyboard-matrix dir-delta)
  "Convert a keyboard matrix and an direction to offset plist to a hash of directions to characters."
  (let ((row 0) (col 0) (next-key NIL) (keyboard (make-hash-table :test 'equal)))
    (dolist (keyrow keyboard-matrix)
      (dolist (key keyrow)
        (dolist (dir (get-directions)) ; foreach direction
          (when (and (getf dir-delta dir) ; if we have an offset for that direction
		   key) ; and we are not on a NIL key
            (setf next-key (get-next-key keyboard-matrix row col (getf dir-delta dir)))
            (if next-key
              (setf (getf (gethash key keyboard) dir) next-key))))
	 (setq col (+ col 1)))
      (setf col 0)
      (setf row (+ row 1)))
    keyboard))

(defun make-keyboards()
  "A hash of chars to keyboard directions to keys. TODO: figure out number pad and its double keys."
  (let ((lc-matrix '(("`" 1 2 3 4 5 6 7 8 9 0 "-" "=")
                     (NIL "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\")
                     (NIL "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'")
                     (NIL "z" "x" "c" "v" "b" "n" "m" "," "." "/")))
	(uc-matrix '(("~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+")
		     (NIL "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|")
		     (NIL "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"")
		     (NIL "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?")))
	(main-dir-delta '(:ne (-1 1) :e (0 1) :se (1 0) :sw (1 -1) :w (0 -1) :nw (-1 0)))
	(num-matrix '((NIL "/(1)" "*(1)" "-(1)")
		      ("7(1)" "8(1)" "9(1)" "+(1)" ) ; TODO: big plus key...
		      ("4(1)" "5(1)" "6(1)" "+(2)" )
		      ("1(1)" "2(1)" "3(1)" )
		      ("0(1)" "0(2)" ".(1)" ))) ; TODO: big zero key...
        (num-dir-delta '(:n (-1 0) :ne (-1 1) :e (0 1) :se (1 1) :s (1 0) :sw (1 -1) :w (0 -1) :nw (-1 -1)))
        (keyboards (make-hash-table :test 'equal)))
     (maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard lc-matrix main-dir-delta))
     ;(maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard uc-matrix main-dir-delta)) ; Shifting now does this
     ;(maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard num-matrix num-dir-delta))
     keyboards))

(defun make-shift()
  (let ((lc '("`" 1 2 3 4 5 6 7 8 9 0 "-" "="
              "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
              "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'"
              "z" "x" "c" "v" "b" "n" "m" "," "." "/"))
	(uc '("~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"
              "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
              "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\""
              "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?"))
	(shift (make-hash-table :test 'equal)))
    (dotimes (cnt (list-length lc))
      (setf (gethash (nth cnt lc) shift) (nth cnt uc))
      (setf (gethash (nth cnt uc) shift) (nth cnt lc)))
    shift))

; http://cl-cookbook.sourceforge.net/os.html
(defun my-command-line()
  (or
    #+SBCL *posix-argv*
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    nil))

(let ((argv (my-command-line)))
  (if (>= (list-length argv) 2)
    (defparameter base-count (parse-integer (last-item (my-command-line))))))

(start-walk-keyboard (make-keyboards) (make-shift) base-count)
