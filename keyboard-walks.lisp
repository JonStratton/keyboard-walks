#!/usr/bin/sbcl --script

(defparameter base-count 8)

(defun get-directions()
  '(:n :ne :e :se :s :sw :w :nw))

(defun start-walk-keyboard(keyboards cnt)
  "Foreach key(letter) in keyboards."
  (maphash #'(lambda (k v) (starting-point keyboards cnt k)) keyboards))

(defun factor(n &optional (start 1) (factors (list n)))
  (if (= 0 (mod n start))
    (push start (cdr (last factors))))
  (if (< start (/ n 2))
    (setf factors (factor n (+ start 1) factors)))
  factors)

(defun get-char-by-dir(keyboards central-char dir)
  "Takes a keyboard and a direction, and returns that char or nil"
  (let ((dir-pair (get-char-line keyboards 2 central-char dir)))
    (if (>= (list-length dir-pair) 2)
      (nth 1 dir-pair))))

(defun get-chars-around(keyboards central-char)
  "Get the characters around another character on the keyboard"
  (let ((char-list (list))
	(dir-pairs))
    (dolist (dir (get-directions))
      (pushnew (get-char-by-dir keyboards central-char dir) char-list))
  char-list))

(defmacro twist-list(keyboards cnt first-char chunker new-start-end)
  "Foreach chunksize foreach direction 1 foreach direction2; get chunk of chars at direction and set the new starting point to ,whatever"
  `(dolist (chunk-size ,chunker) ; Start at 2, because we dont want chunk size 1
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
	    ;(format t "~D -> last(~D) dir(~D) -> ~D~%" temp-list (,new-start-end (nth chunks-count-num temp-list)) dir2 temp-char )
	  )
          (setf flat-list (apply #'append temp-list)) ; Flatten matrix
	  (if (>= (list-length flat-list) cnt)
             (format t "Here: ~D~%" flat-list)))))))

(defun last-item(arr)
  "Like last, but not a list. Just one item"
  (nth 0 (last arr)))

(defun starting-point(keyboards cnt first-char)
  "Foreach possile direction. Does it have base-count keys?"
  (twist-list keyboards cnt first-char (list (/ cnt 2)) last-item) ; fold list in half. TODO: This doesnt seem to be getting all of then
  (twist-list keyboards cnt first-char (factor cnt 2) first) ; pattern
  )

(defun get-char-line(keyboards cnt current dir &optional (char-list (list current) char-list-supplied-p))
  "if there is a char to the dir under cnt, get it"
  (if char-list-supplied-p
    (push current (cdr (last char-list))))
  (if (and (getf (gethash current keyboards) dir) (> cnt 1))
    (setf char-list (get-char-line keyboards (- cnt 1) (getf (gethash current keyboards) dir) dir char-list)))
  char-list)

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
        (dolist (dir '(:n :ne :e :se :sw :w :nw)) ; foreach direction
          (when (and (getf dir-delta dir) ; if we have an offset for that direction
		   key) ; sticking NIL check here for now
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
		     (NIL "A" "S" "D" "F" "G" "H" "J" "K" "L" ":")
		     (NIL "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?")))
	(micro-matrix '((1 2 3 4) ; For testing
			("q" "w" "e" "r")
			("a" "s" "d" "f")
			("z" "x" "c" "v")))
	(main-dir-delta '(:ne (-1 1) :e (0 1) :se (1 0) :sw (1 -1) :w (0 -1) :nw (-1 0)))
	(num-matrix '((NIL "/(1)" "*(1)" "-(1)")
		      ("7(1)" "8(1)" "9(1)" "+(1)" ) ; TODO: big plus key...
		      ("4(1)" "5(1)" "6(1)" "+(2)" )
		      ("1(1)" "2(1)" "3(1)" )
		      ("0(1)" "0(2)" ".(1)" ))) ; TODO: big zero key...
        (num-dir-delta '(:n (-1 0) :ne (-1 1) :e (0 1) :se (1 1) :s (1 0) :sw (1 -1) :w (0 -1) :nw (-1 -1)))
        (keyboards (make-hash-table :test 'equal)))
     (maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard lc-matrix main-dir-delta))
     (maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard uc-matrix main-dir-delta))
     ;(maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard num-matrix num-dir-delta))
     ;(maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard micro-matrix main-dir-delta))
     keyboards))

(start-walk-keyboard (make-keyboards) base-count)
