#!/usr/bin/sbcl --script

(defparameter base-count 4)

(defun start-walk-keyboard(keyboards cnt)
  "Foreach key(letter) in keyboards."
  (maphash #'(lambda (k v) (starting-point keyboards cnt k)) keyboards))

(defun starting-point(keyboards cnt firstchar)
  "Foreach possile direction. Does it have base-count keys?"
  (dolist (dir '(:n :ne :e :se :s :sw :w :nw))
    (let ((current-list (get-char-line keyboards cnt firstchar dir)))
       (if (>= (list-length current-list) cnt)
          (format t "~D: ~D~%" dir current-list)))))

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
    (dolist (keyrow keyboard-matrix) ; foreach row of keys
      (dolist (key keyrow) ; foreach key
        (dolist (dir '(:n :ne :e :se :sw :w :nw)) ; foreach direction
          (if (and (getf dir-delta dir) ; if we have an offset for that direction
		   key) ; sticking NIL check here for now
            (progn
              (setf next-key (get-next-key keyboard-matrix row col (getf dir-delta dir)))
              (if next-key
                (setf (getf (gethash key keyboard) dir) next-key)))))
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
	(main-dir-delta '(:ne (1 -1) :e (1 0) :se (0 1) :sw (-1 1) :w (-1 0) :nw (0 -1)))
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
     keyboards)
)

(start-walk-keyboard (make-keyboards) base-count)
