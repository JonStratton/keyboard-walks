#!/usr/bin/sbcl --script

(defparameter base-count 4)

(defun process-char-hash(chars-hash cnt)
  "Foreach key(letter) in chars-hash."
  (maphash #'(lambda (k v) (starting-point chars-hash cnt k)) chars-hash))

(defun starting-point(chars-hash cnt firstchar)
  "Foreach possile direction. Does it have base-count keys?"
  (dolist (dir '(:n :ne :e :se :s :sw :w :nw))
    (let ((current-list (list)))
       (setf current-list (get-char-line chars-hash cnt firstchar dir))
       (if (>= (list-length current-list) cnt)
          (format t "~D~%" current-list)))))

(defun get-char-line(chars-hash cnt current dir &optional (char-list (list current) char-list-supplied-p))
  "if there is a char to the dir under cnt, get it"
  (if char-list-supplied-p
    (push current (cdr (last char-list))))
  (if (and (getf (gethash current chars-hash) dir) (> cnt 1))
    (setf char-list (get-char-line chars-hash (- cnt 1) (getf (gethash current chars-hash) dir) dir char-list)))
  char-list)

(defun make-keyboard(keyboard-matrix dir-cart-delta)
  "Convert a keyboard matrix and an direction to offset plist to a hash of directions to characters."
  (let ((x 0) (y 0) (keyboard (make-hash-table :test 'equal)))
    (dolist (keyrow keyboard-matrix) ; foreach row of keys
      (dolist (key keyrow) ; foreach key
        (dolist (dir '(:n :ne :e :se :sw :w :nw)) ; foreach direction
          (if (and (getf dir-cart-delta dir) ; if we have an offset for that direction
		   key) ; sticking NIL check here for now
            (let ((x-next-key (+ x (nth 0 (getf dir-cart-delta dir))))
                  (y-next-key (+ y (nth 1 (getf dir-cart-delta dir)))))
              (if (and (>= x-next-key 0) ; and a key in that offset
                       (>= y-next-key 0)
                       (nth y-next-key keyboard-matrix)
                       (nth x-next-key (nth y-next-key keyboard-matrix)))
                (setf (getf (gethash key keyboard) dir) (nth x-next-key (nth y-next-key keyboard-matrix)))))))
	 (setq x (+ x 1)))
      (setf x 0)
      (setf y (+ y 1)))
    keyboard))

(defun make-keyboards()
  "A hash of chars to keyboard directions to keys. TODO: Add caps keyboard and number pad"
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
		      ("0(1)" "0(2)" ".(1)" ))) ; TODO: bug zero key...
        (num-dir-delta '(:n (-1 0) :ne (-1 1) :e (0 1) :se (1 1) :s (1 0) :sw (1 -1) :w (0 -1) :nw (-1 -1)))
        (keyboards (make-hash-table :test 'equal)))
     (maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard lc-matrix main-dir-delta))
     (maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard uc-matrix main-dir-delta))
     ;(maphash #'(lambda (k v) (setf (gethash k keyboards) v)) (make-keyboard num-matrix num-dir-delta))
     keyboards)
)

(process-char-hash (make-keyboards) base-count)
