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

(defun make-keyboard-offset(keyboard offset)
  "Convert a keyboard matrix and an direction to offset plist to a hash of directions to characters."
  (let ((x 0) (y 0) (keyboard-offset (make-hash-table :test 'equal)))
    (dolist (keyrow keyboard) ; foreach row of keys
      (dolist (key keyrow) ; foreach key
        (dolist (dir '(:n :ne :e :se :sw :w :nw)) ; foreach direction
          (if (and (getf offset dir) ; if we have an offset for that direction
		   key) ; sticking NIL check here for now
            (let ((x-offset (+ x (nth 0 (getf offset dir))))
                  (y-offset (+ y (nth 1 (getf offset dir)))))
              (if (and (>= x-offset 0) ; and a key in that offset
                       (>= y-offset 0)
                       (nth y-offset keyboard)
                       (nth x-offset (nth y-offset keyboard)))
                (setf (getf (gethash key keyboard-offset) dir) (nth x-offset (nth y-offset keyboard)))))))
	 (setq x (+ x 1)))
      (setf x 0)
      (setf y (+ y 1)))
    keyboard-offset))

(defun make-chars-hash()
   "A hash of chars to keyboard directions to keys. TODO: Add caps keyboard and number pad"
   (let ((main-keyboard '(("`" 1 2 3 4 5 6 7 8 9 0 "-" "=")
			  (NIL "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\")
			  (NIL "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "\"")
			  (NIL "z" "x" "c" "v" "b" "n" "m" "," "." "/")))
	 (main-offset '(:ne (1 -1) :e (1 0) :se (0 1) :sw (-1 1) :w (-1 0) :nw (0 -1))))
     (make-keyboard-offset main-keyboard main-offset))
)

(process-char-hash (make-chars-hash) base-count)
