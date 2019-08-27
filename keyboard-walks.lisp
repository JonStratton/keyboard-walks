#!/usr/bin/sbcl --script

(defparameter base-count 3)

(defun process-char-hash(chars-hash cnt)
  "Foreach key(letter) in chars-hash."
  (maphash #'(lambda (k v) (starting-point chars-hash cnt k)) chars-hash))

(defun starting-point(chars-hash cnt firstchar)
  "Foreach possile direction. Does it have base-count keys?"
  (dolist (dir '(:n :ne :e :se :s :sw :w :nw))
    (let ((current-list (list)))
       (setf current-list (get-next-char chars-hash cnt firstchar dir))
       (if (>= (list-length current-list) cnt)
          (format t "Got: ~D~%" current-list)))))

(defun get-next-char(chars-hash cnt current dir &optional (char-list (list current) char-list-supplied-p ))
  "if there is a char to the dir under cnt, get it"
  (if char-list-supplied-p
    (push current (cdr (last char-list))))
  (if (and (getf (gethash current chars-hash) dir) (> cnt 1))
    (setf char-list (get-next-char chars-hash (- cnt 1) (getf (gethash current chars-hash) dir) dir char-list)))
  char-list)

(defun make-chars-hash()
   "A hash of chars to keyboard directions to keys"
   ; Hard coded fake micro keyboard to test with. TODO: maybe do this with an algo shifting each row to the right by .5.
   ( let ((x (make-hash-table :test 'equal)))
      (setf (gethash '1 x) '(:e 2 :se "q"))
      (setf (gethash '2 x) '(:e 3 :se "w" :sw "q" :w 1))
      (setf (gethash '3 x) '(:se "e" :sw "w" :w 2))
      (setf (gethash "q" x) '(:ne 2 :e "w" :se "a" :nw 1))
      (setf (gethash "w" x) '(:ne 3 :e "e" :se "s" :sw "a" :w "q" :nw 2))
      (setf (gethash "e" x) '(:se "d" :sw "s" :w "w" :nw 3))
      (setf (gethash "a" x) '(:ne "w" :e "s" :nw "q"))
      (setf (gethash "s" x) '(:ne "e" :e "d" :w "a" :nw "w"))
      (setf (gethash "d" x) '(:w "s" :nw "e"))
      x))

(process-char-hash (make-chars-hash) base-count)
