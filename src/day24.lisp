(ql:quickload :arrow-macros)
(ql:quickload :alexandria)
(ql:quickload :fset)

(defpackage :day24
  (:use :cl :arrow-macros)
  (:local-nicknames (:fs :fset) (:ax :alexandria)))

(in-package :day24)

(defun parse-instructions (s)
  (when s
    (if (or (char= (car s) #\e) (char= (car s) #\w))
        (cons (string (car s)) (parse-instructions (cdr s)))
        (cons (concatenate 'string (list (car s) (cadr s)))
              (parse-instructions (cddr s))))))

(defun get-input ()
  (with-open-file (file "../inputs/day24.txt")
    (loop for line = (read-line file nil nil)
          while line
          collect (parse-instructions (map 'list #'identity line)))))

(defun move (dir coord)
  (cond
    ((equal dir "e")  (mapcar #'+ coord '(1 0 -1)))
    ((equal dir "se") (mapcar #'+ coord '(0 1 -1)))
    ((equal dir "sw") (mapcar #'+ coord '(-1 1 0)))
    ((equal dir "w" ) (mapcar #'+ coord '(-1 0 1)))
    ((equal dir "nw") (mapcar #'+ coord '(0 -1 1)))
    ((equal dir "ne") (mapcar #'+ coord '(1 -1 0)))))

(defun find-tile (moves)
  (reduce (lambda (x y) (move y x)) moves :initial-value '(0 0 0)))

(defun initial-tiles ()
  (-<>> (mapcar #'find-tile (get-input))
    (remove-if (lambda (tile) (=  0 (mod (count tile <> :test #'equal) 2))) <>)))

(defun neighbors (coord)
  (mapcar (lambda (x) (move x coord)) '("e" "se" "sw" "w" "nw" "ne")))

(defun update-tiles (black)
  (let* ((blackl (fs:convert 'list black))
         (white (remove-if (ax:curry #'fs:contains? black)
                           (ax:mappend #'neighbors blackl))))
    (concatenate 'list
      (remove-if-not (lambda (c) (-<>> (neighbors c)
                                   (count-if (ax:curry #'fs:contains? black))
                                   (= 2)))
                     (remove-duplicates white :test #'equal))
      (remove-if (lambda (c) (-<>> (neighbors c)
                               (count-if (ax:curry #'fs:contains? black))
                               (or (= 0 <>) (< 2 <>))))
                 blackl))))

(defun main ()
  (let* ((tiles (fs:convert 'fs:set (initial-tiles)))
         (icount (fs:size tiles)))
    (dotimes (i 100)
      (setf tiles (fs:convert 'fs:set (update-tiles tiles))))
    (list icount (fs:size tiles))))

(time (print (main)))
