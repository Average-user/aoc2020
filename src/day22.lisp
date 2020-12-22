(load "~/quicklisp/setup.lisp")
(ql:quickload :fset)

(defpackage day22
  (:use #:cl)
  (:local-nicknames (#:f #:fset)))

(in-package day22)

(defun get-input ()
  (with-open-file (file "../inputs/day22.txt")
    (loop for line = (read-line file nil nil)
          while line
          when (or (equal line "") (not (char= #\P (char line 0))))
          collect (if (equal line "") nil (read-from-string line)))))

(defun get-decks (s)
  (let ((p1 (loop for x in s while x collect x)))
    (values p1 (subseq s (+ (length p1) 1)))))

(defun game1 (p1 p2)
  (if (not (and p1 p2))
    (list p1 p2)
    (let ((a (car p1)) (b (car p2)))
      (if (> a b)
        (game1 (concatenate 'list (cdr p1) (list a b)) (cdr p2))
        (game1 (cdr p1) (concatenate 'list (cdr p2) (list b a)))))))

(defun give-result (hs a as b bs winner)
  (let ((nhs (f:with hs (concatenate 'list (cons a as) '(-1) (cons b bs)))))
    (if (= 1 winner)
        (list nhs (concatenate 'list as (list a b)) bs)
        (list nhs as (concatenate 'list bs (list b a))))))

(defun game2 (hs d1 d2)
  (if (not (and d1 d2))
    (list d1 d2)
    (let* ((a (car d1)) (b (car d2)) (as (cdr d1)) (bs (cdr d2))
           (canr (and (<= a (length as)) (<= b (length bs)))))
      (cond ((f:contains? hs (concatenate 'list d1 '(-1) d2))
             (list T nil))
            ((not canr) (apply #'game2 (give-result hs a as b bs (if (> a b) 1 2))))
            (T (let ((result (game2 (f:empty-set) (subseq as 0 a) (subseq bs 0 b))))
                 (if (not (cadr result))
                     (apply #'game2 (give-result hs a as b bs 1))
                     (apply #'game2 (give-result hs a as b bs 2)))))))))

(defun play (f)
  (multiple-value-bind (d1 d2) (get-decks (get-input))
    (let ((result (mapcan #'identity (funcall f d1 d2))))
      (reduce #'+ (mapcar #'* (reverse result)
                          (loop for x from 1 to (length result) collect x))))))

(defun main ()
  (list (play #'game1)
        (play (lambda (d1 d2) (game2 (f:empty-set) d1 d2)))))

(time (print (main)))
