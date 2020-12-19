(load "~/quicklisp/setup.lisp")
(ql:quickload "asdf")
(asdf:load-system :cl-ppcre)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-r (rule)
  (let* ((x (cl-ppcre:split "\: " rule))
         (i (car x)) (r (cadr x)))
    (if (or (equal r "\"a\"") (equal r "\"b\""))
        (list (parse-integer i) (string (char r 1)))
        (list (parse-integer i)
              (mapcar (lambda (xs) (mapcar #'parse-integer
                                           (cl-ppcre:split "\\s" xs)))
                      (cl-ppcre:split " \\| " r))))))

(defun concatmap (f xs)
  (apply #'concatenate 'list (mapcar f xs)))

(defun matches (rules i)
  (let ((rule (aref rules i)))
    (if (member rule '("a" "b") :test #'equal)
        (list rule)
        (concatmap (lambda (xs)
                     (reduce (lambda (result i)
                               (concatmap (lambda (e)
                                            (loop for r in result
                                                  collect (concatenate 'string r e)))
                                          (matches rules i)))
                             xs :initial-value (list "")))
                   rule))))

(defun all (f xs) (equal '() (remove-if f xs)))

(defun partition (list n)
  (loop for i from 0 to (floor (- (/ (length list) n) 1))
        collect (subseq list (* n i) (* n (+ 1 i)))))


(defun main ()
  (let* ((lines (get-file "../inputs/day19.txt"))
         (rules (make-array (length lines) :initial-element nil))
         (messages (loop for l in (reverse lines)
                         while (not (equal l ""))
                         collect l)))
    (loop for l in lines
          while (find #\: l)
          do (let ((r (parse-r l)))
               (setf (aref rules (car r)) (cadr r))))
    (let ((l0 (matches rules 0)))
      (print (length (loop for m in messages
                           if (member m l0 :test #'equal)
                           collect m))))
    (let* ((l31 (matches rules 31))
           (l42 (matches rules 42))
           (k    (length (first l42))))
      (print
       (length
        (remove-if
         (lambda (m)
           (let* ((ps (mapcar (lambda (xs) (apply #'concatenate 'string xs))
                              (partition (cl-ppcre:split "" m) k)))
                  (m42 (loop for p in ps
                             while (member p l42 :test #'equal)
                             collect p))
                  (m31 (subseq ps (length m42) (length ps))))
             (not (and (all (lambda (x) (member x l31 :test #'equal)) m31)
                       (< 0 (length m31) (length m42))))))
         messages))))))

(main)
