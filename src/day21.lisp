(load "~/quicklisp/setup.lisp")
(ql:quickload :asdf)
(asdf:load-system :cl-ppcre)

(defun parse-line (l)
  (let ((ws (cl-ppcre:split " \\(" l)))
    (list (cl-ppcre:split "\ " (car ws))
          (mapcar (lambda (x) (remove #\, (remove #\) x)))
                  (cdr (cl-ppcre:split "\ " (cadr ws)))))))

(defun get-input ()
  (with-open-file (file "../inputs/day21.txt")
    (loop for line = (read-line file nil nil)
          while line
          collect (parse-line line))))

(defun intersections (ls)
  (reduce (lambda (x y) (intersection x y :test #'equal))  ls))

(defun potential-id (items)
  (mapcar (lambda (a)
            (list a (intersections (loop for i in items
                                         when (member a (cadr i) :test #'equal)
                                           collect (car i)))))
          (remove-duplicates (apply #'concatenate 'list (mapcar #'cadr items))
                             :test #'equal)))

(defun safe-food (items pids)
  (let* ((ps   (apply #'concatenate 'list (mapcar #'cadr pids))))
    (mapcan (lambda (f) (and (not (member f ps :test #'equal)) (list f)))
            (remove-duplicates (apply #'concatenate 'list (mapcar #'car items))
                               :test #'equal))))

(defun find-names (pids)
  (let ((singleton (car (loop for e in pids
                              when (= 1 (length (cadr e)))
                                collect (cons (car e) (caadr e))))))
    (when singleton
      (let ((subsol (loop for e in pids
                          when (not (equal (car singleton) (car e)))
                            collect (list (car e) (remove (cdr singleton) (cadr e)
                                                          :test #'equal)))))
        (cons singleton (find-names subsol))))))

(defun main ()
  (let* ((items (get-input))
         (pids  (potential-id items)))
    (list
      (reduce #'+ (mapcar (lambda (f) (loop for i in (mapcar #'car items)
                                            when (member f i :test #'equal)
                                              sum 1 into total
                                            finally (return total)))
                          (safe-food items pids)))
      (format nil "~{~a~^,~}"
        (mapcar #'cdr (sort (find-names pids) #'string-lessp :key #'car))))))

(print (main))
