(ql:quickload :hunchentoot)
(ql:quickload :hunchensocket)
(ql:quickload :cl-markup)
(ql:quickload :bordeaux-threads)

;; params
(defparameter *answers* nil)
(defparameter *problem* nil)
(defparameter *starttime* nil)
(defparameter *counter* nil)
(defparameter *dispatch-table* nil)

(defparameter *posturl* "http://localhost:4242/post")

;; Server

(defun check-prime (n)
  (cond ((< n 2) nil)
        ((= n 2) T)
        ((evenp n) nil)
        ((loop :for i :from 3 :until (>= i (/ n i))
            :when (= (mod n i) 0)
            return T) nil)
        (T T)))

(defun check-answer (answer)
  (let ((x 0)
        (y 0)
        (steps 0)
        (num-of-move 0))
    (with-input-from-string (in answer)
      (loop :named l :for line := (read-line in nil nil) :while line
         :do (let ((dir (subseq line 0 1))
                   (step (or (parse-integer (subseq line 1) :junk-allowed t) 0)))
               ;; (format t "[~A ~A]" dir steps)
               (if (and (find dir '("R" "L" "U" "D") :test #'string=) (check-prime step))
                   (progn 
                     (cond ((string= dir "R") (setf x (+ x step)))
                           ((string= dir "L") (setf x (- x step)))
                           ((string= dir "U") (setf y (+ y step)))
                           ((string= dir "D") (setf y (- y step)))
                           (T (return-from l nil)))
                     (setf steps (+ steps step))
                     (setf num-of-move (1+ num-of-move)))
                   (return-from l nil)))))
    ;; (print (cons x y))
    (if (equal *problem* (cons x y))
        (list num-of-move steps)
        nil)))

(defun make-ranking (answers)
  (delete-duplicates (sort (copy-list answers)
                           #'(lambda (a b)
                               (cond ((< (caadr a) (caadr b)) T)
                                     ((> (caadr a) (caadr b)) nil)
                                     ((< (cadadr a) (cadadr b)) T)
                                     ((> (cadadr a) (cadadr b)) nil)
                                     ((< (caddr a) (caddr b)) T)
                                     ((> (caddr a) (caddr b)) nil))))
                     :test (lambda (a b)
                             (string= (car a) (car b)))
                     :from-end T))

(defun start-srv ()
  (setf hunchentoot:*acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start hunchentoot:*acceptor*)
  (init-srv))

(defun init-srv ()
  (reset-srv)
  
  (hunchentoot:define-easy-handler (form :uri "/form") ()
    (setf (hunchentoot:content-type*) "text/html")
    (cl-markup:markup
     (html (:form :action *posturl* :method "post"
                  (:p "token: " (:br)
                      (:input :type "text" :name "token" :size "20"))
                  (:p "answer: " (:br)
                      (:textarea :name "answer" :cols "40" :rows "10" ""))
                  (:input :type "submit" :value "submit")))))
  
  (hunchentoot:define-easy-handler (ranking :uri "/ranking") ()
    (setf (hunchentoot:content-type*) "text/html")
    (cl-markup:markup
     (html (:table
            (:thread
             (:tr
              (:th "#")
              (:th "token")
              (:th "exchange")
              (:th "steps")
              (:th "milliseconds")))
            (:tbody
             (loop :for data :in (make-ranking *answers*)
                :for n :from 1
                :collect (let ((token (car data))
                               (exchange (caadr data))
                               (steps (cadadr data))
                               (milliseconds (caddr data)))
                           (cl-markup:markup
                            (:td n)
                            (:td token)
                            (:td exchange)
                            (:td steps)
                            (:td milliseconds)))))))))

  (hunchentoot:define-easy-handler (post :uri "/post") (token answer)
    (setf (hunchentoot:content-type*) "text/plain")
    (let ((score (check-answer answer))
          (time (- (get-internal-real-time) *starttime*)))
      (if score
          (progn (push (list token score time) *answers*)
                 "OK")
          "FAILED"))))

(defun stop-srv ()
  (hunchentoot:stop hunchentoot:*acceptor*))

(defun reset-srv ()
  (hunchentoot:define-easy-handler (problem :uri "/problem") ()
    (setf (hunchentoot:content-type*) "text/plain")
    (format nil "")))

(defun start-match ()
  (setf *answers* nil)
  (hunchentoot:define-easy-handler (problem :uri "/problem") ()
    (setf (hunchentoot:content-type*) "text/plain")
    (format nil "~A ~A" (car *problem*) (cdr *problem*))))

(defun set-problem ()
  (format t "set goal-point~%")
  (let ((x nil)
        (y nil))
    (format t "x: ")
    (setf x (read))
    (format t "y: ")
    (setf y (read))
    (setf *problem* (cons x y))))

(defun start-timer ()
  (set-problem)
  (format t "(min): ")
  (setf *starttime* (+ (* (read) 60 1000) (get-internal-real-time)))
  (setf *counter* (bordeaux-threads:make-thread (lambda ()
                                                  (loop
                                                     :if (<= *starttime* (get-internal-real-time))
                                                     :do (start-match)
                                                     (loop-finish)
                                                     :do (sleep 0.001))))))

;; UI Server

