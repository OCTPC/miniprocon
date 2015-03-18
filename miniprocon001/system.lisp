(ql:quickload :hunchentoot)
(ql:quickload :hunchensocket)
(ql:quickload :cl-who)
(ql:quickload :bordeaux-threads)

;; params
(defparameter *answers* nil)
(defparameter *problem* nil)
(defparameter *starttime* nil)
(defparameter *counter* nil)
(defparameter *dispatch-table* nil)

;; 問題公開・回答提出用サーバ

(defun check-answer (answer)
  T)

(defun start-srv ()
  (setf hunchentoot:*acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start hunchentoot:*acceptor*)
  (init-srv))

(defun init-srv ()
  (reset-srv)
  (hunchentoot:define-easy-handler (post :uri "/post") (token answer)
    (setf (hunchentoot:content-type*) "text/plain")
    (let ((score (check-answer answer)))
      (if score
          (progn (push (cons token score) *answers*)
                 "回答を受け付けました")
          "回答に不備があるっぽい？"))))

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
                                                     if (<= *starttime* (get-internal-real-time))
                                                     do (start-match)
                                                       (loop-finish)
                                                     do (sleep 0.001))))))


;; UI用サーバ
