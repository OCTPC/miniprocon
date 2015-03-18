(ql:quickload :hunchentoot)
(ql:quickload :hunchensocket)
(ql:quickload :cl-who)


;; 問題公開・回答提出用サーバ

(defun srv-start ()
  (setf hunchentoot:*acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start hunchentoot:*acceptor*)
  (srv-init))

(defun srv-init ()
  )

(defun srv-stop ()
  (hunchentoot:stop hunchentoot:*acceptor*))


;; UI用サーバ
