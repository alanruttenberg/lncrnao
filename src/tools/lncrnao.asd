(require :asdf)
(in-package :cl-user)

(asdf:defsystem lncrnao
  :author "Alan Ruttenberg"
  :components
  ((:file "common")
   (:file "ingest-lncrnadb"))
  :serial t
  :depends-on (owl2 cl-json))

;; Until prove-asdf works
(let ((where (merge-pathnames "lncrnao-tests.lisp" (load-time-value *load-pathname*))))
  (defun cl-user::test-lncrnao()
  (let ((*load-verbose* nil))
    (uiop/utility:with-muffled-conditions ((list 'style-warning 'asdf::bad-system-name))
      (require :testing)))
  (let ((cl-user::*read-time-uri* t))
    (funcall (intern "RUN" 'prove) where))))

