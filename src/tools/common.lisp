(defparameter *lncrna-name2uri* (make-hash-table :test 'equalp))
(defparameter *lncrna-uri2name* (make-hash-table :test 'equalp))
(defparameter *lncrna-id-counter* 100000)

(defun register-or-find-uri (label synonyms)
  (let ((candidates (append (gethash label *lncrna-name2uri*) (apply 'append (mapcar (lambda(s) (gethash s *lncrna-name2uri*)) synonyms)))))
    (setq candidates (remove-duplicates candidates))
    (cond ((= (length candidates) 0)
	   (let ((uri (register-new-lncrna label synonyms)))
	     (dolist (name (cons label synonyms)) (setf (gethash name *lncrna-name2uri*) (list uri)))
	     uri))
	  ((= (length candidates) 1)
	   (car candidates))
	  ((> (length candidates) 1)
	   (format nil "skipping ~a(~{~a~^, ~}) because ambiguous: ~{~a - ~s}~^, ~}" label synonyms candidates 
		   (mapcar (lambda(c) (gethash *lncrna-uri2name* c)) candidates))))))

(defun register-new-lncrna (label synonyms )
  (let ((uri (make-uri nil (format nil "obo:NCRO_~7,'0d" (incf *lncrna-id-counter*)))))
    (setf (gethash uri *lncrna-uri2name*) (cons label synonyms))
    uri))

