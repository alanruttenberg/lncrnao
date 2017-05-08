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

(defun fresh-lncrna-uri ()
  (make-uri nil (format nil "obo:NCRO_~7,'0d" (incf *lncrna-id-counter*))))

(defun register-new-lncrna (label synonyms )
  (let ((uri (make-uri nil (format nil "obo:NCRO_~7,'0d" (incf *lncrna-id-counter*)))))
    (setf (gethash uri *lncrna-uri2name*) (cons label synonyms))
    uri))

(defparameter *species-name-fixes* '(("West Nile Virus" "West Nile virus") ("Human herpesvirus" "Human alphaherpesvirus 1")))
  
(defun taxon-id-from-label (label)
  (setq label (#"replaceAll" label " \\(.*" ""))
  (setq label (or (second (assoc label *species-name-fixes* :test 'equalp)) label))
  (let ((call (get-url (clean-uri "rest.ensembl.org"
				  (format nil "/taxonomy/id/~a" label)
				  "http" "" "content-type=application/json")
		       :accept "application/json"
		       :ignore-errors t)))
    (values (cdr (assoc :id (with-input-from-string (s call) (json:decode-json s))))
	    label)))

(defun taxon-urls-for (names)
  (mapcar (lambda(label) (format nil "http://purl.obolibrary.org/obo/NCBITaxon_~a" (taxon-id-from-label label))) 
	  names))

;(taxon-urls-for	  *lncrnadb-species*)
