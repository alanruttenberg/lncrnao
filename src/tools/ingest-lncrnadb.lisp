(defvar *lncrnadb-raw* (make-hash-table :test 'equalp))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun lncrnao-setup ()
    (let ((already (all-label-sources)))
      (loop for source in '(:iao :bfo :obi :ro)
	    unless (assoc source already)
	      do (new-label-source (load-ontology (make-uri nil (format nil "obo:~a.owl" source))) :key source))))
  (lncrnao-setup))
    
  
(defun load-lncrnadb ()
  (lncrnao-setup)
  (setq *lncrnadb-raw* (make-hash-table :test 'equalp))
  (setq *lncrna-name2uri* (make-hash-table :test 'equalp))
  (setq *lncrna-uri2name* (make-hash-table :test 'equalp))
  (setq *lncrna-id-counter* 100000)
  (loop for source-file in '("annotation.xml" "literature.xml" "nomenclature.xml" "sequence.xml" "species.xml")
	for key in '(:annotation :literature :nomenclature :sequence :species)
	for path = (asdf/system:system-relative-pathname "lncrnao" (format nil "../database/lncrnadb/~a" source-file))
	for xml = (with-open-file (f path) (xmls:parse  f))
	do
	   (dolist (entry (find-elements-with-tag xml "Results"))
	     (let ((id (parse-integer (attribute-named entry "id"))))
	       (push (list key entry)  (gethash id *lncrnadb-raw*)))))
  (maphash (lambda(id entries)
	     (setf (gethash (third (find-element-with-tag (second (assoc :nomenclature entries))  "nomenclature" "name"))
			    *lncrnadb-raw*)
		   entries))
	   *lncrnadb-raw*)
  (register-lncrnadb-names)
  (with-ontology foo (:ontology-iri !obo:ncro/lncrnao/lncrnao-generated.owl)
    ((asq (declaration (object-property !obo:RO_0002160))
	  (declaration (annotation-property !obo:IAO_0000111)))
     (as (lncrnadb-declaration-and-label-axioms))
     (as (lncrnadb-comment-axioms))
     (maphash (lambda(uri v)
		(as (lncrnadb-species-axioms uri)))
	      *lncrna-uri2name*))
    (write-rdfxml foo "~/repos/lncrnao/src/ontology/lncrnao-generated.owl")))

(defun register-lncrnadb-names (&aux uris)
  (maphash (lambda(k v)
	     (when (numberp k)
	       (let* ((nomenclature (second (assoc :nomenclature v)))
		      (name (third (find-element-with-tag nomenclature "Name")))
		      (aliases (mapcan (lambda(el) (split-at-regex el ", {0,1}"))
				       (remove "None known"
					       (remove nil
						       (mapcar 'third (find-elements-with-tag nomenclature "Alias")))
					       :test 'equalp)))
		      (id (parse-integer (attribute-named nomenclature "id"))))
		 (push (cons id (register-or-find-uri name (cons id aliases))) uris))))
	   *lncrnadb-raw*)
  (loop for (id . uri) in uris do (setf (gethash  uri *lncrnadb-raw*) (gethash id *lncrnadb-raw*)))
  )



(defparameter +lncrna-uri+ !obo:NCRO_0004004)

;; Associated Components !

(defun lncrnadb-prolog-axioms ()
  `((declaration (annotation-property !dc:source))))

(defun lncrnadb-declaration-and-label-axioms ()
  (let ((axs nil)
	(editor-preferred-term !obo:IAO_0000111))
    (maphash (lambda(uri labels)
	       (let ((source-annotation `(annotation !dc:source ,(make-uri (format nil "http://www.lncrnadb.org/~a" (car labels))))))
		 (push `(declaration (class ,uri)) axs)
		 (push `(subclass-of ,source-annotation ,uri ,+lncrna-uri+) axs)
		 (push `(annotation-assertion ,source-annotation ,!rdfs:label ,uri ,(car labels)) axs)
		 (push `(annotation-assertion ,source-annotation ,editor-preferred-term ,uri ,(format nil "~a family lncRNA" (car labels))) axs)
		 (loop for syn in (cdr labels)
		       unless (numberp syn)
			 do (push `(annotation-assertion ,source-annotation ,!'alternative term'@iao ,uri ,syn) axs)
		       )))
	     *lncrna-uri2name*)
    axs))

(defun annotation-of-kind (element type)
  (element-with-attribute (car (find-elements-with-tag
				(cddr (find-element-with-tag (second element)
							     "results" "annotation"))
				"annotation")) 
			  "section" type))
  
(defun lncrnadb-comment-axioms ()
  (let ((axs nil))
    (maphash (lambda(key value)
	       (when (uri-p key)
		 (let* ((source-annotation `(annotation !dc:source ,(make-uri (format nil "http://www.lncrnadb.org/~a" (car (gethash key *lncrna-uri2name*))))))
			(annotations (assoc :annotation (gethash key *lncrnadb-raw*)))
			(characteristics (annotation-of-kind annotations "characteristics"))
			(function (annotation-of-kind annotations "function"))
			(expression (annotation-of-kind annotations "expression"))
			(references (annotation-of-kind annotations "references")))
		   (with-output-to-string (s)
		     (when characteristics
		       (format s "Characteristics: ~a~%~%" (third characteristics)))
		     (when function
		       (format s "Function: ~a~%~%" (third function)))
		     (when expression
		       (format s "Expression: ~a~%~%" (third expression)))

		 (push `(annotation-assertion ,source-annotation ,!rdfs:comment ,key ,(get-output-stream-string s)) axs) 
		 ))))
	     *lncrna-uri2name*)
    axs))

(defun lncrnadb-species-axioms  (uri)
  (let ((only-in-taxon !obo:RO_0002160)
	(editor-preferred-term !obo:IAO_0000111)
	(definition !obo:IAO_0000115))
    (loop for species in (mapcar (lambda(e) (Attribute-Named e "Species")) 
				 (find-elements-with-tag  (second (assoc :species (gethash uri *lncrnadb-raw*)))
							  "Entry"))
	  for id = (fresh-lncrna-uri)
	  collect `(declaration (class ,id))
	  collect `(annotation-assertion ,editor-preferred-term ,id ,(format nil "~a ~a lncRNA"
									     (caar (all-matches species "\\((.*)\\)" 1))
									     (car (gethash uri *lncrna-uri2name*))))
	  collect `(annotation-assertion ,!rdfs:label ,id ,(format nil "~a ~a"
								 (caar (all-matches species "\\((.*)\\)" 1))
								 (car (gethash uri *lncrna-uri2name*))))
	  collect `(annotation-assertion ,definition ,id ,(format nil "a ~a lncRNA in the ~a family"
								 (caar (all-matches species "\\((.*)\\)" 1))
								 (car (gethash uri *lncrna-uri2name*))))
	  collect `(subclass-of ,id (object-some-values-from ,only-in-taxon 
					    ,(make-uri nil (format nil "obo:NCBITaxon_~a" 
								  (taxon-id-from-label species)))))
	  collect `(subclass-of ,id ,uri))))


(defvar *lncrnadb-species* nil)

(defun lncrnadb-mentioned-species ()
  (maphash (lambda(k v)
	     (when (numberp k)
	       (let* ((species (mapcar (lambda(e) (attribute-named e "Species"))
				       (find-elements-with-tag (second (assoc :species v)) "species" "Entry"))))
		 (dolist (s species) (pushnew s *lncrnadb-species* :test 'equalp)))))
	   *lncrnadb-raw*))  


    
#|

Characteristics
Expression
Function
Expression

Element
Results>"id"


Annotation.xml
("Results"
 (("id" "0"))
 ("annotation" nil
	       ("annotation" (("section" "Characteristics")) "Ciliated protozoa contain two nuclei, a germline nucleus and a somatic nucleus.
- The somatic nucleus is active but not passed on after fertilization.
- The germline nucleus is transcriptionally inert in the mother cell but is transmitted to progeny. The new somatic nucleus is created from this germline nucleus ((Nowacki 2009)).

In Paramecium tetraurelia creating the new somatic nucleus requires widespread deletion of repetitive sequences and tens of thousands of internal eliminated sequences (IES) to create the compact somatic nucleus in the progeny ((Nowacki 2009)).

Elimination of DNA appears to depend on whether it was present in the maternal somatic nucleus, with DNA present in the maternal somatic nucleus being retained in the progeny somatic nucleus. This process appears to be controlled by interactions between homologous short and long noncoding RNAs ((Nowacki 2009)).")

	       ("annotation" (("section" "Expression")) "DNA in the somatic nucleus is transcribed into sense and antisense noncoding RNAs during both vegetative growth and sexual reproduction ((LepA re 2008)).")
	       ("annotation" (("section" "Function")) "RNA transcripts derived from DNA sequences in the maternal somatic nucleus are necessary but not necessarily sufficient to cause retention of this sequence in the progeny somatic nucleus ((LepA re 2008)).

For example: introduction of an IE sequence into the maternal somatic nucleus is able to maintain the presence of that IES in the somatic nucleus of the progeny in some cases ((Duharcourt 1995), (Duharcourt 1998)).
However degradation of IES RNA transcripts during the vegetal growth stage prevented maintenance of that IES in progeny somatic nuclei, showing long noncoding transcripts from the maternal somatic nuclei were necessary for maintenance of the DNA in progeny ((LepA re 2008)).

Paramecium expresses a complex population of ~25 nuc germline Scan RNAs (scnRNAs) during early meiosis which have homology to all types of germline sequences. Knockdown of scnRNAs allows retention of sequences normally deleted from the progeny somatic nucleus ((LepA re 2008)).
p
ScnRNA : Maternal ncRNA model 
ScnRNAs signal sequences to be deleted while maternal RNAs signal sequences to be maintained. Hypothesised that maternal somatic RNAs deplete homologous germline scnRNAs, preventing excision of these sequences in progeny somatic nuclei. Whereas undepleted scnRNAs do cause excision of target DNA sequences ((LepA re 2008), (LepA re 2008), (Nowacki 2009)).")
	       ("annotation" (("section" "Inheritance")) "Non-mendelian epigenetic inheritance, independent from the germline ((Nowacki 2009)).")
	       ("annotation" (("section" "Misc")) "See Nowacki and Landweber 2009 reivew: Epigenetic inheritance in ciliates, for a wider overview of the field.")
	       ("references" nil ("ref" (("id" "(Nowacki 2009)")) "http://www.ncbi.nlm.nih.gov/pubmed/19879799") ("ref" (("id" "(LepA re 2008)")) "http://www.ncbi.nlm.nih.gov/pubmed/19103667") ("ref" (("id" "(Duharcourt 1995)")) "http://www.ncbi.nlm.nih.gov/pubmed/7649484") ("ref" (("id" "(Duharcourt 1998)")) "http://www.ncbi.nlm.nih.gov/pubmed/9819394"))
	       )
 )

Nomenclature
("Results" (("id" "0"))
	   ("nomenclature" nil
			   ("Name" nil "Maternal somatic nucleus RNAs")
			   ("Alias" nil "Paramecium tetraurelia maternal macronucleus noncoding RNAs")
			   ("Biotype" nil "unassigned")))

Species
("Results" (("id" "2"))
 ("species" nil
  ("Entry" (("Species" "Loris tardigradus (Slender loris)")) "N/A")
  ("Entry"
   (("Species" "Otolemur garnettii (Garnett's greater bushbaby)"))
   "N/A")
  ("Entry" (("Species" "Daubentonia madagascariensis (Aye-aye)"))
   "N/A")
  ("Entry" (("Species" "Propithecus verreauxi (White sifaka)"))
   "N/A")
  ("Entry"
   (("Species" "Cheirogaleus medius (Fat-tailed dwarf lemur)"))
   "N/A")
  ("Entry" (("Species" "Nycticebus pygmaeus (Pygmy slow loris)"))
   "N/A")
  ("Entry" (("Species" "Lemur coronatus (Crowned lemur)")) "N/A")
  ("Entry"
   (("Species" "Galago moholi (South African lesser bushbaby)"))
   "N/A")
  ("Entry"
   (("Species" "Lepilemur dorsalis (Grey-backed sportive lemur)"))
   "N/A")
  ("Entry"
   (("Species" "Lepilemur ruficaudatus (Red-tailed sportive lemur)"))
   "N/A")
  ("references" nil)))
|#
