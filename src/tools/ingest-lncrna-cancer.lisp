;("16" "AC079776.2" "AC079776.2" "ENSG00000214100" "N/A" "chr2:129922863-129934317" "lung adenocarcinoma" "C34" "M8140/3" "microarray, qPCR etc." "lung adenocarcinoma tissue" "down-regulated" "We initially identified a number of interesting candidate lncRNAs for further analysis. Of these, LOC100132354 and RPLP0P2 exhibited the most significantly changed expression in our analysis of 100 pairs of lung adenocarcinoma and normal lung tissue samples. The expression of LOC100132354 was significantly higher in lung adenocarcinoma than in the adjacent tissues, while the expression of RPLP0P2 was significantly lower in lung adenocarcinoma than in the adjacent tissues." "25089627" "2014" "Long noncoding RNA expression profiles of lung adenocarcinoma ascertained by microarray analysis.")

;; Headers:
;; LncRNA name
;; Synonyms
;; Ensembl ID
;; Refseq ID
;; Position
;; Cancer name
;; ICD-0-3-T
;; ICD-0-3-M
;; Methods
;; Sample (tissue/ cell line)
;; Expression pattern
;; Functional description
;; PubMed ID
;; Year
;; Title

(defvar *lncrna-classes* nil)
(defun conclusion-ontology-axioms ()
  (let ((parent !'conclusion based on data'@obi)
	(cex-cancer  !obi:New_request_1)
	(cex-cancer-up !obi:New_request_2)
	(cex-cancer-down !obi:New_request_3)
	(cex-cancer-changes !obi:New_request_4)
	(about !'is about'@iao)
	(gene-expression !obo:GO_0010467)
	(lncrna-expression !obo:GO_New_request_1)
	(participates !'participates in'@ro)
	(lncrna !obo:NCRO_0004004))
    (pushnew `("up-regulated" ,cex-cancer-up) *lncrna-classes* :test 'equalp)
    (pushnew `("down-regulated" ,cex-cancer-down) *lncrna-classes* :test 'equalp)
    (pushnew `("differential expression" ,cex-cancer-changes) *lncrna-classes* :test 'equalp)
    `((declaration (class !obi:New_request_1))
      (declaration (class !obo:NCRO_0004004))
      (declaration (class ,lncrna-expression))
      (declaration (class ,cex-cancer-changes))
      (declaration (class ,cex-cancer-up))
      (declaration (class ,cex-cancer-down))
      (declaration (object-property ,participates))
      (declaration (object-property ,about))
      (declaration (annotation-property !'definition'@iao))
      (declaration (annotation-property !'definition source'@iao))
      (annotation-assertion !rdfs:label ,cex-cancer "conclusion regarding expression of lncRNA in cancer")
      (subclass-of ,cex-cancer ,parent)
      (subclass-of ,cex-cancer (object-some-values-from ,about ,(car (gethash "cancer" *hdo-name-to-id*))))
      (subclass-of ,cex-cancer (object-max-cardinality 1 ,about  ,(car (gethash "cancer" *hdo-name-to-id*))))
      (annotation-assertion !rdfs:label ,cex-cancer-up "conclusion of increased expression of lncRNA in cancer")
      (annotation-assertion !rdfs:label ,cex-cancer-down "conclusion of decreased expression of lncRNA in cancer")
      (annotation-assertion !rdfs:label ,cex-cancer-changes "conclusion of changed expression of lncRNA in cancer")
      (subclass-of ,cex-cancer-changes ,cex-cancer)
      (subclass-of ,cex-cancer-up ,cex-cancer)
      (subclass-of ,cex-cancer-down ,cex-cancer)
      (declaration (class ,lncrna-expression))
      (subclass-of ,cex-cancer (object-some-values-from ,about ,lncrna-expression))
      (annotation-assertion !rdfs:label ,lncrna-expression "expression of lncRNA")
      (subclass-of ,lncrna-expression ,gene-expression)
      (subclass-of ,lncrna-expression (object-some-values-from ,participates ,lncrna))
      )))

(defvar *pmid-instances* (make-hash-table :test 'equalp))

(defun read-lncrna-cancer-assocations ()
  (setq *pmid-instances* (make-hash-table :test 'equalp))
  (let ((missing nil))
  (unless *hdo-name-to-id* (index-disease-ontology *hdo*))
  (let ((path (asdf:system-relative-pathname "lncrnao" "../database/lnc2cancer/lncRNA cancer association.txt")))
    (with-open-file (f path)
      (read-line f nil :eof)
      (loop for line = (read-line f nil :eof)
	    for (lineno name  synonyms ensembleid refseqid position cancer-name ICD-T ICD-M methods sample expression description pubmed year title) = (unless (symbolp line) (split-at-char line #\tab))
	    repeat 200
	    until (eq line :eof)
	    for lncrna = name
	    when (gethash lncrna *lncrna-name2uri*) append
	    (make-lncrna-expression-conclusion expression lncrna pubmed title cancer-name description)

;	       (print-db name synonyms ensembleid refseqid position cancer-name ICD-T ICD-M methods sample expression description pubmed year title)
;	    do	       (pushnew  cancer missing :test 'equalp)
;	       (unless (or (cancer-name-to-do cancer-name) (cancer-name-to-do ICD-T))  (pushnew  (list cancer-name icd-t icd-m)  missing :test 'equalp))
	    )))
;;    missing
))

;; these didn't match labels or exact synonyms
;; http://www.bio-bigdata.com/lnc2cancer/down.jsp
(defvar *hdo-name-substitutions*
  '(("parotid cancer" "C07.9" "" "Parotid gland cancer") ;; exact
   ("B-lymphoblastic leukemia" "" "M9811/3" "lymphoblastic leukemia")  ;;unsure
   ("B cell acute lymphoblastic leukemia" "" "M9826/3" "lymphoblastic leukemia") 
   ("serous ovarian cancer" "C56.9" "M8441/3" "ovarian cancer") ;; subclass missing
   ("esophageal squamous cell carcinoma" "C15" "M8070/3" "esophagus squamous cell carcinoma")  ;; missing subclass
   ("esophageal adenocarcinoma" "C15" "M8140/3" "adenocarcinoma")  ;; subclass?
   ("non-small cell lung cancer" "C34" "M8046/3" "non-small cell lung carcinoma")  ;; exact
   ("urothelial carcinoma of the bladder" "C67" "M8120/3" "Transitional cell carcinoma")  ;; exact
   ("hypopharyngeal squamous cell carcinoma" "C13" "M8070/3" "hypopharynx cancer") 
    ("gastric cancer" "C16" "" "stomach cancer")))


(defun make-lncrna-expression-conclusion (change lncrna pmid pmid-title cancer description)
  (let ((pubmed-id !OBI:0001617)
	(pmid-uri (make-uri (format nil "https://www.ncbi.nlm.nih.gov/pubmed/~a" pmid)))
    	(about !'is about'@iao)
	(definition !'definition'@iao)
	(axioms nil)
	(source !obo:IAO_0000119)
	(class (second (assoc change *lncrna-classes* :test 'equalp))))
    (assert class () change)
    (unless (gethash pmid *pmid-instances*)
      (push `(declaration (named-individual ,pmid-uri)) axioms)
      (push `(annotation-assertion ,!rdfs:label ,pmid-uri ,pmid-title) axioms)
      (push `(class-assertion ,pubmed-id ,pmid-uri) axioms)
      (setf (gethash pmid *pmid-instances*) pmid-uri))
    (let ((lncrna-expression !obo:GO_New_request_1)
	  (conclusion (fresh-lncrna-uri)))
      (push `(declaration (class ,(car (gethash lncrna *lncrna-name2uri*)))) axioms)
      (push `(declaration (named-individual ,conclusion)) axioms)
      (push `(class-assertion ,class ,conclusion) axioms)
      (push `(declaration (class ,(cancer-name-to-do cancer))) axioms)
      (push `(class-assertion  (object-some-values-from ,about ,(cancer-name-to-do cancer)) ,conclusion) axioms)
      (push `(class-assertion  (object-some-values-from ,about ,(car (gethash lncrna *lncrna-name2uri*))) ,conclusion) axioms)
      (push `(annotation-assertion ,!rdfs:label ,conclusion ,(format nil "~a ~a in ~a"  change lncrna cancer )) axioms)
      (push `(annotation-assertion ,definition ,conclusion ,description) axioms)
      (push `(object-property-assertion ,about ,conclusion ,(car (gethash lncrna *lncrna-name2uri*))) axioms)
      (push `(object-property-assertion ,about ,conclusion ,lncrna-expression) axioms)
      (push `(declaration (class ,(cancer-name-to-do cancer))) axioms)
      (push `(object-property-assertion ,about ,conclusion ,(cancer-name-to-do cancer)) axioms)
      (push `(annotation-assertion ,source ,conclusion ,pmid-uri) axioms))
    axioms))
    


(defun cancer-name-to-do (name)
  (index-disease-ontology *hdo*)
  (let ((result (or  (gethash name *hdo-name-to-id*)
		     (gethash (car (last (find name *hdo-name-substitutions* :key 'car :test 'equalp))) *hdo-name-to-id*))))
    (if (= (length result) 1)
	(car result)
	(if (= (length result) 2)
	    (if (#"matches" (uri-full (car result)) ".*DOID.*")
		(car result)
		(second result)
		)))))

(defvar *hdo-name-to-id* nil)

(defun index-disease-ontology (ont)
  (when (null *hdo-name-to-id*)
    (setq *hdo-name-to-id*  (make-hash-table :test 'equalp))
    (each-entity-label ont (list !rdfs:label !oboinowl:hasExactSynonym)
		       (lambda(uri propuri value)
			 (push uri (gethash value *hdo-name-to-id*))))))



