(defvar *lncrnadb* (make-hash-table :test 'equalp))

(defun load-lncrnadb ()
  (loop for source-file in '("annotation.xml" "literature.xml" "nomenclature.xml" "sequence.xml" "species.xml")
	for key in '(:annotation :literature :nomenclature :sequence :species)
	for path = (asdf/system:system-relative-pathname "lncrnao" (format nil "../database/lncrnadb/~a" source-file))
	for xml = (with-open-file (f path) (xmls:parse  f))
	do
	   (dolist (entry (find-elements-with-tag xml "Results"))
	     (let ((id (parse-integer (attribute-named entry "id"))))
	       (push (list key entry)  (gethash id *lncrnadb*)))))
  (maphash (lambda(id entries)
	     (setf (gethash (third (find-element-with-tag (second (assoc :nomenclature entries))  "nomenclature" "name"))
			    *lncrnadb*)
		   entries))
	   *lncrnadb*))
#|	     
			     
)))

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
