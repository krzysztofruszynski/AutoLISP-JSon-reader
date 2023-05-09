
	(load "D:\\LISP\\JSon\\LispUnit.lsp")
	(load "D:\\LISP\\JSon\\JSonParser.lsp")
	

(defun C:TG ()
	(load "D:\\LISP\\JSon\\JSonUnitTest.lsp")
	
	; https://www.json.org/json-en.html
	
	(JSonParser_TextValueTest)	; do sprawdzenia tab, ukośniki unicode x432 cytowania
	(JSonParser_ArrayValueTest)
	(JSonParser_ObjectValueTest)
	; (JSonParser_ComplexValueTest)
	; (JSonParser_NestedValueTest)
	(JSonParser_IntValueTest)
	(JSonParser_FloatValueTest) ; do sprawdzenia liczby ujemne
	(JSonParser_EngineeringFormatValueTest) ; do sprawdzenia duże E
	(JSonParser_TrueValueTest)
	(JSonParser_FalseValueTest)
	(JSonParser_NullValueTest)
	(JSonParser_WhitespacesTest)
	(JSonParser_MultilinesTest)
	
)

(defun JSonParser_TextValueTest ()
	(setq text "{\"name\":\"John\",\"from\":\"USA\", \"hobby\":\"reading\"}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"name\"" result ) ) "\"John\"" )
	nil
)

(defun JSonParser_ArrayValueTest ()
	(setq text "{\"name\":\"John\",\"from\":\"USA\", \"hobby\":[\"reading\",\"sleeping\",\"bball\"]}")
	(setq result (JSon:Parse text))
	;(print (cdr (assoc "\"hobby\"" result))  )
	
	(setq expected (list ( cons "\"name\"" "\"John\"") 
						(cons "\"from\"" "\"USA\"") 
						(cons "\"hobby\"" (list 
							(list "\"reading\"" "\"sleeping\"" "\"bball\"")))
	))
	(Assert:AreEqual result expected)
)
(defun JSonParser_ObjectValueTest ( / text result expected )
	(setq text "{\"name\":\"John\",\"from\":\"USA\", \"hobby\":[\"reading\",\"sleeping\",\"bball\"],\"food\":{\"Chinese\":\"Gong Bao Chicken\",\"Japanese\":\"sushi\"}}")
	(setq result (JSon:Parse text))
	(setq expected (list ( cons "\"name\"" "\"John\"") 
						(cons "\"from\"" "\"USA\"") 
						(cons "\"hobby\"" (list 
							(list "\"reading\"" "\"sleeping\"" "\"bball\"")))
						(cons "\"food\"" (list 
							(cons "\"Chinese\"" "\"Gong Bao Chicken\"")
							(cons "\"Japanese\"" "\"sushi\"")
						))
	))
	(Assert:AreEqual result expected)
)

(defun JSonParser_ComplexValueTest ()
	(setq text "{\"name\":\"John\",\"from\":\"USA\",\"hobby\":[\"reading\",\"sleeping\",\"bball\"], \"sex\":[],\"language\":[\"Mongolian\"],\"food\":{\"Chinese\":\"fried rice\",\"Japanese\":[\"sushi\",\"noodle\",\"hot pot\"]}}")
	(setq result (JSon:Parse text))
	(print result)
	nil
)
(defun JSonParser_NestedValueTest ()
	(setq text "{\"name\":\"John\",\"from\":\"USA\", \"hobby\":[\"reading\",\"sleeping\",\"bball\"], \"sex\":[],\"language\":[\"Mongolian\"],\"food\":{\"Chinese\":{\"soup\":\"egg soup\",\"dishes\":\"fried chicken\"},\"Japanese\":[\"sushi\",\"noodle\",\"hot pot\"]}}")
	(setq result (JSon:Parse text))
	(print result)
	nil
)

(defun JSonParser_IntValueTest ()
	(setq text "{\"name\":\"John\",\"age\":23,\"weight\":80.7}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"age\"" result ) ) 23 )
	nil
)

(defun JSonParser_FloatValueTest (/ text result)
	(setq text "{\"name\":\"John\",\"weight\":80.7}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"weight\"" result)) 80.7 )
	(princ)
)

(defun JSonParser_EngineeringFormatValueTest ( / text result)
	(setq text "{\"name\":\"John\",\"IQ\":1.23e2}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"IQ\"" result)) 123.0)
	(princ)
)

(defun JSonParser_TrueValueTest ()
	(setq text "{\"name\":\"John\",\"isActiveMember\":true}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"isActiveMember\"" result)) :vlax-true )
	(princ)
)

(defun JSonParser_FalseValueTest ()
	(setq text "{\"name\":\"John\",\"IsCitizen\":false}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"IsCitizen\"" result)) :vlax-false )
	(princ)
)

(defun JSonParser_WhitespacesTest ( / text result)
	(setq text "{\"name\" : \"John\" , \"IQ\" : 1.23e2 }")
	(setq result (JSon:Parse text))
	(Assert:IsTrue (and 
			(= (length result ) 2 )
			(type (cdr (car result )) 'STR )
			(type (cdr (car result )) 'REAL )
			(eq (cdr (assoc "\"name\"" result)) "\"John\"")
			(eq (cdr (assoc "\"IQ\"" result)) 123.0)
				) 
	)
	(princ)
)



(defun JSonParser_NullValueTest ()
	(setq text "{\"name\":\"Albert\",\"Citizenship\":null}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"Citizenship\"" result)) nil )
	(princ)
)
(defun JSonParser_MultilinesTest ()
	(setq text "{
	\"name\":\"Benjamin\",
	\"lastname\":\"Franklin\"
	}")
	(setq result (JSon:Parse text))
	(Assert:AreEqual (cdr (assoc "\"lastname\"" result)) "\"Franklin\"" )
	(princ)
)






