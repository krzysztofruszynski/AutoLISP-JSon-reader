

; (setq jsondata (Json:Read "D:\\LISP\\JSon\\data.json"))

(defun JSonTestData ()
	(list 
		(cons "type" "polyline") 
		(cons "points" (list 
			(list (cons "type" "line") (cons "x" 35)(cons "y" -26) )
			(list (cons "type" "line") (cons "x" 35) (cons "y" 26) )
			(list (cons "type" "line") (cons "x" 28.5)(cons "y" 32.5) )
			(list (cons "type" "line") (cons "x" -28.5) (cons "y" 32.5) )
		))
		(cons "isClosed" T)
		(cons "hasArc" T)
	) 
)

(defun C:RT ()
	(load "D:\\LISP\\JSon\\JSonTest.lsp")
	(load "D:\\LISP\\JSon\\JSonParser.lsp")
	(JSonParser_IntergrationTest)
	(JSonModel_To_PoylineModel_Test)
	(PolylineModel_Draw_Test)
)


(defun JSonParser_IntergrationTest ( / )
(setq jsontxt "
 {
 \"type\": \"polyline\",
 \"points\": [
   {
     \"type\": \"line\",
     \"x\": 35,
     \"y\": -26
   },
   {
     \"type\": \"line\",
     \"x\": 35,
     \"y\": 26
   },
   {
     \"type\": \"line\",
     \"x\": 28.5,
     \"y\": 32.5
   },
   {
     \"type\": \"line\",
     \"x\": -28.5,
     \"y\": 32.5
   }
   ],
 \"isClosed\": true,
 \"hasArc\": true
} "
)

(setq jsondata (JSon:Parse jsontxt))

;(setq jsondata (JSonTestData))

	
	
		(= (type (cdr (assoc "point" jsondata ))) 'LIST)
		(= (length (cdr (assoc "point" jsondata))) 4)
		(= (cdr (assoc "isClosed" jsondata)) T)
		(= (cdr (assoc "hasArc" jsondata)) T)
	)
)

(defun JSONPolylineSampleDefinition()
	(list 
		(cons 'Segments (list
			(list 	
				(cons 'Type 'Line)
				(cons 'NextPoint (list 35 -26))
			)
			(list 	
				(cons 'Type 'Line)
				(cons 'NextPoint (list 35 26))
			)
			(list 	
				(cons 'Type 'Line)
				(cons 'NextPoint (list 28.5 32.5))
			)
			(list 	
				(cons 'Type 'Line)
				(cons 'NextPoint (list -28.5 32.5))
			)
		))
	)
	
	(and
		nil
	)
)

(defun JSonModel_To_PoylineModel_Test ()
	(setq jsondata (JSonTestData))
	(setq PolylineData (JSONPolylineSampleDefinition))
)

(defun PolylineModel_Draw_Test ()
	(setq PolylineData (JSONPolylineSampleDefinition))
	(setq drawnPolyline(Polyline:Draw PolylineData))
	
	(and
		(not (null drawnPolyline))
		(= (vlax-get-property drawnPolyline 'Length ) validlength)
	)
)

(defun Polyline:Draw ( polylineData / )
	nil
)
