 
 
(defun JSon:Parse( s / d idx start_idx end_idx key sep v arraybegin )  
		; (setq s text)
	; https://medium.com/@mingat.dalai/lets-make-a-json-parser-102382c7a010
	
	(setq d (list ))
	(setq idx 0)
	(while (not(null (String:Find s "\"" (1+ idx) ) ) )
		(progn
			(setq start_idx  (String:Find s "\"" (1+ idx)))
			(setq end_idx  (String:Find s "\"" (1+ start_idx )))
			(setq key (substr s (1+ start_idx) (1+(- end_idx start_idx) ) ))
			(setq sep (String:Find s ":" end_idx ) )
			(setq nextParam (String:Find s "," (1+ sep)))
			
			(setq wordbegin (String:Find s "\"" (1+ sep) ) )
			(if (not(null wordbegin) )
				(setq wordend (String:Find s "\"" (1+ wordbegin ) ) )
			)
		)
		(cond
			( (String:IsBetween s "[" (1+ sep) (+ 2 sep)) (progn
			(setq v (list))
			(setq idx (1+ (String:Find s "[" (1+ sep) ) ) )
			(while (and (>= idx (String:Find s "[" (1+ sep))) (< idx (String:Find s "]" (1+ sep))))
				(setq start_idx  (String:Find s "\"" idx))
				(setq end_idx  (String:Find s "\"" (1+ start_idx )))
				(setq v (append v (list (substr s (1+ start_idx) (1+(- end_idx start_idx) ) ) ) ) )
				(setq idx (1+ end_idx))
			)		
			(setq v (list v))
		  ) )
		  
		  ( (String:IsBetween s "{" (1+ sep) (+ 2 sep)) (progn
			(setq v (list))
			(setq start_idx  (String:Find s "{" (+ 1 sep)))
			(setq end_idx (1+ (String:Find s "}" start_idx)))
			(setq start start_idx)
			(setq end_idx (String:Find s "}" (1+ start)))
			
			(while (not(null (String:IsBetween s "{" (1+ start) end_idx)))
				(setq start  (String:IsBetween s "{" (1+ start) end_idx ))
				(setq end_idx (1+ end_idx))
			)
			(setq end_idx (String:Find s "}" end_idx))
			
			
			(setq sx (substr s (1+ start_idx) (1+ (- end_idx start_idx))))
			(setq v (JSon:Parse sx))
			(setq idx end_idx )
		  ))
		  		  
		  ( (and wordbegin wordend 
				 (or (null nextParam) ( < wordend nextParam) )
			) (progn 
			(setq start_idx  (String:Find s "\"" (1+ sep)))
			(setq end_idx  (String:Find s "\"" (1+ start_idx )))
			(setq v (substr s (1+ start_idx) (1+(- end_idx start_idx) ) ))
			(setq idx end_idx)
		  ))
		  (t(progn						
			(setq start_idx  (1+ sep))						
			(setq end_idx  (String:Find s "," (1+ start_idx )))
			(if (null end_idx) 
				(setq end_idx  (String:Find s "}" (1+ start_idx )))
			)
			(setq v (substr s (1+ start_idx) (- end_idx start_idx) ) )
			(setq v (cond 
				((eq "true" v) :vlax-true)
				((eq "false" v ) :vlax-false)
				((eq "null" v ) nil)
				(t (read v))
			))			
			(setq idx end_idx)
		  ))
		)
		(setq d (append d (list(cons key v))))
	)
	d
)



(defun String:Find ( s toFind startPosition / )
	(vl-string-position (ascii toFind) s startPosition)
)
(defun String:IsBetween ( s toFind startPosition endPosition / sx )
	(setq sx (vl-string-position (ascii toFind) s startPosition))
	(if (and (not (null sx )) (< sx endPosition ))
		sx
		nil
	)
)


;|
(setq text "{\"name\":\"John\",\"from\":\"USA\", \"hobby\":\"reading\"}")
(setq result (JSon:Parse text) )
|;
 
 
; (defun JSon:Parse ( text / 
; 	(defun *error* ( msg / )
;        (if (not (null msg ) ) (progn (princ "\n JSon:Parse:*error*: " ) (princ msg ) (princ "\n") ) )
; 	))
; 	
; 	(setq len (strlen text))
; 	(setq i 1)
; 	
; 	(while (< i len)	; (setq i 1)
; 		(setq char (substr text i 1))
; 		;(print char)
; 		(cond 
; 			( (JSon:isObjectBegin char ) 	(JSon:getToTheEnd char))
; 			( (JSon:isObjectEnd char ) 		(JSon:objectEndFound) )
; 			( (JSon:isArrayBegin char ) 	(JSon:getToTheEnd ))
; 			( (JSon:isArrayEnd char ) 		(JSon:arrayEndFound ))
; 			( T 							(JSon:letterFound char))
; 			
; 			
; 			;( t (setq result (append result (list char))) )
; 		)
; 		
; 		(setq i (1+ i ))
; 	)
; )
;  
; (defun JSon:isWhiteSign ( char / )
; 	(not(null(member char (list " " "\t"))	))
; )
; 
; ( defun JSon:isObjectBegin ( char / )(= char "{") )
; ( defun JSon:isNewLine ( char / )(= char "\n") )
; ( defun JSon:isObjectEnd ( char / )	(= char "}") )
; ;( defun JSon:isWordBegin ( char / )	(and (= char "\"") ( null *Json:wordBuffer* ) ) )
; ;( defun JSon:isWordEnd ( char / )	(and (= char "\"") ( not ( null *Json:wordBuffer* ) ) ) )
; ( defun JSon:isValueSeparator ( char / )	(= char ":") ) 
; ( defun JSon:isNextParam ( char / )	(= char ",") )
; ( defun JSon:isArrayBegin ( char / )	(= char "[") )
; ( defun JSon:isArrayEnd ( char / )	(= char "]") )
;  
;   
; (defun JSon:objectBeginFound ( / )
; 	(print "JSon:objectBeginFound")
; )
; 
; (defun JSon:objectEndFound ( / )
; 	(print "JSon:objectEndFound")
; 	(JSon:AttachBufferToObject)
; 	(setq *JSon:result* (append *JSon:result*  *Json:object*))
; )
; (defun JSon:NewLineFound ( / )
; 	(print "JSon:NewLineFound")
; )
; (defun JSon:wordBeginFound ( / )
; 	(print "JSon:wordBeginFound")
; 	( JSon:resetWordBuffer )
; )
; (defun JSon:wordEndFound ( / )
; 	(print "JSon:wordEndFound")
; )
; (defun JSon:valueSeparatorFound ( / )
; 	(print "JSon:valueSeparatorFound")
; 	(setq *Json:ActiveName* *Json:wordBuffer*)
; 	( JSon:resetWordBuffer )
; )
; (defun JSon:nextParamFound ( / )
; 	(print "JSon:nextParamFound")
; 	(JSon:AttachBufferToObject)
; 	( JSon:resetActiveName )
; 	( JSon:resetWordBuffer )
; )
; 
; (defun JSon:BufferValue ( / val)
; 	(print "JSon:BufferValue")
; 	(setq val *Json:wordBuffer*)	
; 	(cond
; 		( (JSon:isTrue val ) t)
; 		( (JSon:isText val ) val)
; 	)
; )
; 
; (defun JSon:AttachBufferToObject ()
; 	(setq rval (JSon:BufferValue))
; 	(print *Json:ActiveName*) (print rval)	
; 	(setq *Json:object* (append *Json:object* (list (cons *Json:ActiveName* rval))))	
; 	
; )
; 
; (defun JSon:isTrue (val / )	
; 	(princ "JSon:isTrue") (princ val)
; 	(= val "true")
; )
; 
; (defun JSon:isText (val / )
; 	(princ "JSon:isText") (princ val)
; 	(and (= (substr val 1 1) "\"") (= (substr val (strlen val) 1) "\"") )	
; )
; 
; (defun JSon:resetWordBuffer ()
; 	(setq *Json:wordBuffer* "")
; )
; ( defun JSon:resetActiveName ()
; 	( setq *Json:ActiveName*  nil)
; )
; 
; (defun JSon:arrayBeginFound ( / )
; 	(print "JSon:arrayBeginFound")
; )
; (defun JSon:arrayEndFound ( / )
; 	(print "JSon:arrayEndFound")
; 	
; )
; (defun JSon:letterFound (char / )
; 	;(princ "\n") (princ "JSon:letterFound" ) (print char)
; 	(setq *Json:wordBuffer* (strcat *Json:wordBuffer* char))
; 	;(print *Json:wordBuffer*)
; )
;   
; 
; (progn ; section
; 	(defun JSon:startNewSectionBufer (  / )
; 		(setq *Json:object* nil)
; 	)
; 	(defun JSon:finishSectionBufer (  / )
; 		(print "JSon:finishSectionBufer"	)	
; 		(setq *JSon:result* (append *JSon:result* (list *Json:object*)))
; 	)
; )

