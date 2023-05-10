 
 
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