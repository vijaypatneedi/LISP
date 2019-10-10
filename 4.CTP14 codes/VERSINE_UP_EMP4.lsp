(DEFUN C:VERSINE_UP();/ BLOCKS_LIST MAST_TTC_DIR ENT_LIST DFCC_TRACK_CENTER IR_TRACK_CENTER I MAST ROTATION INSERTION_POINT PROJECTION_POINT1 PROJECTION_POINT2 FINAL_INSERTION_POINT)
;(SETQ ENT_LIST (FORM_SSSET (SSGET)))
  (SETQ BLOCKS_LIST (FORM_SSSET (SSGET "X" (LIST (CONS 0 "INSERT")))))
  (SETQ MAST_TTC_DIR (YARD_DATA_COLLECT_GPS BLOCKS_LIST '("SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "TTC" "SS0" "SS1" "SS2" "SS3" "SS4") 1 5))
  (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST '("UP") MAST_TTC_DIR 1) 0))
  
(SETQ DFCC_TRACK_CENTER (CAR(ENTSEL "\n SELECT DFCC TRACK CENTER:")))
;(SETQ IR_TRACK_CENTER (CAR(ENTSEL "\n SELECT IR TRACK CENTER:")))
(SETQ DFCC_TRACK_CENTER (VLAX-ENAME->VLA-OBJECT DFCC_TRACK_CENTER))
;(SETQ IR_TRACK_CENTER (VLAX-ENAME->VLA-OBJECT IR_TRACK_CENTER))
(SETQ I 0 J 0 MAST NIL ROTATION NIL INSERTION_POINT NIL PROJECTION_POINT1 NIL PROJECTION_POINT2 NIL FINAL_INSERTION_POINT NIL)
;(SETQ PLINE_LIST (list '(0 0 0)))
(SETQ PLINE_LIST NIL)

(WHILE (< J (LENGTH ENT_LIST))
  (SETQ MAST (VLAX-ENAME->VLA-OBJECT (NTH J ENT_LIST)))
  ;(SETQ ROTATION (VLAX-GET-PROPERTY MAST 'ROTATION))
  (SETQ INSERTION_POINT (VLAX-SAFEARRAY->LIST (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY MAST 'INSERTIONPOINT))))
  (SETQ PROJECTION_POINT1 (VLAX-CURVE-GETCLOSESTPOINTTO DFCC_TRACK_CENTER INSERTION_POINT T))
  (SETQ PLINE_LIST (append PLINE_LIST (list PROJECTION_POINT1)))
  ;(SETQ TEMP_DIST (VLAX-CURVE-GETDISTATPOINT DFCC_TRACK_CENTER PROJECTION_POINT1))
   
   (SETQ J (+ J 1))
 )
 
 (command "PLINE" (foreach pt PLINE_LIST (command pt)))
 (SETQ IR_TRACK_CENTER (ENTLAST))
  (SETQ DEL_ENTITY IR_TRACK_CENTER)
 (SETQ IR_TRACK_CENTER (VLAX-ENAME->VLA-OBJECT IR_TRACK_CENTER))
 
 
 

   





(WHILE (< I (LENGTH ENT_LIST))
  (SETQ MAST (VLAX-ENAME->VLA-OBJECT (NTH I ENT_LIST)))
  (SETQ ROTATION (VLAX-GET-PROPERTY MAST 'ROTATION))
  (SETQ INSERTION_POINT (VLAX-SAFEARRAY->LIST (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY MAST 'INSERTIONPOINT))))
  (SETQ PROJECTION_POINT1 (VLAX-CURVE-GETCLOSESTPOINTTO DFCC_TRACK_CENTER INSERTION_POINT T))
  ;(SETQ PLINE_LIST (append pline_list (list projection_point1)))
  (SETQ TEMP_DIST (VLAX-CURVE-GETDISTATPOINT DFCC_TRACK_CENTER PROJECTION_POINT1))
   
   (SETQ ENT_OBJECT MAST)
  (SETQ SAFEARRAY_SET NIL)
  ;(SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	    )
	  )
    )
  )
 )
  
  (SETQ DIST_ADD (ATOF (vlax-get-property (NTH 1 SAFEARRAY_SET) 'TEXTSTRING)))
   (SETQ FINAL_DIST (+ TEMP_DIST (/ DIST_ADD 2.00)))
   (SETQ PROJECTION_POINT1 (vlax-curve-getPointAtDist DFCC_TRACK_CENTER FINAL_DIST))
    
  
  (SETQ PROJECTION_POINT2 (VLAX-CURVE-GETCLOSESTPOINTTO IR_TRACK_CENTER PROJECTION_POINT1 T))
  (SETQ VERS (* 1000 (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2)))
  
   (COND  (( >= VERS 300) (SETQ VERS 300))
          ((AND (< VERS 300) (>= VERS 290)) (SETQ VERS 300))
         ((AND (< VERS 290) (>= VERS 280)) (SETQ VERS 290))
	((AND (< VERS 280) (>= VERS 270)) (SETQ VERS 280))
	((AND (< VERS 270) (>= VERS 260)) (SETQ VERS 270))
	((AND (< VERS 260) (>= VERS 250)) (SETQ VERS 260))
	((AND (< VERS 250) (>= VERS 240)) (SETQ VERS 250))
		  ((AND (< VERS 240) (>= VERS 230)) (SETQ VERS 240))
          ((AND (< VERS 230) (>= VERS 220)) (SETQ VERS 230))
		  ((AND (< VERS 220) (>= VERS 210)) (SETQ VERS 220))
		  ((AND (< VERS 210) (>= VERS 200)) (SETQ VERS 210))
		  ((AND (< VERS 200) (>= VERS 190)) (SETQ VERS 200))
		  ((AND (< VERS 190) (>= VERS 180)) (SETQ VERS 190))
		  ((AND (< VERS 180) (>= VERS 170)) (SETQ VERS 180))
		  ((AND (< VERS 170) (>= VERS 160)) (SETQ VERS 170))
		  ((AND (< VERS 160) (>= VERS 150)) (SETQ VERS 160))
		  ((AND (< VERS 150) (>= VERS 140)) (SETQ VERS 150))
		  ((AND (< VERS 140) (>= VERS 130)) (SETQ VERS 140))
		  ((AND (< VERS 130) (>= VERS 120)) (SETQ VERS 130))
		  ((AND (< VERS 120) (>= VERS 110)) (SETQ VERS 120))
		  ((AND (< VERS 110) (>= VERS 100)) (SETQ VERS 110))
		  ((AND (< VERS 100) (>= VERS 90)) (SETQ VERS 100))
		  ((AND (< VERS 90) (>= VERS 80)) (SETQ VERS 90))
		  ((AND (< VERS 80) (>= VERS 70)) (SETQ VERS 80))
		  ((AND (< VERS 70) (>= VERS 60)) (SETQ VERS 70))
		  ((AND (< VERS 60) (>= VERS 50)) (SETQ VERS 60))
		  ((AND (< VERS 50) (>= VERS 40)) (SETQ VERS 50))
		  ((AND (< VERS 40) (>= VERS 30)) (SETQ VERS 40))
		  ((AND (< VERS 30) (>= VERS 20)) (SETQ VERS 30))
		  ((AND (< VERS 20) (>= VERS 0)) (SETQ VERS 0))
		  
		  
	)
   
   (IF (/= VERS 0)
    (PROGN
   
		(IF (>= (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 16)
			(SETQ FINAL_INSERTION_POINT (POLAR PROJECTION_POINT1 (+ (* 3 (/ PI 4)) ROTATION) 4.0)) ;(/ (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 4)))
			(SETQ FINAL_INSERTION_POINT (POLAR PROJECTION_POINT1 (+ (* 3 (/ PI 4)) ROTATION) 4.0))
		)
		
		
			
		  (IF (<= (ANGLE PROJECTION_POINT1 PROJECTION_POINT2) 3.14)
		   (PROGN
			(COMMAND "INSERT" "VERSINE_LEFT" FINAL_INSERTION_POINT 1 1 (* 1 (/ 180 PI) ROTATION) )
			(MODIFY_ATTRIBUTES (ENTLAST) (LIST "VERSINE") (LIST (RTOS VERS 2 0)));;;;;(STRCAT "TC=" (RTOS (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 2 2))))
	       )
		   
		  (PROGN
		   (COMMAND "INSERT" "VERSINE" FINAL_INSERTION_POINT 1 1 (* 1 (/ 180 PI) ROTATION) )
		   (MODIFY_ATTRIBUTES (ENTLAST) (LIST "VERSINE") (LIST (RTOS VERS 2 0)));;;;;(STRCAT "TC=" (RTOS (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 2 2))))
		   )
		 )
	
	
   )
   
 )
  
  
  (SETQ I (+ I 1))
)

 (ENTDEL DEL_ENTITY)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN C:VERSINE_NEW_UPLINE();/ BLOCKS_LIST MAST_TTC_DIR ENT_LIST DFCC_TRACK_CENTER IR_TRACK_CENTER I MAST ROTATION INSERTION_POINT PROJECTION_POINT1 PROJECTION_POINT2 FINAL_INSERTION_POINT)
;(SETQ ENT_LIST (FORM_SSSET (SSGET)))
  (SETQ BLOCKS_LIST (FORM_SSSET (SSGET "X" (LIST (CONS 0 "INSERT")))))
  (SETQ MAST_TTC_DIR (YARD_DATA_COLLECT_GPS BLOCKS_LIST '("SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "TTC" "SS0" "SS1" "SS2" "SS3" "SS4") 1 5))
  (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST '("DN") MAST_TTC_DIR 1) 0))
  
(SETQ DFCC_TRACK_CENTER (CAR(ENTSEL "\n SELECT DFCC TRACK CENTER:")))
;(SETQ IR_TRACK_CENTER (CAR(ENTSEL "\n SELECT IR TRACK CENTER:")))
(SETQ DFCC_TRACK_CENTER (VLAX-ENAME->VLA-OBJECT DFCC_TRACK_CENTER))
;(SETQ IR_TRACK_CENTER (VLAX-ENAME->VLA-OBJECT IR_TRACK_CENTER))
(SETQ I 0 J 0 MAST NIL ROTATION NIL INSERTION_POINT NIL PROJECTION_POINT1 NIL PROJECTION_POINT2 NIL FINAL_INSERTION_POINT NIL)
;(SETQ PLINE_LIST (list '(0 0 0)))
(SETQ PLINE_LIST NIL)

(WHILE (< J (LENGTH ENT_LIST))
  (SETQ MAST (VLAX-ENAME->VLA-OBJECT (NTH J ENT_LIST)))
  ;(SETQ ROTATION (VLAX-GET-PROPERTY MAST 'ROTATION))
  (SETQ INSERTION_POINT (VLAX-SAFEARRAY->LIST (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY MAST 'INSERTIONPOINT))))
  (SETQ PROJECTION_POINT1 (VLAX-CURVE-GETCLOSESTPOINTTO DFCC_TRACK_CENTER INSERTION_POINT T))
  (SETQ PLINE_LIST (append PLINE_LIST (list PROJECTION_POINT1)))
  ;(SETQ TEMP_DIST (VLAX-CURVE-GETDISTATPOINT DFCC_TRACK_CENTER PROJECTION_POINT1))
   
   (SETQ J (+ J 1))
 )
 
 (command "PLINE" (foreach pt PLINE_LIST (command pt)))
 (SETQ IR_TRACK_CENTER (ENTLAST))
 (SETQ IR_TRACK_CENTER (VLAX-ENAME->VLA-OBJECT IR_TRACK_CENTER))
 
 
 

   





(WHILE (< I (LENGTH ENT_LIST))
  (SETQ MAST (VLAX-ENAME->VLA-OBJECT (NTH I ENT_LIST)))
  (SETQ ROTATION (VLAX-GET-PROPERTY MAST 'ROTATION))
  (SETQ INSERTION_POINT (VLAX-SAFEARRAY->LIST (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY MAST 'INSERTIONPOINT))))
  (SETQ PROJECTION_POINT1 (VLAX-CURVE-GETCLOSESTPOINTTO DFCC_TRACK_CENTER INSERTION_POINT T))
  ;(SETQ PLINE_LIST (append pline_list (list projection_point1)))
  (SETQ TEMP_DIST (VLAX-CURVE-GETDISTATPOINT DFCC_TRACK_CENTER PROJECTION_POINT1))
   
   (SETQ ENT_OBJECT MAST)
  (SETQ SAFEARRAY_SET NIL)
  ;(SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	    )
	  )
    )
  )
 )
  
  (SETQ DIST_ADD (ATOF (vlax-get-property (NTH 1 SAFEARRAY_SET) 'TEXTSTRING)))
   (SETQ FINAL_DIST (+ TEMP_DIST (/ DIST_ADD 2.00)))
   (SETQ PROJECTION_POINT1 (vlax-curve-getPointAtDist DFCC_TRACK_CENTER FINAL_DIST))
    
  
  (SETQ PROJECTION_POINT2 (VLAX-CURVE-GETCLOSESTPOINTTO IR_TRACK_CENTER PROJECTION_POINT1 T))
  (SETQ VERS (* 1000 (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2)))
  
   (COND  (( >= VERS 240) (SETQ VERS 240))
		  ((AND (< VERS 240) (>= VERS 230)) (SETQ VERS 240))
          ((AND (< VERS 230) (>= VERS 220)) (SETQ VERS 230))
		  ((AND (< VERS 220) (>= VERS 210)) (SETQ VERS 220))
		  ((AND (< VERS 210) (>= VERS 200)) (SETQ VERS 210))
		  ((AND (< VERS 200) (>= VERS 190)) (SETQ VERS 200))
		  ((AND (< VERS 190) (>= VERS 180)) (SETQ VERS 190))
		  ((AND (< VERS 180) (>= VERS 170)) (SETQ VERS 180))
		  ((AND (< VERS 170) (>= VERS 160)) (SETQ VERS 170))
		  ((AND (< VERS 160) (>= VERS 150)) (SETQ VERS 160))
		  ((AND (< VERS 150) (>= VERS 140)) (SETQ VERS 150))
		  ((AND (< VERS 140) (>= VERS 130)) (SETQ VERS 140))
		  ((AND (< VERS 130) (>= VERS 120)) (SETQ VERS 130))
		  ((AND (< VERS 120) (>= VERS 110)) (SETQ VERS 120))
		  ((AND (< VERS 110) (>= VERS 100)) (SETQ VERS 110))
		  ((AND (< VERS 100) (>= VERS 90)) (SETQ VERS 100))
		  ((AND (< VERS 90) (>= VERS 80)) (SETQ VERS 90))
		  ((AND (< VERS 80) (>= VERS 70)) (SETQ VERS 80))
		  ((AND (< VERS 70) (>= VERS 60)) (SETQ VERS 70))
		  ((AND (< VERS 60) (>= VERS 50)) (SETQ VERS 60))
		  ((AND (< VERS 50) (>= VERS 40)) (SETQ VERS 50))
		  ((AND (< VERS 40) (>= VERS 30)) (SETQ VERS 40))
		  ((AND (< VERS 30) (>= VERS 20)) (SETQ VERS 30))
		  ((AND (< VERS 20) (>= VERS 0)) (SETQ VERS 0))
		  
		  
	)
   
   (IF (/= VERS 0)
    (PROGN
   
		(IF (>= (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 16)
			(SETQ FINAL_INSERTION_POINT (POLAR PROJECTION_POINT1 (+ (* 7 (/ PI 4)) ROTATION) 2.0)) ;(/ (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 4)))
			(SETQ FINAL_INSERTION_POINT (POLAR PROJECTION_POINT1 (+ (* 7 (/ PI 4)) ROTATION) 2.0))
		)
		
		
			
		  (IF (<= (ANGLE PROJECTION_POINT1 PROJECTION_POINT2) 3.14)
		   (PROGN
			(COMMAND "INSERT" "VERSINE_LEFT" FINAL_INSERTION_POINT 1 1 (* 1 (/ 180 PI) ROTATION) )
			(MODIFY_ATTRIBUTES (ENTLAST) (LIST "VERSINE") (LIST (RTOS VERS 2 0)));;;;;(STRCAT "TC=" (RTOS (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 2 2))))
	       )
		   
		  (PROGN
		   (COMMAND "INSERT" "VERSINE" FINAL_INSERTION_POINT 1 1 (* 1 (/ 180 PI) ROTATION) )
		   (MODIFY_ATTRIBUTES (ENTLAST) (LIST "VERSINE") (LIST (RTOS VERS 2 0)));;;;;(STRCAT "TC=" (RTOS (DISTANCE2D PROJECTION_POINT1 PROJECTION_POINT2) 2 2))))
		   )
		 )
	
	
   )
   
 )
  
  (SETQ I (+ I 1))
)
)