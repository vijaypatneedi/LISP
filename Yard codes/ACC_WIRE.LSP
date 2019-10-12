
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN C:UPDATE_ACC_WIRE_CP204( / WPT1 WPT2 WPT3 WPT4 UPTRACK DNTRACK BLOCKS_LIST BLOCK_NAME_LIST ACC_LIST ACC_LIST_POINT ACC_LIST_UP_DN ACC_LIST_UP ACC_LIST_DN ACC_LIST_UP_DIST ACC_LIST_DN_DIST 
            ANCHOR_ENTITIES ANCHOR_ENTITIES_POINT ANCHOR_ENTITIES_UP_DN ANCHOR_ENTITIES_UP ANCHOR_ENTITIES_UP_DIST ANCHOR_ENTITIES_DN_DIST ACC_ANCHOR_DN ACC_ANCHOR_UP M ENT1 ENT_NAME WIRE_NAME)
  
  (PROMPT "\n PLEASE RUN TRACK FIRST!!!!!!!!")
  (SETQ WPT1 (GETPOINT "\n ENTER 1ST POINT OF POLYGON"))
  (SETQ WPT2 (GETPOINT "\n ENTER 2ND POINT OF POLYGON"))
  (SETQ WPT3 (GETPOINT "\n ENTER 3RD POINT OF POLYGON"))
  (SETQ WPT4 (GETPOINT "\n ENTER 4TH POINT OF POLYGON"))
  
  (SETQ BLOCKS_LIST (FORM_SSSET (SSGET "WP" (LIST WPT1 WPT2 WPT3 WPT4) (LIST (CONS 0 "INSERT")))))
  
  (SETQ UPTRACK (CAR (ENTSEL "\n PLEASE RUN TRACK FIRST!!!!!!!!& THEN SELECT UPTRACK:")))
  (SETQ DNTRACK (CAR (ENTSEL "\n SELECT DNTRACK:")))
  
  
  
   ;(SETQ BLOCKS_LIST (FORM_SSSET (SSGET "X" (LIST (CONS 0 "INSERT")))))
  (SETQ BLOCK_NAME_LIST (EXTRACT_ENTITY_INFO2 BLOCKS_LIST))
  
  (SETQ ACC_LIST (FILTER_LIST (LIST "ACC") BLOCK_NAME_LIST 1))
  (SETQ ACC_LIST_POINT (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST ACC_LIST 0) 10 -1) (LIST 0 1)))
  (SETQ ACC_LIST_UP_DN (ACC_UP_DN ACC_LIST_POINT))
  (SETQ ACC_LIST_UP (FILTER_LIST (LIST "UP") ACC_LIST_UP_DN 1))
  (SETQ ACC_LIST_DN (FILTER_LIST (LIST "DN") ACC_LIST_UP_DN 1))
  
  (SETQ ACC_LIST_UP_DIST (SORT_FUN (DISTANCE_LINE (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST ACC_LIST_UP 0) 10 -1) (LIST 0 1))) 1 1))
  (SETQ ACC_LIST_DN_DIST (SORT_FUN (DISTANCE_LINE (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST ACC_LIST_DN 0) 10 -1) (LIST 0 1))) 1 1))
  
  
  

  (SETQ ANCHOR_ENTITIES (YARD_DATA_COLLECT_GPS_204 BLOCKS_LIST '("ANCHOR") 1 "ANCHOR_TYPE1"))
   (SETQ ANCHOR_ENTITIES (FILTER_LIST (LIST "BWA") ANCHOR_ENTITIES 1))
   (SETQ ANCHOR_ENTITIES_POINT  (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST ANCHOR_ENTITIES 0)  10 -1) (LIST 0 1)))
   (SETQ ANCHOR_ENTITIES_UP_DN (ANCHOR_UP_DN ANCHOR_ENTITIES_POINT UPTRACK DNTRACK))
   (SETQ ANCHOR_ENTITIES_DN (FILTER_LIST (LIST "DN") ANCHOR_ENTITIES_UP_DN 1))
   (SETQ ANCHOR_ENTITIES_UP (FILTER_LIST (LIST "UP") ANCHOR_ENTITIES_UP_DN 1))
   
   (SETQ ANCHOR_ENTITIES_UP_DIST (SORT_FUN (DISTANCE_LINE (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST ANCHOR_ENTITIES_UP 0) 10 -1) (LIST 0 1))) 1 1))
   (SETQ ANCHOR_ENTITIES_DN_DIST (SORT_FUN (DISTANCE_LINE (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST ANCHOR_ENTITIES_DN 0) 10 -1) (LIST 0 1))) 1 1))
   
   
   (SETQ ACC_ANCHOR_DN (SORT_FUN (APPEND ACC_LIST_DN_DIST ANCHOR_ENTITIES_DN_DIST) 1 1))
   (SETQ ACC_ANCHOR_UP (SORT_FUN (APPEND ACC_LIST_UP_DIST ANCHOR_ENTITIES_UP_DIST) 1 1))
   ; (SETQ ANCHOR_ENTITIES_DIST (SORT_FUN (DISTANCE_LINE ANCHOR_ENTITIES_POINT) 1 1))
   (SETQ M 0)
   (WHILE (< M (LENGTH ACC_ANCHOR_DN))
     (SETQ ENT1 (NTH 0 (NTH M ACC_ANCHOR_DN)))
	 (SETQ ENT_NAME (CADR (CAR (EXTRACT_ENTITY_INFO2 (LIST ENT1) ))))
	 (IF (= ENT_NAME "ACC")
	 (IF (>= M 2)
	 (PROGN
	 (SETQ WIRE_NAME (NTH 1 (ASSOC "ACTUAL_WIRE_RUN1" (GET_ATTRIBUTES (NTH 0 (NTH (- M 2)ACC_ANCHOR_DN))))))
	 (MODIFY_ATTRIBUTES ENT1 (LIST "WIRE_RUN1" "WIRE_RUN2") (LIST WIRE_NAME WIRE_NAME))
	 )
	 (PROGN 
	 (SETQ WIRE_NAME (NTH 1 (ASSOC "ACTUAL_WIRE_RUN1" (GET_ATTRIBUTES (NTH 0 (NTH (+ M 2)ACC_ANCHOR_DN))))))
	 (MODIFY_ATTRIBUTES ENT1 (LIST "WIRE_RUN1" "WIRE_RUN2") (LIST WIRE_NAME WIRE_NAME))
	 )
	 )
	 
	 )
	 
	 (SETQ M (+ M 1))
	 )
	 
	 (SETQ M 0)
	 (WHILE (< M (LENGTH ACC_ANCHOR_UP))
     (SETQ ENT1 (NTH 0 (NTH M ACC_ANCHOR_UP)))
	 (SETQ ENT_NAME (CADR (CAR (EXTRACT_ENTITY_INFO2 (LIST ENT1) ))))
	 (IF (= ENT_NAME "ACC")
	 (IF (>= M 2)
	 (PROGN
	 (SETQ WIRE_NAME (NTH 1 (ASSOC "ACTUAL_WIRE_RUN1" (GET_ATTRIBUTES (NTH 0 (NTH (- M 2)ACC_ANCHOR_UP))))))
	 (MODIFY_ATTRIBUTES ENT1 (LIST "WIRE_RUN1" "WIRE_RUN2") (LIST WIRE_NAME WIRE_NAME))
	 )
	 
	 (PROGN 
	 (SETQ WIRE_NAME (NTH 1 (ASSOC "ACTUAL_WIRE_RUN1" (GET_ATTRIBUTES (NTH 0 (NTH (+ M 2)ACC_ANCHOR_DN))))))
	 (MODIFY_ATTRIBUTES ENT1 (LIST "WIRE_RUN1" "WIRE_RUN2") (LIST WIRE_NAME WIRE_NAME))
	 )
	 )
	 )
	 
	 (SETQ M (+ M 1))
	 )
	 
	 
   
   )

   
   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (DEFUN YARD_DATA_COLLECT_GPS_204 (ENTITY_LIST BLOCKNAME_LIST J ATTRIBUTE / LIST1 I TEMP_ELE TEMP_LIST LIST2)

  (SETQ LIST1 (SINGLE_ELE_LIST (FILTER_LIST BLOCKNAME_LIST (DATA_COLLECT_GPS ENTITY_LIST BLOCKNAME_LIST 2 -1)  0) 1))
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ TEMP_LIST NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_LIST (YARD_STRUCTURE_INFO_GPS (NTH I LIST1)))
	(SETQ A (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO_GPS (NTH I LIST1)))))
  (SETQ TEMP_ELE (LIST (NTH 1 (ASSOC J  TEMP_LIST)) (NTH 1 (ASSOC ATTRIBUTE A))))
  (SETQ LIST2 (CONS TEMP_ELE LIST2))
    (SETQ I (+ I 1))
    )
  (REVERSE LIST2)
)
   
   
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN GET_DYNAMIC_PROPERTIES (ENTNAME IDENTIFIER  / TEMP_ELE ENT_OBJECT SAFEARRAY_SET  I J LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT)) 
(IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETDYNAMICBLOCKPROPERTIES")
	   )
	 )
  )

(SETQ I 0)
  (SETQ J 0)
  (SETQ LIST1 NIL)
(WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ TEMP_ELE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "PROPERTYNAME"))
    (IF (/= (VL-POSITION TEMP_ELE IDENTIFIER) NIL)
      (PROGN (SETQ LIST1 (CONS (LIST TEMP_ELE (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "VALUE"))) LIST1))))
    (SETQ I (+ I 1))
  )
))(REVERSE LIST1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(DEFUN DISTANCE_LINE (ENT_LIST / FINAL_LIST I TEMP_DISTANCE1 P1 ent1)
(setq ent1 (vlax-ename->vla-object POLYLINE_ENTITY_FOR_DISTANCE_V))
(SETQ I 0 FINAL_LIST NIL)
(WHILE (< I (LENGTH ENT_LIST))
  (SETQ P1 (NTH 1 (NTH I ENT_LIST)))
  (SETQ TEMP_DISTANCE1 (VLAX-CURVE-GETDISTATPOINT ent1 (vlax-curve-getclosestpointto ent1 P1)))
  (SETQ FINAL_LIST (CONS (LIST (NTH 0 (NTH I ENT_LIST)) TEMP_DISTANCE1) FINAL_LIST))
  (SETQ I (+ 1 I))

)
FINAL_LIST
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN ANCHOR_UP_DN (ENT_LIST UPTRACK DNTRACK / FINAL_LIST I ent1 ent2 P1 P2 PROJECTION_UPTRACK PROJECTION_DNTRACK DIST_UPTRACK DIST_DNTRACK LINE_TYPE)
   (setq ent1 (vlax-ename->vla-object UPTRACK))
    (setq ent2 (vlax-ename->vla-object DNTRACK))
	(SETQ I 0 FINAL_LIST NIL)
   (WHILE (< I (LENGTH ENT_LIST))
   (SETQ P2 (NTH 1 (NTH I ENT_LIST)))
   (SETQ PROJECTION_UPTRACK (vlax-curve-getclosestpointto ent1 P2))
   (SETQ PROJECTION_DNTRACK (vlax-curve-getclosestpointto ent2 P2))
   (SETQ DIST_UPTRACK (DISTANCE2D P2 PROJECTION_UPTRACK))
   (SETQ DIST_DNTRACK (DISTANCE2D P2 PROJECTION_DNTRACK))
   (IF (< DIST_UPTRACK DIST_DNTRACK)
      (SETQ LINE_TYPE "DN")
	  (SETQ LINE_TYPE "UP")
	  )
	  (SETQ FINAL_LIST (CONS (LIST (NTH 0 (NTH I ENT_LIST)) LINE_TYPE) FINAL_LIST))
	  (SETQ I (+ I 1))
    )
 FINAL_LIST
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN ACC_UP_DN (ENT_LIST / FINAL_LIST I DIRECTION_TYPE FLIP_1)
(SETQ I 0 FINAL_LIST NIL)
(SETQ DIRECTION_TYPE NIL)
(WHILE (< I (LENGTH ENT_LIST))
 (SETQ FLIP_1 (CADR (ASSOC "ACF1" (GET_DYNAMIC_PROPERTIES (NTH 0 (NTH I ENT_LIST)) (LIST "ACF1")))))
 (IF (= FLIP_1 0)
   (SETQ DIRECTION_TYPE "DN")
   (SETQ DIRECTION_TYPE "UP")
   )
   (SETQ FINAL_LIST (CONS (LIST (NTH 0 (NTH I ENT_LIST)) DIRECTION_TYPE) FINAL_LIST))


(SETQ I (+ I 1))
)

FINAL_LIST
)

