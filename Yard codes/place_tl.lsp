(defun c:place_tl()

 (SETQ UPTRACK POLYLINE_DISTANCE_UPTRACK_V)
 (SETQ DNTRACK POLYLINE_DISTANCE_DNTRACK_V)
(setq wire_ent (car (entsel)))
(setq wire_ent (vlax-ename->vla-object wire_ent))
(setq st_point (vlax-curve-getStartPoint wire_ent)) 
(setq end_point (vlax-curve-getEndPoint wire_ent))


(command "zoom" "a")
(setq ent_list1 (form_ssset (ssget "_C" st_point (polar st_point (/ pi 2) 0.002) (list (CONS 0 "INSERT"))))) 
(setq ent_list2 (form_ssset (ssget "_C" end_point end_point (list (CONS 0 "INSERT"))))) 
(command "zoom" "p")




(setq ent_list_name1 (EXTRACT_ENTITY_INFO2 ent_list1)) 
(setq ent_list_final1 (single_ele_list (filter_list (list "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "PORTAL") ent_list_name1 1) 0))
(setq ent_list_map1 (BUILD_LIST (ENTITY_DFX  ent_list_final1 10 -1) (LIST 0 1)))
(setq ent_list_map1 (car (map_nearest11 ent_list_map1 st_point 1)))
(setq eff_name (effective_name ent_list_final1) )






(if (= eff_name "PORTAL")
(PROGN 
(SETQ loc1 (NTH 1 (ASSOC "MAST_NUMBER1" (GET_ATTRIBUTES ent_list_final1))))
(SETQ loc2 (NTH 1 (ASSOC "MAST_NUMBER2" (GET_ATTRIBUTES ent_list_final1))))
(setq chain1 (NTH 1 (ASSOC "CHAINAGE1" (GET_ATTRIBUTES ent_list_final1))))
)

(PROGN 
(SETQ loc1 (NTH 1 (ASSOC "MAST_NUMBER1" (GET_ATTRIBUTES ent_list_final1))))
;(SETQ loc2 (NTH 1 (ASSOC "MAST_NUMBER1" (GET_ATTRIBUTES ent_list_final1))))
(setq chain1 (NTH 1 (ASSOC "CHAINAGE1" (GET_ATTRIBUTES ent_list_final1))))
)

)

(setq ent_list_build (LIST (LIST ent_list_final1 st_point))) ;(BUILD_LIST (ENTITY_DFX (list ent_list_final1) 10 -1) (LIST 0 1)))
  (SETQ MAIN_BLOCK_INSERTION_POINT (CDR (ASSOC 10 (ENTGET ent_list_final1))))
  (SETQ MAST_ROTATION (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ent_list_final1) 'ROTATION))
  (SETQ MAST_ROTATION1 (* (/ 180 PI) (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ent_list_final1) 'ROTATION)))

  (setq track_type (cadr (car (anchor_up_dn ent_list_build UPTRACK DNTRACK))))
  (if (= track_type "UP")
        (progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 3 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (CHANGE_FLIP (ENTLAST) "TLF2")
		(CHANGE_FLIP (ENTLAST) "TLF1")
		)
		(progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 1 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (CHANGE_FLIP (ENTLAST) "TLF2")
		)
		
  )
 (setq tl1 (entlast))
(setq entlist1 NIL ent_list_name1 NIL ent_list_final1 NIL ent_list_build NIL)

  
;(setq ent_list1 (form_ssset (ssget "C" end_point end_point (list (CONS 0 "INSERT"))))) 
(setq ent_list_name1 (EXTRACT_ENTITY_INFO2 ent_list2)) 
(setq ent_list_final1 (car (single_ele_list (filter_list (list "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "PORTAL") ent_list_name1 1) 0)))
(setq eff_name (effective_name ent_list_final1) )

(if  (= eff_name "PORTAL")
(PROGN 
(SETQ loc11 (NTH 1 (ASSOC "MAST_NUMBER1" (GET_ATTRIBUTES ent_list_final1))))
(SETQ loc22 (NTH 1 (ASSOC "MAST_NUMBER2" (GET_ATTRIBUTES ent_list_final1))))
(setq chain11 (NTH 1 (ASSOC "CHAINAGE1" (GET_ATTRIBUTES ent_list_final1))))
)

(PROGN 
(SETQ loc11 (NTH 1 (ASSOC "MAST_NUMBER1" (GET_ATTRIBUTES ent_list_final1))))
;(SETQ loc2 (NTH 1 (ASSOC "MAST_NUMBER1" (GET_ATTRIBUTES ent_list_final1))))
(setq chain11 (NTH 1 (ASSOC "CHAINAGE1" (GET_ATTRIBUTES ent_list_final1))))
)

)

(setq ent_list_build (list (list ent_list_final1 end_point)));(BUILD_LIST (ENTITY_DFX (list ent_list_final1) 10 -1) (LIST 0 1)))
  (SETQ MAIN_BLOCK_INSERTION_POINT (CDR (ASSOC 10 (ENTGET ent_list_final1))))
  (SETQ MAST_ROTATION (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ent_list_final1) 'ROTATION))
  (SETQ MAST_ROTATION1 (* (/ 180 PI) (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ent_list_final1) 'ROTATION)))

  (setq track_type (cadr (car (anchor_up_dn ent_list_build UPTRACK DNTRACK))))
  (if (= track_type "UP")
        (progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 3 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (CHANGE_FLIP (ENTLAST) "TLF1")
		)
		(progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 1 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        ;(CHANGE_FLIP (ENTLAST) "TLF2")
		)
		
  )
    
	(setq tl2 (entlast))
	
	
    (SETQ TEMP_DFCC_CHAINAGE chain1)
	(SETQ TEMP_DFCC_KM_CHAINAGE (SUBSTR TEMP_DFCC_CHAINAGE 1 (VL-STRING-SEARCH "/" TEMP_DFCC_CHAINAGE)))
	(SETQ TEMP_DFCC_ACTUAL_CHAINAGE (SUBSTR TEMP_DFCC_CHAINAGE (+ 2 (VL-STRING-SEARCH "/" TEMP_DFCC_CHAINAGE) )))
	(SETQ DISTANCE1 (+ (* (ATOF TEMP_DFCC_KM_CHAINAGE) 1000.00) (ATOF TEMP_DFCC_ACTUAL_CHAINAGE)))
     
	(SETQ TEMP_DFCC_CHAINAGE chain11)
	(SETQ TEMP_DFCC_KM_CHAINAGE (SUBSTR TEMP_DFCC_CHAINAGE 1 (VL-STRING-SEARCH "/" TEMP_DFCC_CHAINAGE)))
	(SETQ TEMP_DFCC_ACTUAL_CHAINAGE (SUBSTR TEMP_DFCC_CHAINAGE (+ 2 (VL-STRING-SEARCH "/" TEMP_DFCC_CHAINAGE) )))
	(SETQ DISTANCE2 (+ (* (ATOF TEMP_DFCC_KM_CHAINAGE) 1000.00) (ATOF TEMP_DFCC_ACTUAL_CHAINAGE)))
  
  
  
  

)


(defun c:get_Yard_track()

(SETQ POLYLINE_DISTANCE_UPTRACK_V (CAR (ENTSEL "\n SELECT UPTRACK:")))
(SETQ POLYLINE_DISTANCE_DNTRACK_V (CAR (ENTSEL "\n SELECT DNTRACK:")))




)


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

 
  
(defun c:Place_Tl_New( / WPT1 WPT2 WPT3 WPT4 BLOCKS_LIST UPTRACK DNTRACK TOTAL_LIST_ANCHOR ANCHOR_LIST_TOTAL ANCHOR_LIST_BWA ANCHOR_LIST I visited_list
temp_ele temp_list pair_list temp_anc temp_mast TRACK_TYPE FLIP1 MAIN_BLOCK_INSERTION_POINT MAST_ROTATION MAST_ROTATION1 temp_anc_other MAST_NUMBER_ANC MAST_CHAIN_ANC 
MAST_NUMBER_ANC_OTHER MAST_CHAIN_ANC_OTHER TEMP_DFCC_KM_CHAINAGE TEMP_DFCC_ACTUAL_CHAINAGE TL_DIST )
(SETQ WPT1 (GETPOINT "\n PICK POINT1"))
(SETQ WPT2 (GETPOINT "\n PICK POINT2"))
(SETQ WPT3 (GETPOINT "\n PICK POINT3"))
(SETQ WPT4 (GETPOINT "\n PICK POINT4"))
(SETQ BLOCKS_LIST (FORM_SSSET (SSGET "WP" (LIST WPT1 WPT2 WPT3 WPT4) (LIST (CONS 0 "INSERT")))))
(SETQ UPTRACK (CAR (ENTSEL "\n SELECT UPTRACK:")))
(SETQ DNTRACK (CAR (ENTSEL "\n SELECT DNTRACK:")))
(SETQ TOTAL_LIST_ANCHOR (ANCHOR_STRUCTURE_MAPPING_GPS_204))

(SETQ ANCHOR_LIST_TOTAL (YARD_DATA_COLLECT_GPS_204 BLOCKS_LIST '("ANCHOR")  1 "ANCHOR_TYPE1"))
(SETQ ANCHOR_LIST_BWA (SINGLE_ELE_LIST (FILTER_LIST (LIST "BWA" "FTA" "BWA(DMA)" "FTA(DMA)") ANCHOR_LIST_TOTAL 1) 0))

(SETQ ANCHOR_LIST (YARD_DATA_COLLECT_GPS_204 ANCHOR_LIST_BWA '("ANCHOR")  1 "ACTUAL_WIRE_RUN1"))

(SETQ I 0)
(setq visited_list NIL)
(while (< i (length anchor_list))
(SETQ temp_ele (nth 1 (nth i anchor_list)))
(setq temp_list (filter_list (list temp_ele) anchor_list 1))

(if (AND (= (member (car temp_list) visited_list) NIL) (> (LENGTH TEMP_LIST) 1))
(progn
(setq pair_list (filter_list (list (car (car temp_list)) (car (cadr temp_list))) TOTAL_LIST_ANCHOR 1))

(setq temp_anc (nth 1 (nth 0 pair_list)))
;(SETQ temp_anc_other (nth 1 (nth 1 pair_list)))

(setq temp_mast (nth 0 (nth 0 pair_list)))
(SETQ TRACK_TYPE (CADR (CAR (anchor_up_dn (BUILD_LIST (ENTITY_DFX (list TEMP_ANC) 10 -1) (LIST 0 1)) UPTRACK DNTRACK))))
(SETQ FLIP1 (CADR (CAR (GET_DYNAMIC_PROPERTIES TEMP_ANC (LIST "ANF1")))))

;(SETQ MAST_NUMBER_TEMP (CADR (assoc "ANCHOR_NATURE1" (get_attributes temp_anc_other))))
;(SETQ MAST_CHAIN_TEMP (CADR (assoc "ANCHOR_FOUNDATION1" (get_attributes temp_anc_other))))

  (SETQ MAIN_BLOCK_INSERTION_POINT (CDR (ASSOC 10 (ENTGET temp_anc))))
  (SETQ MAST_ROTATION (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT temp_mast) 'ROTATION))
  (SETQ MAST_ROTATION1 (* (/ 180 PI) (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT temp_mast) 'ROTATION)))

(if (= track_type "UP")
        (progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 3 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (if (= flip1 0)
		(CHANGE_FLIP (ENTLAST) "TLF2"))
		(CHANGE_FLIP (ENTLAST) "TLF1")
		)
		(progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 1 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (if (= flip1 0)
		(CHANGE_FLIP (ENTLAST) "TLF2"))
		
		
		)
		
  ) (SETQ TL1 (ENTLAST))
  
;;;;;;;;;;;;;;;;;;;;;;OTHER END TENSION LENGTH;;;;;;;;;;;;;;
  
(setq temp_anc (nth 1 (nth 1 pair_list)))
;(SETQ temp_anc_other (nth 1 (nth 1 pair_list)))

(setq temp_mast (nth 0 (nth 0 pair_list)))
(SETQ TRACK_TYPE (CADR (CAR (anchor_up_dn (BUILD_LIST (ENTITY_DFX (list TEMP_ANC) 10 -1) (LIST 0 1)) UPTRACK DNTRACK))))
(SETQ FLIP1 (CADR (CAR (GET_DYNAMIC_PROPERTIES TEMP_ANC (LIST "ANF1")))))

;(SETQ MAST_NUMBER_TEMP (CADR (assoc "ANCHOR_NATURE1" (get_attributes temp_anc_other))))
;(SETQ MAST_CHAIN_TEMP (CADR (assoc "ANCHOR_FOUNDATION1" (get_attributes temp_anc_other))))

  (SETQ MAIN_BLOCK_INSERTION_POINT (CDR (ASSOC 10 (ENTGET temp_anc))))
  (SETQ MAST_ROTATION (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT temp_mast) 'ROTATION))
  (SETQ MAST_ROTATION1 (* (/ 180 PI) (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT temp_mast) 'ROTATION)))

(if (= track_type "UP")
        (progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 3 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (if (= flip1 0)
		(CHANGE_FLIP (ENTLAST) "TLF2"))
		(CHANGE_FLIP (ENTLAST) "TLF1")
		)
		(progn
		(COMMAND "INSERT" "TENSION_LENGTH" (POLAR MAIN_BLOCK_INSERTION_POINT (+ (* 1 (/ PI 2)) MAST_ROTATION) 35) "1" MAST_ROTATION1)
        (if (= flip1 0)
		(CHANGE_FLIP (ENTLAST) "TLF2"))
		
		
		)
		
  ) (SETQ TL2 (ENTLAST))

  
(setq temp_anc (nth 1 (nth 0 pair_list)))
(SETQ temp_anc_other (nth 1 (nth 1 pair_list)))

(SETQ MAST_NUMBER_ANC (CADR (assoc "ANCHOR_NATURE1" (get_attributes temp_anc_other))))
(SETQ MAST_CHAIN_ANC (CADR (assoc "ANCHOR_FOUNDATION1" (get_attributes temp_anc_other))))

(SETQ MAST_NUMBER_ANC_OTHER (CADR (assoc "ANCHOR_NATURE1" (get_attributes temp_anc))))
(SETQ MAST_CHAIN_ANC_OTHER (CADR (assoc "ANCHOR_FOUNDATION1" (get_attributes temp_anc))))


 
  (SETQ TEMP_DFCC_KM_CHAINAGE (ATOF (SUBSTR MAST_CHAIN_ANC 1 (VL-STRING-SEARCH "/" MAST_CHAIN_ANC))))
  (SETQ TEMP_DFCC_ACTUAL_CHAINAGE (ATOF (SUBSTR MAST_CHAIN_ANC (+ 2 (VL-STRING-SEARCH "/" MAST_CHAIN_ANC) ))))
  (SETQ DIST1 (+ (* TEMP_DFCC_KM_CHAINAGE 1000.00) TEMP_DFCC_ACTUAL_CHAINAGE))
  
  (SETQ TEMP_DFCC_KM_CHAINAGE (ATOF (SUBSTR MAST_CHAIN_ANC_OTHER 1 (VL-STRING-SEARCH "/" MAST_CHAIN_ANC_OTHER))))
  (SETQ TEMP_DFCC_ACTUAL_CHAINAGE (ATOF (SUBSTR MAST_CHAIN_ANC_OTHER (+ 2 (VL-STRING-SEARCH "/" MAST_CHAIN_ANC_OTHER) ))))
  (SETQ DIST2 (+ (* TEMP_DFCC_KM_CHAINAGE 1000.00) TEMP_DFCC_ACTUAL_CHAINAGE))
  
   (SETQ TL_DIST (RTOS (ABS (- DIST1 DIST2)) 2 2))
   (MODIFY_ATTRIBUTES TL1 (LIST "TENSION_LENGTH" "LOCATION_NUM" "CHAINAGE") 
                        (LIST TL_DIST MAST_NUMBER_ANC MAST_CHAIN_ANC))
						
   (MODIFY_ATTRIBUTES TL2 (LIST "TENSION_LENGTH" "LOCATION_NUM" "CHAINAGE") 
                        (LIST TL_DIST MAST_NUMBER_ANC_OTHER MAST_CHAIN_ANC_OTHER))
  (setq visited_list (append temp_list visited_list))
  
))
(setq i (+ i 1))
;(setq visited_list (append temp_list visited_list))
)


)
  
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
(defun change_color(ent / list1 )

 (setq list1 (entget ent))
 (setq list1 (append list1 (list (cons 62 3))))
 (entmod list1)
 (entupd ent)
 )
 
 
(DEFUN MAP_NEAREST11 (LIST1 PT K / I TEMP_SET LIST2)
  (SETQ I 0)
  (SETQ TEMP_SET NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (IF	(NOT (ATOM (NTH K (NTH I LIST1))))
      (PROGN (SETQ TEMP_SET
		    (LIST
		      (ABS (DISTANCE (NTH K (NTH I LIST1)) PT))
		      (NTH I LIST1)
		    )
	     )
	     (SETQ LIST2 (CONS TEMP_SET LIST2))
      )
      (PROGN (SETQ TEMP_SET (LIST (ABS (DISTANCE (NTH I LIST1) PT))
				  (NTH I LIST1)
			    )
	     )
	     (SETQ LIST2 (CONS TEMP_SET LIST2))
      )
    )
    (SETQ I (+ I 1))
  )
  ;(NTH 1 (NTH 0 (SORT_FUN LIST2 0 0)))
  (SORT_FUN LIST2 0 0)
)