
                                                                    ;YARD STRUCTURE INFO PROGRAMS;
(DEFUN ADD_LISTS1 (LIST1 LIST2 / I TEMP_ELE LIST3)
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (LIST (NTH I LIST1) (NTH I LIST2)))
    (SETQ LIST3 (CONS TEMP_ELE LIST3))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST3)
)
;;;;;;;;;
(DEFUN SUB_ENT_DATA_COLLECT (BLOCKENAME FILTER_LIST DFX1 DFX2 / BS_PT ename SUB_ENT_LIST I TEMP_ELE LIST2 J FLAG LIST2)
  (SETQ BS_PT (CDR (ASSOC 10 (ENTGET BLOCKENAME))))
  (if (setq ename (tblobjname "BLOCK" (CDR (ASSOC 2 (ENTGET BLOCKENAME)))))
    (reverse
      (while (setq ename (entnext ename))
	(setq SUB_ENT_LIST (cons ename SUB_ENT_LIST))
      )
    )
  )


(SETQ I 0)
(SETQ TEMP_ELE NIL)
(SETQ LIST2 NIL)
(WHILE (< I (LENGTH SUB_ENT_LIST))
  (SETQ TEMP_ELE (ENTGET (NTH I SUB_ENT_LIST)))
  (SETQ J 0)
  (SETQ FLAG 1)
  (WHILE (< J (LENGTH FILTER_LIST))
    (IF	(= (VL-POSITION (NTH J FILTER_LIST) TEMP_ELE) NIL)
      (PROGN (SETQ J (LENGTH FILTER_LIST)) (SETQ FLAG 0))
    )
    (SETQ J (+ J 1))
  )
  (IF (= FLAG 1)
    (SETQ LIST2	(CONS (SUM_LIST1 BS_PT (LIST (CDR (ASSOC DFX1 TEMP_ELE))
			    (CDR (ASSOC DFX2 TEMP_ELE))
		      ))
		      LIST2
		)
    )
  )
  (SETQ I (+ I 1))
)(REVERSE LIST2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SUB_ENT_DATA_COLLECT1 (BLOCKENAME FILTER_LIST DFX1 DFX2 / BS_PT ename SUB_ENT_LIST I TEMP_ELE LIST2 J FLAG LIST2)
  (SETQ BS_PT (CDR (ASSOC 10 (ENTGET BLOCKENAME))))
  (if (setq ename (tblobjname "BLOCK" (CDR (ASSOC 2 (ENTGET BLOCKENAME)))))
    (reverse
      (while (setq ename (entnext ename))
	(setq SUB_ENT_LIST (cons ename SUB_ENT_LIST))
      )
    )
  )


(SETQ I 0)
(SETQ TEMP_ELE NIL)
(SETQ LIST2 NIL)
(WHILE (< I (LENGTH SUB_ENT_LIST))
  (SETQ TEMP_ELE (ENTGET (NTH I SUB_ENT_LIST)))
  (SETQ J 0)
  (SETQ FLAG 1)
  (WHILE (< J (LENGTH FILTER_LIST))
    (IF	(= (VL-POSITION (NTH J FILTER_LIST) TEMP_ELE) NIL)
      (PROGN (SETQ J (LENGTH FILTER_LIST)) (SETQ FLAG 0))
    )
    (SETQ J (+ J 1))
  )
  (IF (= FLAG 1)
    (SETQ LIST2	(CONS  (LIST (CDR (ASSOC DFX1 TEMP_ELE))
			    (CDR (ASSOC DFX2 TEMP_ELE))
		      )
		      LIST2
		)
    )
  )
  (SETQ I (+ I 1))
)(REVERSE LIST2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;list1---> (effective_name portal_entity starting_point ending_point)
;list2---> (effective_names of cantilevers to be filtered)

(DEFUN MAP_CANTILEVER (LIST1 LIST2	/	  I	    LIST3     SS_SET
	      TEMP_ELE	PTLIST	  J	    ENAME     BS_POINT
	      EFF_NAME	TEMP_ELE1
	     )

  (SETQ I 0)
  (SETQ J 0)
  (SETQ SS_SET NIL)
  (SETQ TEMP_ELE NIL)
  (SETQ TEMP_ELE1 NIL)
  (SETQ ENAME NIL)
  (SETQ EFF_NAME NIL)
  (SETQ BS_POINT NIL)
  (SETQ LIST1 (SORT_FUN LIST1 2 0))
  (WHILE (< I 0)
    (SETQ TEMP_ELE (NTH I LIST1))
    (SETQ
      PTLIST (LIST (NTH 1 TEMP_ELE) (NTH 2 TEMP_ELE))
    )
    (SETQ SS_SET (SSGET "F" PTLIST '((0 . "INSERT"))))
   (SETQ LIST3 NIL)
    (WHILE (< J (SSLENGTH SS_SET))
      (SETQ ENAME (SSNAME SS_SET J))
      (SETQ BS_POINT (CDR (ASSOC 10 (ENTGET ENAME))))
      (SETQ ENAME (VLAX-ENAME->VLA-OBJECT ENAME))
      (SETQ EFF_NAME (VLAX-GET-PROPERTY ENAME "EFFECTIVENAME"))
      (SETQ TEMP_ELE1 (LIST ENAME EFF_NAME BS_POINT))
      
      
      (SETQ LIST3 (CONS TEMP_ELE1 LIST3))
      (SETQ J (+ J 1))
    )(SETQ LIST3 (FILTER_LIST LIST2 LIST3 1))(SETQ LIST3 (SORT_FUN LIST3 2 1))
    (SETQ LIST3 (CONS TEMP_ELE LIST3))
    (SETQ I (+ I 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN GET_ATTRIBUTES	(ENTNAME / ENT_OBJECT SAFEARRAY_SET I LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

  (SETQ I 0)
  (SETQ LIST1 NIL)
  (WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ
      LIST1 (CONS
	      (LIST
		(VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING")
		(VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TEXTSTRING")
		(VLAX-SAFEARRAY->LIST
		  (VLAX-VARIANT-VALUE
		    (VLAX-GET-PROPERTY
		      (NTH I SAFEARRAY_SET)
		      "InsertionPoint"
		    )
		  )
		)
	        (IF ( = (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) 'VISIBLE) :VLAX-TRUE ) 1 0)
	      )
	      LIST1
	    )
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST1 (REVERSE LIST1))
  (SETQ LIST1 (SORT_FUN LIST1 0 0)))
    (SETQ LIST1 NIL)
    )LIST1
  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (DEFUN LIST_FORM (LIST1 / I TEMP_LIST)
    (SETQ TEMP_LIST NIL)
    (SETQ I 0)
    (WHILE (< I (LENGTH LIST1))
      (SETQ TEMP_LIST (APPEND (REVERSE (NTH I LIST1)) TEMP_LIST))
      (SETQ I (+ I 1))
    )
    (SETQ TEMP_LIST (REVERSE TEMP_LIST))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN YARD_STRUCTURE_INFO (ENT_NAME	 /	      ENTINFO
			    TEMP_ELE1	 TEMP_ELE2    TEMP_ELE3
			    TEMP_ELE4	 FINAL_LIST   EFFNAME
			   )
  (SETQ	TEMP_ELE1 NIL
	TEMP_ELE2 NIL
	TEMP_ELE3 NIL
	TEMP_ELE4 NIL
	TEMP_ELE5 NIL
	TEMP_ELE6 NIL
  )
  (SETQ ENTINFO (ENTGET ENT_NAME))
  (SETQ TEMP_ELE1 (LIST 1 ENT_NAME))
  (SETQ	TEMP_ELE2 (LIST	2
			(VLAX-GET-PROPERTY
			  (VLAX-ENAME->VLA-OBJECT ENT_NAME)
			  "EFFECTIVENAME"
			)
		  )
  )
  (SETQ FLIPLIST (GET_FLIPSTATES ENT_NAME (NTH 1 TEMP_ELE2)))
  (SETQ TEMP_ELE3 (LIST 3 (CDR (ASSOC 10 ENTINFO))))
  (SETQ	TEMP_ELE4 (LIST	4
			(MERGE_FLIPSTATES
			  (GET_ATTRIBUTES ENT_NAME)
			  (NTH 1 FLIPLIST)
			)
		  )
  )

  (SETQ TEMP_ELE5 (LIST 5 (NTH 0 FLIPLIST)))
  (IF(OR (OR (= (NTH 1 TEMP_ELE2) "PORTAL")
	     (= (NTH 1 TEMP_ELE2) "TTC")
	 )
	 (= (NTH 1 TEMP_ELE2) "SS4")
     )

     
    (SETQ TEMP_ELE6
	   (LIST 6
		 (GET_CANTILEVERS
		   (NTH 1 TEMP_ELE3)
		   (NTH 1 TEMP_ELE2)
		   ENT_NAME
		 )
	   )
    )
    (SETQ TEMP_ELE6 (LIST 6 NIL))
  )
  (SETQ	FINAL_LIST
	 (LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 TEMP_ELE4 TEMP_ELE5
	       TEMP_ELE6)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun Block_Ename1 (blockName / ename SUB_ENT_LIST I TEMP_ELE LIST2 TEMP_LIST)
  (if (setq ename (tblobjname "block" (CDR (ASSOC 2 (ENTGET BLOCKNAME)))))
    (reverse
      (while (setq ename (entnext ename))
	(setq SUB_ENT_LIST (cons ename SUB_ENT_LIST))
      )
    )
  )
  (SETQ I 0)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH SUB_ENT_LIST))
    (SETQ TEMP_LIST
	   (FILTER_LIST '(10 11 12 13) (ENTGET (NTH I SUB_ENT_LIST)) 0)
    )
    (SETQ TEMP_ELE (ADD_LISTS
		     (N_ELE_LIST
		       (CDR (ASSOC 8 (ENTGET (NTH I SUB_ENT_LIST))))
		       (LENGTH TEMP_LIST)
		     )
		     (ADD_LISTS
		       (N_ELE_LIST
			 (CDR (ASSOC 0 (ENTGET (NTH I SUB_ENT_LIST))))
			 (LENGTH TEMP_LIST)
		       )
		       TEMP_LIST
		     )

		   )
    )
    (SETQ LIST2 (APPEND LIST2 TEMP_ELE))
    (SETQ I (+ I 1))
  )
  (SETQ LIST2 (REVERSE LIST2))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SORT_FUN	(LIST1 FLAG1 FLAG2 /)
  (IF (= NIL (VL-CONSP (CAR LIST1)))
    (PROGN (SETQ LIST1 (INDEX_ADD LIST1))
	   (SETQ LIST1
		  (VL-SORT LIST1
			   '(LAMBDA (X Y) (< (CADR X) (CADR Y)))
		  )
	   )
	   (SETQ LIST1 (MAPCAR '(LAMBDA (X) (CADR X)) LIST1))
    )
    (PROGN
      (IF (NOT (ATOM (NTH FLAG1 (NTH 0 LIST1))))
	(SETQ LIST1
	       (VL-SORT
		 LIST1
		 '(LAMBDA (X Y)
		    (< (NTH FLAG2 (NTH FLAG1 X)) (NTH FLAG2 (NTH FLAG1 Y)))
		  )
	       )
	)
	(PROGN (SETQ LIST1
		      (VL-SORT LIST1
			       '(LAMBDA (X Y) (< (NTH FLAG2 X) (NTH FLAG2 Y)))
		      )
	       )
	)
      )
    )
  )
  LIST1
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DA CANTILEVERS AND MASTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN GET_FLIP
       (ENT_NAME LAYERLIST CRITERIA POS1 POS2 SORTPOS / TEMP_ELE)
(SETQ TEMP_ELE NIL)
  (SETQ
    TEMP_ELE
     (CAR
       (SORT_FUN (FILTER_LIST LAYERLIST (BLOCK_ENAME1 ENT_NAME) 0)
		 0
		 SORTPOS
       )
     )
  )
  (IF (= (NTH 1 TEMP_ELE) CRITERIA)
    (SETQ TEMP_ELE POS1)
    (SETQ TEMP_ELE POS2)
  )
  
  TEMP_ELE
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ANCHORING BLOCKS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN GET_FLIP1
       (ENT_NAME LAYERLIST CRITERIA POS1 POS2 SORTPOS / TEMP_ELE)
(SETQ TEMP_ELE NIL)
  (SETQ
    TEMP_ELE
     (CAR
       (SORT_FUN (FILTER_LIST '("LINE" "LWPOLYLINE") (FILTER_LIST LAYERLIST (BLOCK_ENAME1 ENT_NAME) 0) 1)
		 0
		 SORTPOS
       )
     )
  )
  
  (IF (= (NTH 1 TEMP_ELE) CRITERIA)
    (SETQ TEMP_ELE POS1)
    (SETQ TEMP_ELE POS2)
  )
  TEMP_ELE
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UPRIGHTCANTILEVER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;IT IS VERY SPECFIC PROGRAM TO UPRIGHT CANTILEVER BLOCK IT WON'T WORK FOR REMAINING BLOCKS
;;;;;;;;INPUT_LIST PATTERN ((START_PT END_PT)(START_PT)(END_PT))
;;;;;;;;OUPUT IS VARIBLE IT HAVE TWO OPTIONS "UP" OR "DN"
;;;;;;;;;;UPRIGHT CANTILEVER LINE MUST BE HORIZONTAL AND VERTICAL , IT CONTAINS ONE HORIZONTAL LINE MUST......
(DEFUN UPRIGHT_FLIP
       (LIST1 / I TEMP_ELE HOR_SET VER_SET DIFF HOR_ELE VER_ELE)
  (SETQ I 0)
  (SETQ HOR_SET NIL)
  (SETQ VER_SET NIL)
  ;;;;;;;;;;;;;;;;;DECIDING OF LINE NATURE WHEATHER IT IS VERTICAL OR HORIZONTAL
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH I LIST1))
    (IF	(/= (FIX (CAR (CAR TEMP_ELE)))
	   (FIX (CAR (CADR TEMP_ELE)))
	)
      (PROGN (SETQ TEMP_ELE (LIST (CONS "H" (CAR TEMP_ELE))
				  (CONS "H" (CADR TEMP_ELE))
			    )
	     )
	     (SETQ HOR_SET TEMP_ELE)
      )
      (PROGN (SETQ TEMP_ELE (LIST (CONS "V" (CAR TEMP_ELE))
				  (CONS "V" (CADR TEMP_ELE))
			    )
	     )
	     (SETQ VER_SET (CONS TEMP_ELE VER_SET))
      )
    )
    (SETQ I (+ I 1))
    (SETQ TEMP_ELE NIL)
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (SETQ HOR_ELE (CDR (NTH 0 HOR_SET)))
  (SETQ VER_SET (MAPCAR '(LAMBDA (X) (CDR X)) (LIST_FORM VER_SET)))
  ;;;;;;;;;;;;;;;;TAKING ONE VERTICAL DATA AND HORIZONTAL DATA FOR COMPARISION
  (SETQ I 0)
  (WHILE (< I (LENGTH VER_SET))
    (SETQ TEMP_ELE (DISTANCE (NTH I VER_SET) HOR_ELE))
    (SETQ DIFF (CONS (LIST I TEMP_ELE) DIFF))
    (SETQ I (+ I 1))
  )
  (SETQ VER_ELE (NTH (CAR (LAST (SORT_FUN DIFF 0 1))) VER_SET))
  (IF (< (CADR HOR_ELE) (CADR VER_ELE))
    "UP"
    "DN"
  )
)
;;OUTPUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;STAGGERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN GET_FLIP3
		 (ENT_NAME   LAYERLIST	/	   TEMP_ELE
		  TEMP_ELE1  TEMP_ELE2	I	   LIST1
		  J	     K		TEMP_ELE3  LIST2
		 )
  (SETQ TEMP_ELE NIL)
  (SETQ
    TEMP_ELE

    (FILTER_LIST '("LINE" "SOLID") (SORT_FUN (FILTER_LIST LAYERLIST (BLOCK_ENAME1 ENT_NAME) 0)
	       0
	       3
     ) 1)

  )
  (SETQ I 0)
  (SETQ LIST1 NIL)
  (WHILE (< I (LENGTH TEMP_ELE))
    (SETQ J 0)
    (SETQ TEMP_ELE1 NIL)
    (WHILE (< J 6)
      (SETQ TEMP_ELE1 (CONS (NTH I TEMP_ELE) TEMP_ELE1))
      (SETQ J (+ J 1))(SETQ I (+ I 1))
    )

    (SETQ LIST1 (CONS TEMP_ELE1 LIST1))
    
  )
  (SETQ LIST1 (REVERSE LIST1))
  (SETQ K 0)
  (SETQ TEMP_ELE3 NIL)
  (SETQ LIST2 NIL)
  (WHILE (< K (LENGTH LIST1))
    (SETQ TEMP_ELE3 (NTH K LIST1))
    (SETQ TEMP_ELE3 (SORT_FUN TEMP_ELE3 0 4))
    (IF	(= (NTH 1 (CAR TEMP_ELE3)) "LINE")
      (SETQ LIST2 (CONS "UP" LIST2))
      (SETQ LIST2 (CONS "DN" LIST2))
    )(SETQ K(+ K 1))
  )(SETQ LIST2(REVERSE LIST2))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN GET_FLIPSTATES (ENT_NAME BLKEFFNAME / LIST1 LIST2 LIST3 LIST4)
  (SETQ	LIST1 '("SINGLE_CANT_MAST"  "SINGLE_CANT_DA"
		"DOUBLE_CANT_MAST"  "DOUBLE_CANT_DA"
		"TRIPLE_CANT_MAST"  "TRIPLE_CANT_DA"
		"TTC"		    "SS1"
		"SS2"		    "SS3"
		"SS4"               "BOX_TYPE"
	       )
	LIST2 '("ANCHOR")
	LIST3 '("SINGLE_CANT_UPRIGHT"
		"DOUBLE_CANT_UPRIGHT"
		"TRIPLE_CANT_UPRIGHT"
	       )
	LIST4 '("SINGLE_CANT_MAST"     "SINGLE_CANT_DA"
		"DOUBLE_CANT_MAST"     "DOUBLE_CANT_DA"
		"TRIPLE_CANT_MAST"     "TRIPLE_CANT_DA"
		"TTC"		       "SS1"
		"SS2"		       "SS3"
		"SS4"		       "SINGLE_CANT_UPRIGHT"
		"DOUBLE_CANT_UPRIGHT"  "TRIPLE_CANT_UPRIGHT" "BOX_TYPE"
	       )
  )
  
  (SETQ FLIP_STATE1 NIL)
  (IF (/= (VL-POSITION BLKEFFNAME (APPEND LIST1 LIST2 LIST3 LIST4))
	  NIL
      )

    (PROGN (IF (/= (VL-POSITION BLKEFFNAME LIST1) NIL)
	     (SETQ FLIP_STATE
		    (GET_FLIP ENT_NAME '("STRUCTURE") "LINE" "DN" "UP" 4)
	     )
	   )


	   (IF (/= (VL-POSITION BLKEFFNAME LIST2) NIL)
	     (SETQ FLIP_STATE
		    (GET_FLIP1 ENT_NAME '("ANCHOR") "LINE" "END" "START" 3)
	     )
	   )

	   (IF (/= (VL-POSITION BLKEFFNAME LIST3) NIL)
	     (SETQ FLIP_STATE
		    (UPRIGHT_FLIP (SUB_ENT_DATA_COLLECT ENT_NAME '((0 . "LINE")(8 . "STRUCTURE")) 10 11))
	     )
	   )


	   (IF (/= (VL-POSITION BLKEFFNAME LIST4) NIL)
	     (SETQ FLIP_STATE1 (GET_FLIP3 ENT_NAME '("STAGGER")))
	   )
    )
    (SETQ FLIP_STATE NIL)
  )
  (LIST FLIP_STATE FLIP_STATE1)
)


(DEFUN MERGE_FLIPSTATES	(LIST1 LIST2 / I J TEMP_ELE LIST3)
  (SETQ I 0)
  (SETQ J 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH I LIST1))
    (IF	(OR (OR (OR (= (NTH 0 TEMP_ELE) "STAGGER1") (= (NTH 0 TEMP_ELE) "STAGGER2")) (= (NTH 0 TEMP_ELE) "STAGGER3")) (= (NTH 0 TEMP_ELE) "STAGGER"))
      (PROGN (SETQ TEMP_ELE (APPEND TEMP_ELE (LIST (NTH J LIST2)))) (SETQ J (+ J 1)))
    )
    (SETQ LIST3 (CONS TEMP_ELE LIST3))
    (SETQ I (+ I 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DATA_COLLECT2 (POINT1	  POINT2	 FILTER_LIST
			   OUTPUT_DFX1	 OUTPUT_DFX2 /		 I
			   TEMP_ENAME	  ENT_OBJECT	 SAFEARRAY_NAME
			   ENT_OBJECT_NAME		 TEMP_ELE TEMP_ELE1 ATTRIBUTE_SET SS_SET
			  )
  (SETQ
    SS_SET
     (SSGET "W" POINT1 POINT2 FILTER_LIST)
  )
  (SETQ I 0)(SETQ TEMP_ELE NIL)
  (SETQ ATTRIBUTE_SET NIL)
  (WHILE (< I (SSLENGTH SS_SET))
    (IF	(OR (= OUTPUT_DFX1 66) (= OUTPUT_DFX1 2))
      (PROGN (SETQ TEMP_ENAME (SSNAME SS_SET I))
	     (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT TEMP_ENAME))
	    (IF (= OUTPUT_DFX1 66)(PROGN (SETQ SAFEARRAY_NAME
		    (VLAX-SAFEARRAY->LIST
		      (VLAX-VARIANT-VALUE
			(VLAX-INVOKE-METHOD ENT_OBJECT 'GetAttributes)
		      )
		    )
	     )
	     (SETQ ENT_OBJECT_NAME
		    (VLAX-GET-PROPERTY
		      (NTH 0 SAFEARRAY_NAME)
		      "TEXTSTRING"
		    )
	     ))(PROGN (SETQ ENT_OBJECT_NAME (VLAX-GET-PROPERTY ENT_OBJECT 'EFFECTIVENAME)))
      ))
      (PROGN
	(SETQ ENT_OBJECT_NAME
	       (CDR (ASSOC OUTPUT_DFX1
			   (ENTGET (SSNAME SS_SET I))
		    )
	       )
	)
      )
    )
    (SETQ TEMP_ELE
	   (CDR (ASSOC OUTPUT_DFX2 (ENTGET (SSNAME SS_SET I))))
    )
    (SETQ TEMP_ELE1 (LIST ENT_OBJECT_NAME TEMP_ELE))
(SETQ ATTRIBUTE_SET (CONS TEMP_ELE1 ATTRIBUTE_SET))
    (SETQ I (+ I 1))
    
  )ATTRIBUTE_SET
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN YARD_SSGET (WPT1 WPT2 BLOCKNAME_LIST / LIST1)

  (SETQ LIST1 (SINGLE_ELE_LIST (FILTER_LIST BLOCKNAME_LIST (DATA_COLLECT2 WPT1 WPT1 '((0 . "INSERT")) 2 -1)  0) 1))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN YARD_DATA_COLLECT (WPT1 WPT2 BLOCKNAME_LIST J K / LIST1 I TEMP_ELE TEMP_LIST LIST2)

  (SETQ LIST1 (SINGLE_ELE_LIST (FILTER_LIST BLOCKNAME_LIST (DATA_COLLECT2 WPT1 WPT2 '((0 . "INSERT")) 2 -1)  0) 1))
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ TEMP_LIST NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_LIST (YARD_STRUCTURE_INFO (NTH I LIST1)))
  (SETQ TEMP_ELE (LIST (NTH 1 (ASSOC J  TEMP_LIST)) (NTH 1 (ASSOC K  TEMP_LIST))))
  (SETQ LIST2 (CONS TEMP_ELE LIST2))
    (SETQ I (+ I 1))
    )
  (REVERSE LIST2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN BLOCK_REFERENCE_LENGTHS (BLOCKNAME EBLOCKNAME / BASE_POINT POINTS_LIST I LIST2 TEMP_ELE TEMP_ELE1 J LIST1)

(SETQ BASE_POINT (CDR (ASSOC 10 (ENTGET (TBLOBJNAME "BLOCK" BLOCKNAME)))))
(SETQ POINTS_LIST (BUILD_LIST (FILTER_LIST '("LINE") (BLOCK_ENAME1 EBLOCKNAME) 1) '(3 4 5)))
(SETQ I 0)
(SETQ LIST2 NIL)
(SETQ TEMP_ELE NIL)
(SETQ TEMP_ELE1 NIL)
(WHILE (< I (LENGTH POINTS_LIST))
  (SETQ TEMP_ELE (NTH I POINTS_LIST))
  (SETQ J 0)
  (SETQ LIST1 NIL)
  (WHILE (< J (LENGTH BASE_POINT)) 
    (SETQ TEMP_ELE1 (- (NTH J BASE_POINT) (NTH J TEMP_ELE)))
    (SETQ LIST1 (CONS TEMP_ELE1 LIST1))
    (SETQ J(+ J 1))
    )
  (SETQ LIST2(CONS (REVERSE LIST1) LIST2))
  (SETQ I (+ I 1))
  )
(SETQ LIST2 (SORT_FUN (REVERSE LIST2) 0 1))
)




(DEFUN SUM_LIST (point1 list11 / BASE_POINT POINTS_LIST I LIST2 TEMP_ELE TEMP_ELE1 J LIST1)

(SETQ I 0)
(SETQ LIST2 NIL)
(SETQ TEMP_ELE NIL)
(SETQ TEMP_ELE1 NIL)
(WHILE (< I (LENGTH list11))
  (SETQ TEMP_ELE (NTH I list11))
  (SETQ J 0)
  (SETQ LIST1 NIL)
  (WHILE (< J (LENGTH point1)) 
    (SETQ TEMP_ELE1 (- (NTH J point1) (NTH J TEMP_ELE)))
    (SETQ LIST1 (CONS TEMP_ELE1 LIST1))
    (SETQ J(+ J 1))
    )
  (SETQ LIST2(CONS (REVERSE LIST1) LIST2))
  (SETQ I (+ I 1))
  )
(SETQ LIST2 (REVERSE LIST2))
  )


(DEFUN SUM_LIST1 (point1 list11 / BASE_POINT POINTS_LIST I LIST2 TEMP_ELE TEMP_ELE1 J LIST1)

(SETQ I 0)
(SETQ LIST2 NIL)
(SETQ TEMP_ELE NIL)
(SETQ TEMP_ELE1 NIL)
(WHILE (< I (LENGTH list11))
  (SETQ TEMP_ELE (NTH I list11))
  (SETQ J 0)
  (SETQ LIST1 NIL)
  (WHILE (< J (LENGTH point1)) 
    (SETQ TEMP_ELE1 (+ (NTH J point1) (NTH J TEMP_ELE)))
    (SETQ LIST1 (CONS TEMP_ELE1 LIST1))
    (SETQ J(+ J 1))
    )
  (SETQ LIST2(CONS (REVERSE LIST1) LIST2))
  (SETQ I (+ I 1))
  )
(SETQ LIST2 (REVERSE LIST2))
  )





(DEFUN GET_CANTILEVERS ( PT NAME_BLK ENAMEBLK / SS_SET I LIST1 ENAME BS_POINT TEMP_ELE LIST1)
(SETQ SS_SET (SSGET "F" (SUM_LIST PT (BLOCK_REFERENCE_LENGTHS NAME_BLK ENAMEBLK)) '((0 . "INSERT"))))
  (SETQ I 0)
  (SETQ ENAME NIL)
  (SETQ BS_POINT NIL)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST1 NIL)
  (IF (/= SS_SET NIL)
(WHILE (< I (SSLENGTH SS_SET))
      (SETQ ENAME (SSNAME SS_SET I))
  (IF (/= ENAME ENAMEBLK)
      (PROGN (SETQ BS_POINT (CDR (ASSOC 10 (ENTGET ENAME))))
      (SETQ TEMP_ELE (APPEND (LIST ENAME) BS_POINT))
      
      
      (SETQ LIST1 (CONS TEMP_ELE LIST1))))
      (SETQ I (+ I 1))
    )
(SETQ LIST1 (REVERSE (SORT_FUN LIST1 0 2)))
    )(SINGLE_ELE_LIST LIST1 0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (DEFUN	BREAK_LIST (LIST1 LIST2 / I TEMP_ELE J TEMP_ELE1 LIST3)
   (SETQ I 0)
   (SETQ TEMP_ELE NIL)
   (SETQ J 0)

   (WHILE (< I (LENGTH LIST2))
     (SETQ TEMP_ELE (NTH I LIST2))
     (SETQ TEMP_ELE1 NIL)
     (WHILE (< J TEMP_ELE)
       (SETQ TEMP_ELE1 (CONS (NTH J LIST1) TEMP_ELE1))
       (SETQ J (+ J 1))
     )
     (SETQ LIST3 (CONS (REVERSE TEMP_ELE1)
		       LIST3
		 )
     )
     (SETQ I (+ I 1))
   )
   (REVERSE LIST3)
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN GET_TEXT_DATA (CPT1 CPT2 LAYER_NAME SET_NO / STRUCTURE_DATA UP_LINE_STRUCTURE_DATA DN_LINE_STRUCTURE_DATA)
  (SETQ SET_NO (- SET_NO 1))
(SETQ STRUCTURE_DATA (SORT_FUN (DATA_COLLECT2 CPT1 CPT2 (LIST (CONS 0  "TEXT") (CONS 8 LAYER_NAME)) -1 10) 1 1))
(SETQ STRUCTURE_DATA (BREAK_LIST  STRUCTURE_DATA (LIST (NTH 0 (ADDITION '(1) (DIV_FUN1 (SINGLE_ELE_LIST STRUCTURE_DATA 1) 1))) (LENGTH STRUCTURE_DATA))))
(SETQ UP_LINE_STRUCTURE_DATA (REVERSE (SORT_FUN (NTH 1 STRUCTURE_DATA) 1 1)))
(SETQ DN_LINE_STRUCTURE_DATA (REVERSE (SORT_FUN (NTH 0 STRUCTURE_DATA) 1 1)))
(SETQ UP_LINE_STRUCTURE_DATA (BREAK_LIST UP_LINE_STRUCTURE_DATA (APPEND (ADDITION (N_ELE_LIST 1 SET_NO) (SORT_FUN (DIV_FUN1 UP_LINE_STRUCTURE_DATA SET_NO) 0 0)) (LIST (LENGTH UP_LINE_STRUCTURE_DATA)))))
(SETQ DN_LINE_STRUCTURE_DATA (BREAK_LIST DN_LINE_STRUCTURE_DATA (APPEND (ADDITION (N_ELE_LIST 1 SET_NO) (SORT_FUN (DIV_FUN1 DN_LINE_STRUCTURE_DATA SET_NO) 0 0)) (LIST (LENGTH DN_LINE_STRUCTURE_DATA)))))
  (LIST UP_LINE_STRUCTURE_DATA DN_LINE_STRUCTURE_DATA)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN MAP_NEAREST (LIST1 PT K J / I TEMP_SET LIST2)
  (SETQ I 0)
  (SETQ TEMP_SET NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))

    (IF	(NOT (ATOM (NTH K (NTH I LIST1))))
      (PROGN (SETQ TEMP_SET
		    (LIST
		      (ABS (- (NTH J (NTH K (NTH I LIST1))) (NTH J PT)))
		      (NTH I LIST1)
		    )
	     )
	     (SETQ LIST2 (CONS TEMP_SET LIST2))
      )
      (PROGN (SETQ TEMP_SET (LIST (ABS (- (NTH J (NTH I LIST1)) (NTH J PT)))
				  (NTH I LIST1)
			    )
	     )
	     (SETQ LIST2 (CONS TEMP_SET LIST2))
      )
    )



    (SETQ I (+ I 1))
  )
  (NTH 1 (NTH 0 (SORT_FUN LIST2 0 0)))

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DATA_TO_STRUCTURE_MAP (LIST1 LIST2 K Q J / I TEMP_ELE LIST3)
(SETQ I 0)
(SETQ TEMP_ELE NIL)
(SETQ LIST3 NIL)
(WHILE (< I (LENGTH LIST1))
  (SETQ TEMP_ELE (MAP_NEAREST LIST2 (NTH K (NTH I LIST1)) Q J))
  (SETQ TEMP_ELE (APPEND (LIST (NTH 0 (NTH I LIST1))) TEMP_ELE ))
  (SETQ LIST3 (CONS TEMP_ELE LIST3))
  
  (SETQ I (+ I 1))
  )
LIST3
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN EXTRACT_ENTITY_INFO (LIST1 J K / I TEMP_ELE TEMP_LIST LIST2)
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ TEMP_LIST NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_LIST (YARD_STRUCTURE_INFO (NTH I LIST1)))
  (SETQ TEMP_ELE (LIST (NTH 1 (ASSOC J  TEMP_LIST)) (NTH 1 (ASSOC K  TEMP_LIST))))
  (SETQ LIST2 (CONS TEMP_ELE LIST2))
    (SETQ I (+ I 1))
    )
  (REVERSE LIST2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN MAP_NEAREST1 (LIST1 PT K / I TEMP_SET LIST2)
  (SETQ I 0)
  (SETQ TEMP_SET NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))

    (IF	(NOT (ATOM (NTH K (NTH I LIST1))))
      (PROGN (SETQ TEMP_SET
		    (LIST
		      (ABS (DISTANCE2D (NTH K (NTH I LIST1)) PT))
		      (NTH I LIST1)
		    )
	     )
	     (SETQ LIST2 (CONS TEMP_SET LIST2))
      )
      (PROGN (SETQ TEMP_SET (LIST (ABS (DISTANCE2D (NTH I LIST1) PT))
				  (NTH I LIST1)
			    )
	     )
	     (SETQ LIST2 (CONS TEMP_SET LIST2))
      )
    )



    (SETQ I (+ I 1))
  )
  (NTH 1 (NTH 0 (SORT_FUN LIST2 0 0)))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN DISTANCE2D ( PT1 PT2 / )
  (SETQ PT1 (LIST (CAR PT1) (CADR PT1)))
  (SETQ PT2 (LIST (CAR PT2) (CADR PT2)))
  (ABS (DISTANCE PT1 PT2))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DATA_TO_STRUCTURE_MAP1 (LIST1 LIST2 K Q  / I TEMP_ELE TEMP_ELE1 LIST3)
(SETQ I 0)
(SETQ TEMP_ELE NIL)
(SETQ TEMP_ELE1 NIL)
(SETQ LIST3 NIL)
(WHILE (< I (LENGTH LIST1))
  (SETQ TEMP_ELE1 (MAP_NEAREST1 LIST2 (NTH K (NTH I LIST1)) Q ))
  (SETQ TEMP_ELE (APPEND (LIST (NTH 0 (NTH I LIST1))) TEMP_ELE1 ))
  (SETQ LIST3 (CONS TEMP_ELE LIST3))
  (SETQ LIST2 (VL-REMOVE TEMP_ELE1 LIST2))
  (SETQ I (+ I 1))
  )
LIST3
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










                                                                             ;ATTRIBUTE MODIFICATION FUNCTIONS



(DEFUN MODIFY_ATTRIBUTES (ENTNAME IDENTIFIER VALUE / TEMP_ELE ENT_OBJECT SAFEARRAY_SET I J)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

  (SETQ I 0)
  (SETQ J 0)
  (SETQ LIST1 NIL)
  (WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ TEMP_ELE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING"))
    (IF (/= (VL-POSITION TEMP_ELE IDENTIFIER) NIL) (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "TEXTSTRING" (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE)) ))
    (SETQ I (+ I 1))
  )  
)))






(DEFUN INSERT_ATTRIBUTES (ENTNAME IDENTIFIER POINT / TEMP_ELE ENT_OBJECT SAFEARRAY_SET I J)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT)) 
(IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

(SETQ I 0)
  (SETQ J 0)
  (SETQ LIST1 NIL)
(WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ TEMP_ELE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING"))
    (IF (/= (VL-POSITION TEMP_ELE IDENTIFIER) NIL) (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "INSERTIONPOINT" (MAKE_SAFEARRAY (NTH (VL-POSITION TEMP_ELE IDENTIFIER) POINT))) ))
    (SETQ I (+ I 1))
  )
)))



(DEFUN INSERT_ATTRIBUTES1 (ENTNAME IDENTIFIER POINT / TEMP_ELE ENT_OBJECT SAFEARRAY_SET BS_POINT POINT IDENTIFIER_VALUE_LIST I J)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ BS_POINT (CDR(ASSOC 10 (ENTGET ENTNAME))))
  (SETQ POINT (SUM_LIST '(0 0 0) (SUM_LIST BS_POINT POINT)))
  (SETQ IDENTIFIER_VALUE_LIST (ATTACH_STRING IDENTIFIER POINT))
  (SETQ IDENTIFIER (SINGLE_ELE_LIST IDENTIFIER_VALUE_LIST 0))
  (SETQ POINT (SINGLE_ELE_LIST IDENTIFIER_VALUE_LIST 1))
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
      (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (NTH (VL-POSITION TEMP_ELE IDENTIFIER) POINT) )))
    (SETQ I (+ I 1))
  )
)))






  
(DEFUN MAKE_SAFEARRAY (LIST1  / I N SAFE_LIST )
  (SETQ N (- (LENGTH LIST1) 1))
  (SETQ SAFE_LIST (VLAX-MAKE-SAFEARRAY VLAX-VBDOUBLE (CONS 0 N)))
  (SETQ I 0)
  (WHILE (< I N)
     (VLAX-SAFEARRAY-PUT-ELEMENT SAFE_LIST I (NTH I LIST1))
    (SETQ I (+ I 1))
  )SAFE_LIST
)


(DEFUN ATTACH_STRING (LIST1 LIST2 / I LIST3 TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 TEMP_ELE4 )
  (SETQ I 0)
  (SETQ LIST3 NIL)
  (SETQ TEMP_ELE1 NIL TEMP_ELE2 NIL TEMP_ELE3 NIL TEMP_ELE4 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE1 (NTH I LIST1))
    (SETQ TEMP_ELE2 (NTH I LIST2))
    (SETQ TEMP_ELE3 (LIST (STRCAT TEMP_ELE1 " " "X") (NTH 0 TEMP_ELE2)))
    (SETQ TEMP_ELE4 (LIST (STRCAT TEMP_ELE1 " " "Y") (NTH 1 TEMP_ELE2)))
    (SETQ LIST3 (CONS TEMP_ELE3 LIST3))
    (SETQ LIST3 (CONS TEMP_ELE4 LIST3))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST3)
)



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





(DEFUN CHANGE_FLIP (ENTNAME FLIP_STATE / TEMP_ELE ENT_OBJECT SAFEARRAY_SET BS_POINT POINT IDENTIFIER_VALUE_LIST I J FLIP_STATE)
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
    (IF ( = TEMP_ELE FLIP_STATE)
      (PROGN (SETQ FLIP_STATE (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "VALUE"  )))
	     (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (IF (= FLIP_STATE 0) (vlax-make-variant 1 vlax-vbInteger) (vlax-make-variant 0 vlax-vbInteger)) )
	))
    (SETQ I (+ I 1))
  )
)))



(DEFUN CHANGE_DYNAMIC_PROPERTIES (ENTNAME IDENTIFIER VALUE  / TEMP_ELE ENT_OBJECT SAFEARRAY_SET  I J LIST1)
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
      (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (vlax-make-variant (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) vlax-vbdouble) )))
    (SETQ I (+ I 1))
  )
)))




                                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





                                               ;;;;FUNCTIONS COMBINED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TO BREAK STRING BASED ON THIS SYMBOL "/"
;IT GIVES THE ADDITION LIST OF TWO STRINGS
;INPUT CHAINAGE LIST OR ANY STRING LIST WITH "/"
(DEFUN BREAK_/ (TEMP_SET / I Z TEMP_ELE ELE1 ELE2 TEMP_SET1)
  (SETQ I 0)
  (WHILE (< I (LENGTH TEMP_SET))
    (SETQ TEMP_ELE (NTH I TEMP_SET))
    (IF	(= T (VL-CONSP TEMP_ELE))
      (SETQ TEMP_ELE (CAR TEMP_ELE))
    )
    (SETQ Z (IF	(VL-STRING-POSITION (ASCII "/") TEMP_ELE)
	      (VL-STRING-POSITION (ASCII "/") TEMP_ELE)
	      (VL-STRING-POSITION (ASCII "\\") TEMP_ELE)
	    )
    )
    (IF	(= Z NIL)
      (PROGN
	(ALERT
	  "CHECK THE CHAINAGE,MAST_NUMBER AND KM_STONE ATTRIBUTES FORMATION SYMBOL(/,\\),
    	 SOMETHING IS WRONG WITH THE SYMBOLS,
    	 FOR RUNNING OF PROGRAM WE NEED TO USE THIS SYMBOLS IN CHAINAGE,MAST_NUMBER AND KM_STONE ATTRIBUTES (/,\\)"
	)
	(QUIT)
      )
    )
    (SETQ ELE1 (SUBSTR TEMP_ELE 1 Z))
    (SETQ
      ELE2
       (SUBSTR TEMP_ELE
	       (+ 2 Z)
       )
    )
    (IF	(= ELE2 NIL)
      (PROGN
	(ALERT
	  "CHECK THE CHAINAGE,MAST_NUMBER AND KM_STONE ATTRIBUTES AFTER OR BEFORE THESE SYMBOLS (/,\\)"
	)
	(QUIT)
      )
    )
    (SETQ TEMP_SET1 (CONS (LIST ELE1 ELE2) TEMP_SET1))
    (SETQ I (+ I 1))
  )
  (SETQ TEMP_SET1 (REVERSE TEMP_SET1))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;COLLECTS DATA LIKE ATTRIBUTES, BLOCK NAMES , TEXT OBJECTS ETC.., ALONG WITH THEIR CO-ORDINATES;;;;;;;;;;;;;;;;;
;Syntax (DATA_COLLECT POINT1 POINT2 FILTER_LIST DXF_CODE)
;DXF_CODE : EFFECTIVENAMES----->2 ====> FILTER_LIST '((0 . "INSERT"))
;           ATTRIBUTENAMES----->66====>FILTER_LIST '((0 . "INSERT")(66 . 1))
;           TEXT_OBJECTS------>1 =====> FILTER_LIST BASED ON REQUIREMENT
(DEFUN DATA_COLLECT (POINT1	  POINT2	 FILTER_LIST
			   OUTPUT_DFX	  /		 I
			   TEMP_ENAME	  ENT_OBJECT	 SAFEARRAY_NAME
			   ENT_OBJECT_NAME		 TEMP_ELE TEMP_ELE1 ATTRIBUTE_SET SS_SET
			  )
  (SETQ
    SS_SET
     (SSGET "W" POINT1 POINT2 FILTER_LIST)
  )
  (SETQ I 0)(SETQ TEMP_ELE NIL)
  (SETQ ATTRIBUTE_SET NIL)
  (WHILE (< I (SSLENGTH SS_SET))
    (IF	(OR (= OUTPUT_DFX 66) (= OUTPUT_DFX 2))
      (PROGN (SETQ TEMP_ENAME (SSNAME SS_SET I))
	     (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT TEMP_ENAME))
	    (IF (= OUTPUT_DFX 66)(PROGN (SETQ SAFEARRAY_NAME
		    (VLAX-SAFEARRAY->LIST
		      (VLAX-VARIANT-VALUE
			(VLAX-INVOKE-METHOD ENT_OBJECT 'GetAttributes)
		      )
		    )
	     )
	     (SETQ ENT_OBJECT_NAME
		    (VLAX-GET-PROPERTY
		      (NTH 0 SAFEARRAY_NAME)
		      "TEXTSTRING"
		    )
	     ))(PROGN (SETQ ENT_OBJECT_NAME (VLAX-GET-PROPERTY ENT_OBJECT 'EFFECTIVENAME)))
      ))
      (PROGN
	(SETQ ENT_OBJECT_NAME
	       (CDR (ASSOC OUTPUT_DFX
			   (ENTGET (SSNAME SS_SET I))
		    )
	       )
	)
      )
    )
    (SETQ TEMP_ELE
	   (CDR (ASSOC 10 (ENTGET (SSNAME SS_SET I))))
    )
    (SETQ TEMP_ELE1 (CONS ENT_OBJECT_NAME TEMP_ELE))
(SETQ ATTRIBUTE_SET (CONS TEMP_ELE1 ATTRIBUTE_SET))
    (SETQ I (+ I 1))
    
  )ATTRIBUTE_SET
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;FILTERS A GIVEN LIST BASED ON THE ELEMENTS IN OTHER GIVEN LIST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;USAGE : LIST1----> '(A B C) ; LIST2 -----> '((A D) (C D) (E B) (E C)) ; POSITION = 0 ===> OUTPUT = '((A D) (C D))
                                                                             ; POSITION = 1 ===> OUTPUT = '((E B) (E C))
(DEFUN FILTER_LIST (LIST1 LIST2 POSITION / I LOOP_ELE TEMP_ELE LIST3)
  (SETQ I 0)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST2))
    (SETQ LOOP_ELE (NTH POSITION (NTH I LIST2)))
    (IF	(>= (VL-POSITION LOOP_ELE LIST1) 0)
      (PROGN (SETQ TEMP_ELE (NTH I LIST2))
	     (SETQ LIST3 (CONS TEMP_ELE LIST3))
      )
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DIV_LIST_INDEX
		      (LIST1 FLAG / I A DIFF TEMP_SET TEMP_SET1)
  (SETQ I 0)
  (SETQ TEMP_SET NIL)
  (SETQ TEMP_SET1 NIL)
  (SETQ TEMP_SET2 NIL)
  
   
      (SETQ TEMP_SET (SINGLE_ELE_LIST LIST1 FLAG))
      (SETQ TEMP_SET (VL-SORT TEMP_SET
			      '(LAMBDA (X Y) (< X Y))
		     )
      )
      (WHILE (< I (LENGTH TEMP_SET))
	(IF (/= (NTH (+ I 1) TEMP_SET) NIL)
	  (PROGN (SETQ
		   DIFF	(LIST I
			      (ABS (- (NTH I TEMP_SET)
				      (NTH (+ I 1) TEMP_SET)
				   )
			      )
			)
		 )
		 (SETQ TEMP_SET1 (CONS DIFF TEMP_SET1))
	  )
	)
	(SETQ I (+ I 1))
      )
      (SETQ TEMP_SET1 (VL-SORT TEMP_SET1
			       '(LAMBDA (X Y) (< (CADR X) (CADR Y)))
		      )
      )
      ;(SETQ A (+ 1 (CAR (NTH (- (LENGTH TEMP_SET1) 1) TEMP_SET1))))
(SETQ TEMP_SET1 (REVERSE TEMP_SET1))
    
  

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN INDEX_ADD (LIST1 / I LIST2)
  (SETQ I 0)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (IF	(= (VL-CONSP (NTH I LIST1)) NIL)
      (SETQ LIST2 (CONS (LIST I (NTH I LIST1)) LIST2))
      (SETQ LIST2 (CONS (CONS I (NTH I LIST1)) LIST2))
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST2 (REVERSE LIST2))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;FUNCTION FOR LOCATION MAPPING TO ANOTHER LIST;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LIST1 CONTAINS LOC LIST AND ANCHOR_SET CONTAINS MAP_LIST
(DEFUN LOC_MAP_LIST
       (LIST1 ANCHOR_SET / I J K TEMP_ELE TEMP_SET TEMP_SET1)
  (SETQ J 0)
  (SETQ LIST1 (MAPCAR '(LAMBDA (X) (CONS NIL (CDR X))) LIST1))
  (WHILE (< J (LENGTH ANCHOR_SET))
    (SETQ I 0)
    (SETQ TEMP_SET NIL)
    (WHILE (< I (LENGTH LIST1))
      (SETQ TEMP_ELE
	     (ABS (- (CADR (NTH J ANCHOR_SET)) (CADR (NTH I LIST1)))
	     )
      )
      (SETQ TEMP_SET (CONS TEMP_ELE TEMP_SET))
      (SETQ I (+ I 1))
    )
    (SETQ TEMP_SET (REVERSE TEMP_SET))
    (SETQ TEMP_SET1 TEMP_SET)
    (SETQ TEMP_SET1
	   (VL-SORT TEMP_SET1
		    '(LAMBDA (X Y) (< X Y))
	   )
    )
    (SETQ K (VL-POSITION (NTH 0 TEMP_SET1) TEMP_SET))
    (SETQ LIST1
	   (SUBST (CONS (CAR (NTH J ANCHOR_SET)) (CDR (NTH K LIST1)))
		  (NTH K LIST1)
		  LIST1
	   )
    )
    (SETQ J (+ J 1))
  )
  LIST1
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;SET1 CONTAINS LIST WITH DATA AND POINT SET WITH SORTED ORDER
					;SET2 CONTAINS LIST WITH DATA AND POINT_SET WITH SORTED ORDER
					;FUNCTIONING OF THIS PROGRAM IS TAKE EACH ELEMENT IN SET2 AND MAP IT TO THE NEAREST ELEMENT IN SET1 ,IF NO ELEMENT IS NEAREST TO A ONE PARTICULAR ELEMENT IN SET1 IT SIMPLY PUT NIL
					;INTIAL SET ALL ELEMENTS IS REPLACED WITH NILS AND COMPARISIO IS DONE NEAREST ELEMENT WAS FOUND IT REPLACES THAT ELEMENT IN SET1 WITH SET2 OTHERWISE NIL WILL BE THERE IN SET1
					;EXAMPLE (SET1 SET2 SET1 SET2)
					;OUTPUT (NIL  "AS" NIL NIL NIL "SD")
(DEFUN SET1_SET2 (SET1 SET2 / I J K TEMP_ELE TEMP_SET TEMP_SET1)
  (SETQ J 0)
  (SETQ SET1 (MAPCAR '(LAMBDA (X) (LIST NIL (CADR X))) SET1))
  (SETQ	SET1 (VL-SORT SET1
		      '(LAMBDA (X Y) (< (CADR X) (CADR Y)))
	     )
  )
  (SETQ	SET2 (VL-SORT SET2
		      '(LAMBDA (X Y) (< (CADR X) (CADR Y)))
	     )
  )
  (WHILE (< J (LENGTH SET2))
    (SETQ I 0)
    (SETQ TEMP_SET NIL)
    (WHILE (< I (LENGTH SET1))
      (SETQ TEMP_ELE
	     (ABS (- (CADR (NTH J SET2)) (CADR (NTH I SET1)))
	     )
      )
      (SETQ TEMP_SET (CONS TEMP_ELE TEMP_SET))
      (SETQ I (+ I 1))
    )
    (SETQ TEMP_SET (REVERSE TEMP_SET))
    (SETQ TEMP_SET1 TEMP_SET)
    (SETQ TEMP_SET1
	   (VL-SORT TEMP_SET1
		    '(LAMBDA (X Y) (< X Y))
	   )
    )
    (SETQ K (VL-POSITION (NTH 0 TEMP_SET1) TEMP_SET))
    (SETQ SET1
	   (SUBST (LIST (CAR (NTH J SET2)) (CADR (NTH K SET1)))
		  (NTH K SET1)
		  SET1
	   )
    )
    (SETQ J (+ J 1))
  )
  (SETQ SET1 (MAPCAR '(LAMBDA (X) (CAR X)) SET1))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;FLAG 0 INDICATES SUBLIST OF FIRST ELEMENT ,FLAG 1 INDICATES SUBLIST OF SECOND ELEMENT SORT
					;THIS FUNCTION IS USED FOR THE SORTING OF LIST IN TERMS OF X OR Y CO-ORDINATES
					;SYNTAX (SORT_FUN LIST1 0)
(DEFUN SORT_FUN	(LIST1 FLAG1 FLAG2 /)
  (IF (= NIL (VL-CONSP (CAR LIST1)))
    (PROGN (SETQ LIST1 (INDEX_ADD LIST1))
	   (SETQ LIST1
		  (VL-SORT LIST1
			   '(LAMBDA (X Y) (< (CADR X) (CADR Y)))
		  )
	   )
	   (SETQ LIST1 (MAPCAR '(LAMBDA (X) (CADR X)) LIST1))
    )
    (PROGN
      (IF (NOT (ATOM (NTH FLAG1 (NTH 0 LIST1))))
	(SETQ LIST1
	       (VL-SORT
		 LIST1
		 '(LAMBDA (X Y)
		    (< (NTH FLAG2 (NTH FLAG1 X)) (NTH FLAG2 (NTH FLAG1 Y)))
		  )
	       )
	)
	(PROGN (SETQ LIST1
		      (VL-SORT LIST1
			       '(LAMBDA (X Y) (< (NTH FLAG2 X) (NTH FLAG2 Y)))
		      )
	       )
	)
      )
    )
  )
  LIST1
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;FUNCTION TO COMBINE TWO LISTS

(DEFUN ADD_LISTS (LIST1 LIST2 / I TEMP_ELE MAIN_LIST)
  (SETQ I 0)
  (SETQ MAIN_LIST NIL)
  (WHILE (< I (LENGTH LIST1))
    (IF	(AND (VL-CONSP (NTH I LIST1)) (VL-CONSP (NTH I LIST2)))
      (PROGN (SETQ TEMP_ELE (APPEND (NTH I LIST1) (NTH I LIST2)))
	     (SETQ MAIN_LIST (CONS TEMP_ELE MAIN_LIST))
      )
    )
    (IF	(AND (NOT (VL-CONSP (NTH I LIST1)))
	     (NOT (VL-CONSP (NTH I LIST2)))
	)
      (PROGN (SETQ TEMP_ELE (CONS (NTH I LIST1) (NTH I LIST2)))
	     (SETQ MAIN_LIST (CONS TEMP_ELE MAIN_LIST))
      )
    )
    (IF	(AND (VL-CONSP (NTH I LIST1))
	     (NOT (VL-CONSP (NTH I LIST2)))
	)
      (PROGN (SETQ TEMP_ELE (APPEND (NTH I LIST1) (LIST (NTH I LIST2))))
	     (SETQ MAIN_LIST (CONS TEMP_ELE MAIN_LIST))
      )
    )
    (IF	(AND (NOT (VL-CONSP (NTH I LIST1)))
	     (VL-CONSP (NTH I LIST2))
	)
      (PROGN (SETQ TEMP_ELE (CONS (NTH I LIST1) (NTH I LIST2)))
	     (SETQ MAIN_LIST (CONS TEMP_ELE MAIN_LIST))
      )
    )
    (SETQ I (+ I 1))
  )
  (SETQ MAIN_LIST (REVERSE MAIN_LIST))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;FUNCTION TO PRINT A LIST WITH EQUAL INTERVALS FROM BASE POINT
(DEFUN PRINT_PT	(LIST1 FORMAT BASE_PT DIST / I)
  (SETQ I 0)
  (WHILE (< I (LENGTH LIST1))
    (SETQ
      FORMAT (SUBST (CONS 1 (NTH I LIST1)) (ASSOC 1 FORMAT) FORMAT)
    )
    (SETQ FORMAT
	   (SUBST
	     (LIST 10 (+ (CAR BASE_PT) (* DIST I)) (CADR BASE_PT) 0.0)
	     (ASSOC 10 FORMAT)
	     FORMAT
	   )
    )
    (ENTMAKE FORMAT)
    (SETQ I (+ I 1))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					;FUNCTION TO MAP ELEMENTS IN A LIST FROM GIVEN TWO LISTS

(DEFUN MAP_ELEMENTS (MY_LIST	   MAP_TO_LIST	 MAP_FROM_LIST
		     /		   N		 TEMP_EXCEL
		     COLUMN_EXCEL
		    )
  (SETQ LOCATION_EXCEL_INFO (ADD_LISTS MAP_TO_LIST MAP_FROM_LIST))
  (SETQ N 0)
  (SETQ TEMP_EXCEL NIL)
  (SETQ COLUMN_EXCEL NIL)
  (WHILE (< N (LENGTH MY_LIST))
    (SETQ TEMP_EXCEL
	   (CDR (ASSOC (NTH N MY_LIST) LOCATION_EXCEL_INFO))
    )
    (SETQ COLUMN_EXCEL (CONS TEMP_EXCEL COLUMN_EXCEL))
    (SETQ N (+ N 1))
  )
  (SETQ COLUMN_EXCEL (REVERSE COLUMN_EXCEL))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN MAP_ELEMENTS1 (MY_LIST	    MAP_TO_LIST	  MAP_FROM_LIST
		      /		    N		  TEMP_EXCEL
		      COLUMN_EXCEL
		     )
  (SETQ LOCATION_EXCEL_INFO (ADD_LISTS MAP_TO_LIST MAP_FROM_LIST))
  (SETQ N 0)
  (SETQ TEMP_EXCEL NIL)
  (SETQ COLUMN_EXCEL NIL)
  (WHILE (< N (LENGTH MY_LIST))
    (SETQ TEMP_EXCEL
	   (CDR (ASSOC (NTH N MY_LIST) LOCATION_EXCEL_INFO))
    )
    (IF	(= TEMP_EXCEL NIL)
      (SETQ COLUMN_EXCEL (CONS (NTH N MY_LIST) COLUMN_EXCEL))

      (SETQ COLUMN_EXCEL (CONS TEMP_EXCEL COLUMN_EXCEL))
    )
    (SETQ N (+ N 1))
  )
  (SETQ COLUMN_EXCEL (REVERSE COLUMN_EXCEL))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN MAP_ELEMENTS2 (MY_LIST	    MAP_TO_LIST	  MAP_FROM_LIST
		      /		    N		  TEMP_EXCEL
		      COLUMN_EXCEL
		     )
  (SETQ LOCATION_EXCEL_INFO (ADD_LISTS MAP_TO_LIST MAP_FROM_LIST))
  (SETQ N 0)
  (SETQ TEMP_EXCEL NIL)
  (SETQ COLUMN_EXCEL NIL)
  (WHILE (< N (LENGTH MY_LIST))
    (SETQ TEMP_EXCEL
	   (CDR (ASSOC (NTH N MY_LIST) LOCATION_EXCEL_INFO))
    )
    (IF	(= TEMP_EXCEL NIL)
      (SETQ COLUMN_EXCEL (CONS "" COLUMN_EXCEL))

      (SETQ COLUMN_EXCEL (CONS TEMP_EXCEL COLUMN_EXCEL))
    )
    (SETQ N (+ N 1))
  )
  (SETQ COLUMN_EXCEL (REVERSE COLUMN_EXCEL))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;FUNCTION TO PRINT BLOCKS
(DEFUN BLOCK_PT	(FILE_STR PT / NAME EXT)
  (COMMAND "INSERT" (FINDFILE FILE_STR) PT "" "" "")
  (COMMAND "EXPLODE" "LAST")
  (COMMAND "SCALE" "LAST" "" PT "1")
  (COMMAND "ROTATE" "LAST" "" PT "0")
  (SETQ	EXT (SUBSTR FILE_STR
		    (+ 2 (VL-STRING-POSITION (ASCII ".") FILE_STR))
	    )
  )
  (SETQ	NAME (SUBSTR FILE_STR
		     1
		     (VL-STRING-POSITION (ASCII ".") FILE_STR)
	     )
  )
  (IF (= EXT "DWG")
    (COMMAND "PURGE" "B" NAME "Y" "Y" ^C)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN CREATE_LIST ( / I )
  (SETQ I 0)
  (SETQ ADAPTOR_LIST NIL)
  (SETQ TEMP_ELE (GETINT "ENTER ADAPTOR LOCATION NUMBERS:"))
  (WHILE (/= TEMP_ELE
	     NIL

	 )
    (SETQ ADAPTOR_LIST (CONS (RTOS TEMP_ELE) ADAPTOR_LIST))
    (SETQ TEMP_ELE (GETINT "ENTER ADAPTOR LOCATION NUMBERS:"))
  )
  (SETQ ADAPTOR_LIST (REVERSE ADAPTOR_LIST))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN N_ELE_LIST (XYZ LEN / I)
  (SETQ I 0)
  (SETQ ELE_LIST NIL)
  (WHILE (< I LEN)
    (SETQ ELE_LIST (CONS XYZ ELE_LIST))
    (SETQ I (+ I 1))
  )
  (SETQ ELE_LIST (REVERSE ELE_LIST))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SINGLE_ELE_LIST (LIST1 POS / N TEMP_ELE LIST2)
  (SETQ N 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST2 NIL)
  (WHILE (< N (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH N LIST1))(IF (NOT (ATOM TEMP_ELE))(PROGN
    (SETQ TEMP_ELE (NTH POS TEMP_ELE))
    (SETQ LIST2 (CONS TEMP_ELE LIST2)))(PROGN (SETQ TEMP_ELE TEMP_ELE)
    (SETQ LIST2 (CONS TEMP_ELE LIST2))))
    (SETQ N (+ N 1))
  )
  (SETQ LIST2 (REVERSE LIST2))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN MINUS_LIST (LIST1 LIST2 POS / N TEMP_ELE LIST3)

  (SETQ N 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST3 NIL)

  (WHILE (< N (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH N LIST1))
    (SETQ TEMP_ELE1 (NTH POS TEMP_ELE))
    (SETQ SUBST_ELE (- TEMP_ELE1 (NTH N LIST2)))
    (SETQ TEMP_ELE (SUBST  SUBST_ELE (NTH POS TEMP_ELE) TEMP_ELE))
    (SETQ LIST3 (CONS TEMP_ELE LIST3))
    (SETQ N (+ N 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION FOR DIVISION OF LIST BASED ON Y-CO-ORDINATE DIFFERENCE
;;LIST1 CONTAINS THE SUB ELEMENTS WHICH CONTAINS TEXT WITH X,Y AND Z CO-ORDINATES
(DEFUN DIV_FUN1 (LIST1 FLAG / I A LIST2 )
(SETQ A  (SINGLE_ELE_LIST (REVERSE (SORT_FUN (DIV_LIST_INDEX (SINGLE_ELE_LIST (SORT_FUN LIST1 1 0) 1) 1) 0 1)) 0))
  (SETQ I 0)
  (SETQ LIST2 NIL)
  (WHILE (< I FLAG)
    (SETQ LIST2 (CONS (NTH I A) LIST2))
    (SETQ I (+ I 1))
    )LIST2
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION FOR DIVISION OF LIST BASED ON Y-CO-ORDINATE DIFFERENCE
;;LIST1 CONTAINS THE SUB ELEMENTS WHICH CONTAINS TEXT WITH X,Y AND Z CO-ORDINATES
(DEFUN DIV_FUN (LIST1 / I A DIFF TEMP_SET TEMP_SET1 TEMP_SET2 )
(SETQ I 0)
  (SETQ TEMP_SET NIL)
  (SETQ TEMP_SET1 NIL)
  (SETQ TEMP_SET2 NIL)
(SETQ TEMP_SET (MAPCAR '(LAMBDA (X) (CADDR X)) LIST1))
(SETQ TEMP_SET (VL-SORT	TEMP_SET
			'(LAMBDA (X Y) (< X Y))
	       )
)
(WHILE (< I (LENGTH TEMP_SET))
  (IF (/= (NTH (+ I 1) TEMP_SET) NIL)
    (PROGN (SETQ
	     DIFF (LIST	I
			(ABS (-	(NTH I TEMP_SET)
				(NTH (+ I 1) TEMP_SET)
			     )
			)
		  )
	   )
	   (SETQ TEMP_SET1 (CONS DIFF TEMP_SET1))
    )
  )
  (SETQ I (+ I 1))
)
(SETQ TEMP_SET1	(VL-SORT TEMP_SET1
			 '(LAMBDA (X Y) (< (CADR X) (CADR Y)))
		)
)
(SETQ A (+ 1 (CAR (NTH (- (LENGTH TEMP_SET1) 1) TEMP_SET1))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN C:LAYER_CONVERT ( / UP_LINE DN_LINE WPT1 WPT2 LINE_POINTS UP_LINE_POINTS DN_LINE_POINTS UP_LINE_POINT1 UP_LINE_POINT2 DN_LINE_POINT1 DN_LINE_POINT2 TEXT_OBJECTS)
    (VL-LOAD-COM)
  (SETQ UP_LINE NIL)
  (SETQ DN_LINE NIL)
  (SETQ WPT1 (GETPOINT "\N 1ST CORNER: "))
  (SETQ WPT2 (GETPOINT "\N 2ND CORNER: "))
  (SETQ LAYER_NAME (GETSTRING "\N ENTER LAYER NAME"))
  (IF (/= (TBLSEARCH "LAYER" LAYER_NAME) NIL)
  (PROGN (SETQ LINE_POINTS (DATA_COLLECT2 WPT1 WPT2 '((0 . "LINE") (8 . "LINE_LAYER")) 10 11))
  (SETQ LINE_POINTS (REVERSE (SORT_FUN LINE_POINTS 0 1)))
  (SETQ LINE_POINTS (BREAK_LIST LINE_POINTS '(2 4)))
  (SETQ UP_LINE_POINTS (NTH 0 LINE_POINTS))
  (SETQ DN_LINE_POINTS (NTH 1 LINE_POINTS))
  (SETQ UP_LINE_POINT1 (NTH 0 (REVERSE (SORT_FUN (NTH 0 (SORT_FUN UP_LINE_POINTS 0 0)) 0 1))))
  (SETQ UP_LINE_POINT2 (NTH 1 (REVERSE (SORT_FUN (NTH 1 (SORT_FUN UP_LINE_POINTS 0 0)) 0 1))))
  (SETQ DN_LINE_POINT1 (NTH 0 (REVERSE (SORT_FUN (NTH 0 (SORT_FUN DN_LINE_POINTS 0 0)) 0 1))))
  (SETQ DN_LINE_POINT2 (NTH 1 (REVERSE (SORT_FUN (NTH 1 (SORT_FUN DN_LINE_POINTS 0 0)) 0 1))))
  (SETQ UP_LINE (SINGLE_ELE_LIST (DATA_COLLECT2 UP_LINE_POINT1 UP_LINE_POINT2 '((0 . "TEXT")) -1 1) 0))
  (SETQ DN_LINE (SINGLE_ELE_LIST (DATA_COLLECT2 DN_LINE_POINT1 DN_LINE_POINT2 '((0 . "TEXT")) -1 1) 0))
  (SETQ TEXT_OBJECTS (APPEND UP_LINE DN_LINE))
  (SETQ I 0)
  (WHILE (< I (LENGTH TEXT_OBJECTS))
    (VLA-PUT-LAYER (VLAX-ENAME->VLA-OBJECT (NTH I TEXT_OBJECTS)) LAYER_NAME)
    (SETQ I (+ I 1))
    ))
    (ALERT "LAYER NAME NOT FOUND, CREATE THE LAYER NAME ENTERED.")
    )
   (LIST (LENGTH  UP_LINE) (LENGTH DN_LINE))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN ADDITION	(LIST1 LIST2 / I TEMP_ELE LIST3)

  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (+ (NTH I LIST1) (NTH I LIST2)))
    (SETQ LIST3 (CONS TEMP_ELE LIST3))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST3)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
-------------------------------------------------------------------------------
; NOTE: REVIEW THE CONDITIONS OF EACH ARGUMENT IN THE FUNCTION HEADINGS
;-------------------------------------------------------------------------------
; GETEXCEL - STORES THE VALUES FROM AN EXCEL SPREADSHEET INTO *EXCELDATA@ LIST
; ARGUMENTS: 3
;   EXCELFILE$ = PATH AND FILENAME
;   SHEETNAME$ = SHEET NAME OR NIL FOR NOT SPECIFIED
;   MAXRANGE$ = MAXIMUM CELL ID RANGE TO INCLUDE OR NIL TO GET THE CURRENT REGION FROM CELL A1
; SYNTAX EXAMPLES:
; (GETEXCEL "C:\\TEMP\\TEMP.XLS" "SHEET1" "E19") = OPEN C:\TEMP\TEMP.XLS ON SHEET1 AND READ UP TO CELL E19
; (GETEXCEL "C:\\TEMP\\TEMP.XLS" NIL "XYZ123") = OPEN C:\TEMP\TEMP.XLS ON CURRENT SHEET AND READ UP TO CELL XYZ123
;-------------------------------------------------------------------------------
(DEFUN GETEXCEL (EXCELFILE$ SHEETNAME$ MAXRANGE$ / COLUMN# COLUMNROW@ DATA@ EXCELRANGE^
  EXCELVALUE EXCELVALUE EXCELVARIANT^ MAXCOLUMN# MAXROW# RANGE$ ROW# WORKSHEET)
  (IF (= (TYPE EXCELFILE$) 'STR)
    (IF (NOT (FINDFILE EXCELFILE$))
      (PROGN
        (ALERT (STRCAT "EXCEL FILE " EXCELFILE$ " NOT FOUND."))
        (EXIT)
      );PROGN
    );IF
    (PROGN
      (ALERT "EXCEL FILE NOT SPECIFIED.")
      (EXIT)
    );PROGN
  );IF
  (GC)
  (IF (SETQ *EXCELAPP% (VLAX-GET-OBJECT "EXCEL.APPLICATION"))
    (PROGN
      (ALERT "CLOSE ALL EXCEL SPREADSHEETS TO CONTINUE!")
      (VLAX-RELEASE-OBJECT *EXCELAPP%)(GC)
    );PROGN
  );IF
  (SETQ EXCELFILE$ (FINDFILE EXCELFILE$))
  (SETQ *EXCELAPP% (VLAX-GET-OR-CREATE-OBJECT "EXCEL.APPLICATION"))
  (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% 'WORKBOOKS) 'OPEN EXCELFILE$)
  (IF SHEETNAME$
    (VLAX-FOR WORKSHEET (VLAX-GET-PROPERTY *EXCELAPP% "SHEETS")
      (IF (= (VLAX-GET-PROPERTY WORKSHEET "NAME") SHEETNAME$)
        (VLAX-INVOKE-METHOD WORKSHEET "ACTIVATE")
      );IF
    );VLAX-FOR
  );IF
  (IF MAXRANGE$
    (PROGN
      (SETQ COLUMNROW@ (COLUMNROW MAXRANGE$))
      (SETQ MAXCOLUMN# (NTH 0 COLUMNROW@))
      (SETQ MAXROW# (NTH 1 COLUMNROW@))
    );PROGN
    (PROGN
      (SETQ CURREGION (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY
        (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVESHEET") "RANGE" "A1") "CURRENTREGION")
      );SETQ
      (SETQ MAXROW# (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY CURREGION "ROWS") "COUNT"))
      (SETQ MAXCOLUMN# (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY CURREGION "COLUMNS") "COUNT"))
    );PROGN
  );IF
  (SETQ *EXCELDATA@ NIL)
  (SETQ ROW# 1)
  (REPEAT MAXROW#
    (SETQ DATA@ NIL)
    (SETQ COLUMN# 1)
    (REPEAT MAXCOLUMN#
      (SETQ RANGE$ (STRCAT (NUMBER2ALPHA COLUMN#)(ITOA ROW#)))
      (SETQ EXCELRANGE^ (VLAX-GET-PROPERTY *EXCELAPP% "RANGE" RANGE$))
      (SETQ EXCELVARIANT^ (VLAX-GET-PROPERTY EXCELRANGE^ 'VALUE))
      (SETQ EXCELVALUE (VLAX-VARIANT-VALUE EXCELVARIANT^))
      (SETQ EXCELVALUE
        (COND
          ((= (TYPE EXCELVALUE) 'INT) (ITOA EXCELVALUE))
          ((= (TYPE EXCELVALUE) 'REAL) (RTOSR EXCELVALUE))
          ((= (TYPE EXCELVALUE) 'STR) (VL-STRING-TRIM " " EXCELVALUE))
          ((/= (TYPE EXCELVALUE) 'STR) "")
        );COND
      );SETQ
      (SETQ DATA@ (APPEND DATA@ (LIST EXCELVALUE)))
      (SETQ COLUMN# (1+ COLUMN#))
    );REPEAT
    (SETQ *EXCELDATA@ (APPEND *EXCELDATA@ (LIST DATA@)))
    (SETQ ROW# (1+ ROW#))
  );REPEAT
  (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVEWORKBOOK") 'CLOSE :VLAX-FALSE)
  (VLAX-INVOKE-METHOD *EXCELAPP% 'QUIT)
  (VLAX-RELEASE-OBJECT *EXCELAPP%)(GC)
  (SETQ *EXCELAPP% NIL)
  *EXCELDATA@
);DEFUN GETEXCEL
;-------------------------------------------------------------------------------
; GETCELL - RETURNS THE CELL VALUE FROM THE *EXCELDATA@ LIST
; ARGUMENTS: 1
;   CELL$ = CELL ID
; SYNTAX EXAMPLE: (GETCELL "E19") = VALUE OF CELL E19
;-------------------------------------------------------------------------------
(DEFUN GETCELL (CELL$ / COLUMN# COLUMNROW@ RETURN ROW#)
  (SETQ COLUMNROW@ (COLUMNROW CELL$))
  (SETQ COLUMN# (1- (NTH 0 COLUMNROW@)))
  (SETQ ROW# (1- (NTH 1 COLUMNROW@)))
  (SETQ RETURN "")
  (IF *EXCELDATA@
    (IF (AND (>= (LENGTH *EXCELDATA@) ROW#)(>= (LENGTH (NTH 0 *EXCELDATA@)) COLUMN#))
      (SETQ RETURN (NTH COLUMN# (NTH ROW# *EXCELDATA@)))
    );IF
  );IF
  RETURN
);DEFUN GETCELL
;-------------------------------------------------------------------------------
; OPENEXCEL - OPENS AN EXCEL SPREADSHEET
; ARGUMENTS: 3
;   EXCELFILE$ = EXCEL FILENAME OR NIL FOR NEW SPREADSHEET
;   SHEETNAME$ = SHEET NAME OR NIL FOR NOT SPECIFIED
;   VISIBLE = T FOR VISIBLE OR NIL FOR HIDDEN
; SYNTAX EXAMPLES:
; (OPENEXCEL "C:\\TEMP\\TEMP.XLS" "SHEET2" T) = OPENS C:\TEMP\TEMP.XLS ON SHEET2 AS VISIBLE SESSION
; (OPENEXCEL "C:\\TEMP\\TEMP.XLS" NIL NIL) = OPENS C:\TEMP\TEMP.XLS ON CURRENT SHEET AS HIDDEN SESSION
; (OPENEXCEL NIL "PARTS LIST" NIL) =  OPENS A NEW SPREADSHEET AND CREATES A PART LIST SHEET AS HIDDEN SESSION
;-------------------------------------------------------------------------------
(DEFUN OPENEXCEL (EXCELFILE$ SHEETNAME$ VISIBLE / SHEET$ SHEETS@ WORKSHEET)
  (IF (= (TYPE EXCELFILE$) 'STR)
    (IF (FINDFILE EXCELFILE$)
      (SETQ *EXCELFILE$ EXCELFILE$)
      (PROGN
        (ALERT (STRCAT "EXCEL FILE " EXCELFILE$ " NOT FOUND."))
        (EXIT)
      );PROGN
    );IF
    (SETQ *EXCELFILE$ "")
  );IF
  (GC)
  (IF (SETQ *EXCELAPP% (VLAX-GET-OBJECT "EXCEL.APPLICATION"))
    (PROGN
      (ALERT "CLOSE ALL EXCEL SPREADSHEETS TO CONTINUE!")
      (VLAX-RELEASE-OBJECT *EXCELAPP%)(GC)
    );PROGN
  );IF
  (SETQ *EXCELAPP% (VLAX-GET-OR-CREATE-OBJECT "EXCEL.APPLICATION"))
  (IF EXCELFILE$
    (IF (FINDFILE EXCELFILE$)
      (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% 'WORKBOOKS) 'OPEN EXCELFILE$)
      (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% 'WORKBOOKS) 'ADD)
    );IF
    (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% 'WORKBOOKS) 'ADD)
  );IF
  (IF VISIBLE
    (VLA-PUT-VISIBLE *EXCELAPP% :VLAX-TRUE)
  );IF
  (IF (= (TYPE SHEETNAME$) 'STR)
    (PROGN
      (VLAX-FOR SHEET$ (VLAX-GET-PROPERTY *EXCELAPP% "SHEETS")
        (SETQ SHEETS@ (APPEND SHEETS@ (LIST (VLAX-GET-PROPERTY SHEET$ "NAME"))))
      );VLAX-FOR
      (IF (MEMBER SHEETNAME$ SHEETS@)
        (VLAX-FOR WORKSHEET (VLAX-GET-PROPERTY *EXCELAPP% "SHEETS")
          (IF (= (VLAX-GET-PROPERTY WORKSHEET "NAME") SHEETNAME$)
            (VLAX-INVOKE-METHOD WORKSHEET "ACTIVATE")
          );IF
        );VLAX-FOR
        (VLAX-PUT-PROPERTY (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% "SHEETS") "ADD") "NAME" SHEETNAME$)
      );IF
    );PROGN
  );IF
  (PRINC)
);DEFUN OPENEXCEL
;-------------------------------------------------------------------------------
; PUTCELL - PUT VALUES INTO EXCEL CELLS
; ARGUMENTS: 2
;   STARTCELL$ = STARTING CELL ID
;   DATA@ = VALUE OR LIST OF VALUES
; SYNTAX EXAMPLES:
; (PUTCELL "A1" "PART NUMBER") = PUTS PART NUMBER IN CELL A1
; (PUTCELL "B3" '("DIM" 7.5 "9.75")) = STARTING WITH CELL B3 PUT DIM, 7.5, AND 9.75 ACROSS
;-------------------------------------------------------------------------------
(DEFUN PUTCELL (STARTCELL$ DATA@ / CELL$ COLUMN# EXCELRANGE ROW#)
  (IF (= (TYPE DATA@) 'STR)
    (SETQ DATA@ (LIST DATA@))
  )
  (SETQ EXCELRANGE (VLAX-GET-PROPERTY *EXCELAPP% "CELLS"))
  (IF (CELL-P STARTCELL$)
    (SETQ COLUMN# (CAR (COLUMNROW STARTCELL$))
          ROW# (CADR (COLUMNROW STARTCELL$))
    );SETQ
    (IF (VL-CATCH-ALL-ERROR-P
          (SETQ CELL$ (VL-CATCH-ALL-APPLY 'VLAX-GET-PROPERTY
            (LIST (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVESHEET") "RANGE" STARTCELL$))
          );SETQ
        );VL-CATCH-ALL-ERROR-P
        (ALERT (STRCAT "THE CELL ID \"" STARTCELL$ "\" IS INVALID."))
        (SETQ COLUMN# (VLAX-GET-PROPERTY CELL$ "COLUMN")
              ROW# (VLAX-GET-PROPERTY CELL$ "ROW")
        );SETQ
    );IF
  );IF
  (IF (AND COLUMN# ROW#)
    (FOREACH ITEM DATA@
      (VLAX-PUT-PROPERTY EXCELRANGE "ITEM" ROW# COLUMN# (VL-PRINC-TO-STRING ITEM))
      (SETQ ROW# (1+ ROW#))
    );FOREACH
  );IF
  (PRINC)
);DEFUN PUTCELL
;-------------------------------------------------------------------------------
; CLOSEEXCEL - CLOSES EXCEL SPREADSHEET
; ARGUMENTS: 1
;   EXCELFILE$ = EXCEL SAVEAS FILENAME OR NIL TO CLOSE WITHOUT SAVING
; SYNTAX EXAMPLES:
; (CLOSEEXCEL "C:\\TEMP\\TEMP.XLS") = SAVEAS C:\TEMP\TEMP.XLS AND CLOSE
; (CLOSEEXCEL NIL) = CLOSE WITHOUT SAVING
;-------------------------------------------------------------------------------
(DEFUN CLOSEEXCEL (EXCELFILE$ / SAVEAS)
  (IF EXCELFILE$
    (IF (= (STRCASE EXCELFILE$) (STRCASE *EXCELFILE$))
      (IF (FINDFILE EXCELFILE$)
        (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVEWORKBOOK") "SAVE")
        (SETQ SAVEAS T)
      );IF
      (IF (FINDFILE EXCELFILE$)
        (PROGN
          (VL-FILE-DELETE (FINDFILE EXCELFILE$))
          (SETQ SAVEAS T)
        );PROGN
        (SETQ SAVEAS T)
      );IF
    );IF
  );IF
  (IF SAVEAS
    (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVEWORKBOOK")
      "SAVEAS" EXCELFILE$ -4143 "" "" :VLAX-FALSE :VLAX-FALSE NIL
    );VLAX-INVOKE-METHOD
  );IF
  (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVEWORKBOOK") 'CLOSE :VLAX-FALSE)
  (VLAX-INVOKE-METHOD *EXCELAPP% 'QUIT)
  (VLAX-RELEASE-OBJECT *EXCELAPP%)(GC)
  (SETQ *EXCELAPP% NIL *EXCELFILE$ NIL)
  (PRINC)
);DEFUN CLOSEEXCEL
;-------------------------------------------------------------------------------
; COLUMNROW - RETURNS A LIST OF THE COLUMN AND ROW NUMBER
; FUNCTION BY: GILLES CHANTEAU FROM MARSEILLE, FRANCE
; ARGUMENTS: 1
;   CELL$ = CELL ID
; SYNTAX EXAMPLE: (COLUMNROW "ABC987") = '(731 987)
;-------------------------------------------------------------------------------
(DEFUN COLUMNROW (CELL$ / COLUMN$ CHAR$ ROW#)
  (SETQ COLUMN$ "")
  (WHILE (< 64 (ASCII (SETQ CHAR$ (STRCASE (SUBSTR CELL$ 1 1)))) 91)
    (SETQ COLUMN$ (STRCAT COLUMN$ CHAR$)
          CELL$ (SUBSTR CELL$ 2)
    );SETQ
  );WHILE
  (IF (AND (/= COLUMN$ "") (NUMBERP (SETQ ROW# (READ CELL$))))
    (LIST (ALPHA2NUMBER COLUMN$) ROW#)
    '(1 1);DEFAULT TO "A1" IF THERE'S A PROBLEM
  );IF
);DEFUN COLUMNROW
;-------------------------------------------------------------------------------
; ALPHA2NUMBER - CONVERTS ALPHA STRING INTO NUMBER
; FUNCTION BY: GILLES CHANTEAU FROM MARSEILLE, FRANCE
; ARGUMENTS: 1
;   STR$ = STRING TO CONVERT
; SYNTAX EXAMPLE: (ALPHA2NUMBER "ABC") = 731
;-------------------------------------------------------------------------------
(DEFUN ALPHA2NUMBER (STR$ / NUM#)
  (IF (= 0 (SETQ NUM# (STRLEN STR$)))
    0
    (+ (* (- (ASCII (STRCASE (SUBSTR STR$ 1 1))) 64) (EXPT 26 (1- NUM#)))
       (ALPHA2NUMBER (SUBSTR STR$ 2))
    );+
  );IF
);DEFUN ALPHA2NUMBER
;-------------------------------------------------------------------------------
; NUMBER2ALPHA - CONVERTS NUMBER INTO ALPHA STRING
; FUNCTION BY: GILLES CHANTEAU FROM MARSEILLE, FRANCE
; ARGUMENTS: 1
;   NUM# = NUMBER TO CONVERT
; SYNTAX EXAMPLE: (NUMBER2ALPHA 731) = "ABC"
;-------------------------------------------------------------------------------
(DEFUN NUMBER2ALPHA (NUM# / VAL#)
  (IF (< NUM# 27)
    (CHR (+ 64 NUM#))
    (IF (= 0 (SETQ VAL# (REM NUM# 26)))
      (STRCAT (NUMBER2ALPHA (1- (/ NUM# 26))) "Z")
      (STRCAT (NUMBER2ALPHA (/ NUM# 26)) (CHR (+ 64 VAL#)))
    );IF
  );IF
);DEFUN NUMBER2ALPHA
;-------------------------------------------------------------------------------
; CELL-P - EVALUATES IF THE ARGUMENT CELL$ IS A VALID CELL ID
; ARGUMENTS: 1
;   CELL$ = STRING OF THE CELL ID TO EVALUATE
; SYNTAX EXAMPLES: (CELL-P "B12") = T, (CELL-P "BT") = NIL
;-------------------------------------------------------------------------------
(DEFUN CELL-P (CELL$)
  (AND (= (TYPE CELL$) 'STR)
    (OR (= (STRCASE CELL$) "A1")
      (NOT (EQUAL (COLUMNROW CELL$) '(1 1)))
    );OR
  );AND
);DEFUN CELL-P
;-------------------------------------------------------------------------------
; ROW+N - RETURNS THE CELL ID LOCATED A NUMBER OF ROWS FROM CELL
; FUNCTION BY: GILLES CHANTEAU FROM MARSEILLE, FRANCE
; ARGUMENTS: 2
;   CELL$ = STARTING CELL ID
;   NUM# = NUMBER OF ROWS FROM CELL
; SYNTAX EXAMPLES: (ROW+N "B12" 3) = "B15", (ROW+N "B12" -3) = "B9"
;-------------------------------------------------------------------------------
(DEFUN ROW+N (CELL$ NUM#)
  (SETQ CELL$ (COLUMNROW CELL$))
  (STRCAT (NUMBER2ALPHA (CAR CELL$)) (ITOA (MAX 1 (+ (CADR CELL$) NUM#))))
);DEFUN ROW+N
;-------------------------------------------------------------------------------
; COLUMN+N - RETURNS THE CELL ID LOCATED A NUMBER OF COLUMNS FROM CELL
; FUNCTION BY: GILLES CHANTEAU FROM MARSEILLE, FRANCE
; ARGUMENTS: 2
;   CELL$ = STARTING CELL ID
;   NUM# = NUMBER OF COLUMNS FROM CELL
; SYNTAX EXAMPLES: (COLUMN+N "B12" 3) = "E12", (COLUMN+N "B12" -1) = "A12"
;-------------------------------------------------------------------------------
(DEFUN COLUMN+N (CELL$ NUM#)
  (SETQ CELL$ (COLUMNROW CELL$))
  (STRCAT (NUMBER2ALPHA (MAX 1 (+ (CAR CELL$) NUM#))) (ITOA (CADR CELL$)))
);DEFUN COLUMN+N
;-------------------------------------------------------------------------------
; RTOSR - USED TO CHANGE A REAL NUMBER INTO A SHORT REAL NUMBER STRING
; STRIPPING OFF ALL TRAILING 0'S.
; ARGUMENTS: 1
;   REALNUM~ = REAL NUMBER TO CONVERT TO A SHORT STRING REAL NUMBER
; RETURNS: SHORTREAL$ THE SHORT STRING REAL NUMBER VALUE OF THE REAL NUMBER.
;-------------------------------------------------------------------------------
(DEFUN RTOSR (REALNUM~ / DIMZIN# SHORTREAL$)
  (SETQ DIMZIN# (GETVAR "DIMZIN"))
  (SETVAR "DIMZIN" 8)
  (SETQ SHORTREAL$ (RTOS REALNUM~ 2 8))
  (SETVAR "DIMZIN" DIMZIN#)
  SHORTREAL$
);DEFUN RTOSR
;-------------------------------------------------------------------------------
(PRINC);END OF GETEXCEL.LSP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






					;***********************************************************************************************************************************************************************************************************;
					;                                                                      END OF DATA MANIPULATION FUNCTIONS FOR DATA EXTRACTED FROM LOP BLOCKS                                                                ;
					;***********************************************************************************************************************************************************************************************************;






					;###########################################################################################################################################################################################################;
					;                                                        START OF DATA DUMPING FUNCTIONS INTO EXCEL                                                                                                         ;
					;###########################################################################################################################################################################################################;



(DEFUN PUTCELLSHEET (SHEETNAME	STARTCELL$ DATA@      /
		     CELL$	COLUMN#	   EXCELRANGE ROW#
		     mySheet
		    )
  (IF (= (TYPE DATA@) 'STR)
    (SETQ DATA@ (LIST DATA@))
  )
  (setq mybook *EXCELAPP%)
  (setq	mySheet	(vl-catch-all-apply
		  'vlax-get-property
		  (list	(vlax-get-property myBook "Sheets")
			"Item"
			sheetName
		  )
		)
  )
  (vlax-invoke-method mySheet "Activate")
  (SETQ EXCELRANGE (VLAX-GET-PROPERTY *EXCELAPP% "CELLS"))
  (IF (CELL-P STARTCELL$)
    (SETQ COLUMN# (CAR (COLUMNROW STARTCELL$))
	  ROW#	  (CADR (COLUMNROW STARTCELL$))
    )					;SETQ
    (IF	(VL-CATCH-ALL-ERROR-P
	  (SETQ
	    CELL$ (VL-CATCH-ALL-APPLY
		    'VLAX-GET-PROPERTY
		    (LIST (VLAX-GET-PROPERTY *EXCELAPP% "ACTIVESHEET")
			  "RANGE"
			  STARTCELL$
		    )
		  )
	  )				;SETQ
	)				;VL-CATCH-ALL-ERROR-P
      (ALERT (STRCAT "THE CELL ID \"" STARTCELL$ "\" IS INVALID.")
      )
      (SETQ COLUMN# (VLAX-GET-PROPERTY CELL$ "COLUMN")
	    ROW#    (VLAX-GET-PROPERTY CELL$ "ROW")
      )					;SETQ
    )					;IF
  )					;IF
  (IF (AND COLUMN# ROW#)
    (FOREACH ITEM DATA@
      (VLAX-PUT-PROPERTY
	EXCELRANGE
	"ITEM"
	ROW#
	COLUMN#
	(VL-PRINC-TO-STRING ITEM)
      )
      (SETQ ROW# (1+ ROW#))
    )					;FOREACH
  )					;IF
  (PRINC)
)



(defun putCellsrow/column (sheetName  cellName	 list1	    FLAG
			   /	      myXL	 myBook	    mySheet
			   myRange    cellValue
			  )
  (setq mybook *EXCELAPP%)
  (setq	mySheet	(vl-catch-all-apply
		  'vlax-get-property
		  (list	(vlax-get-property myBook "Sheets")
			"Item"
			sheetName
		  )
		)
  )
  (vlax-invoke-method mySheet "Activate")
  (setq	myRange	(vlax-get-property
		  (vlax-get-property mySheet 'Cells)
		  "Range"
		  cellName
		)
  )
  (vlax-put-property
    myRange
    'Value2
    (MAKE_SAFEARRAY_STR list1 FLAG)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



					;###########################################################################################################################################################################################################;
					;                                                        START OF DATA EXTRACTION FUNCTIONS FROM EXCEL                                                                                                      ;
					;###########################################################################################################################################################################################################;


(defun getCellsrow/column
       (sheetName cellName / myXL myBook mySheet myRange cellValue)
  (setq mybook *EXCELAPP%)
  (setq	mySheet	(vl-catch-all-apply
		  'vlax-get-property
		  (list	(vlax-get-property myBook "Sheets")
			"Item"
			sheetName
		  )
		)
  )
  (vlax-invoke-method mySheet "Activate")
  (setq	myRange	(vlax-get-property
		  (vlax-get-property mySheet 'Cells)
		  "Range"
		  cellName
		)
  )
  (setq
    cellValue (vlax-variant-value (vlax-get-property myRange 'Value2))
  )


  (if (= 'safearray (type cellValue))
    (progn
      (setq tempCellValue (vlax-safearray->list cellValue))
      (setq cellValue (list))
      (if (= (length tempCellValue) 1)
	(progn
	  (foreach a tempCellValue
	    (if	(= (type a) 'LIST)
	      (progn
		(foreach b a
		  (if (= (type b) 'LIST)
		    (setq
		      cellValue	(append
				  cellValue
				  (list (vlax-variant-value (car b)))
				)
		    )
		    (setq
		      cellValue	(append	cellValue
					(list (vlax-variant-value b))
				)
		    )
		  )
		)
	      )
	      (setq cellValue (append cellValue
				      (list (vlax-variant-value a))
			      )
	      )
	    )
	  )
	)
	(progn
	  (foreach a tempCellValue
	    (setq tmpList (list))
	    (foreach b a
	      (setq tmp (vlax-variant-value b))
	      (setq tmpList (append tmpList (list tmp)))
	    )
	    (setq cellValue (append cellValue tmpList))
	  )
	)
      )
    )
  )
  cellValue
)

					;***********************************************************************************************************************************************************************************************************;
					;                                                                      END OF DATA EXTRACTION FUNCTIONS FROM EXCEL                                                                                          ;
					;***********************************************************************************************************************************************************************************************************;
                                                 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN BUILD_ENAME_LIST(WPT1 WPT2 / ENTITY_NAME_LIST  EFFECTIVE_NAME_LIST ENTITY_EFFECTIVE_NAME_LIST I STATUS ENAME FINAL_LIST)
  (SETQ ENTITY_NAME_LIST (SINGLE_ELE_LIST (SORT_FUN (DATA_COLLECT WPT1 WPT2 (LIST (CONS 0 "INSERT")) -1) 0 1)  0))
  (SETQ EFFECTIVE_NAME_LIST (SORT_FUN (DATA_COLLECT WPT1 WPT2 (LIST (CONS 0 "INSERT")) 2) 0 1))
  (SETQ ENTITY_EFFECTIVE_NAME_LIST (FILTER_LIST '("ANCHOR" "ACA") (ADD_LISTS ENTITY_NAME_LIST EFFECTIVE_NAME_LIST) 1))
  (SETQ I 0)
  (SETQ STATUS NIL)
  (WHILE (< I (LENGTH ENTITY_EFFECTIVE_NAME_LIST))
    (SETQ ENAME (CDR( ASSOC 2 (ENTGET (CAR (NTH I ENTITY_EFFECTIVE_NAME_LIST))))))
    (SETQ SUB_ENT_LIST (FILTER_LIST '("LWPOLYLINE" "LINE") (SORT_FUN (Block_Ename ENAME) 0 2)  0))
    (IF (= (CAR (CAR SUB_ENT_LIST)) "LWPOLYLINE") (SETQ STATUS(CONS "START" STATUS)) (SETQ STATUS(CONS "END" STATUS)))
    (SETQ I (+ I 1))
    )
  (SETQ STATUS (REVERSE STATUS))
  (SETQ FINAL_LIST (ADD_LISTS STATUS ENTITY_EFFECTIVE_NAME_LIST))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN BUILD_LIST (LIST1 LIST2 / I J LIST3 TEMP_ELE TEMP_ELE1)
  (SETQ I 0)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH I LIST1))
    (SETQ J 0)
    (SETQ TEMP_ELE1 NIL)
    (WHILE (< J (LENGTH LIST2))
      (SETQ TEMP_ELE1 (CONS (NTH (NTH J LIST2) TEMP_ELE) TEMP_ELE1))
      (SETQ J (+ J 1))
    )
    (SETQ TEMP_ELE1(REVERSE TEMP_ELE1))
    (SETQ LIST3 (CONS TEMP_ELE1 LIST3))
    (SETQ I (+ I 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun	Block_Ename (blockName / ename SUB_ENT_LIST I TEMP_ELE LIST2)
   (if (setq ename (tblobjname "block" blockName))
     (reverse
       (while (setq ename (entnext ename))
	 (setq SUB_ENT_LIST (cons ename SUB_ENT_LIST))
       )
     )
   )
   (SETQ I 0)
   (SETQ LIST2 NIL)
   (WHILE (< I (LENGTH SUB_ENT_LIST))
     (SETQ TEMP_LIST
	    (FILTER_LIST '(10 11) (ENTGET (NTH I SUB_ENT_LIST)) 0)
     )
     (SETQ TEMP_ELE (ADD_LISTS (N_ELE_LIST (CDR(ASSOC 0 (ENTGET (NTH I SUB_ENT_LIST)))) (LENGTH TEMP_LIST)) TEMP_LIST))
     (SETQ LIST2 (APPEND LIST2 TEMP_ELE))
     (SETQ I (+ I 1))
   )
   (SETQ LIST2 (REVERSE LIST2))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;****************************************GENERAL PROGRAM**************************************************************************************************************************************;
;;;;;;;;;;PROGRAM TO RETREIVE PARTICULAR ATTRIBUTE VALUES FROM A LIST OF ENTITIES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;******************************FLAG INDICATES THE POSITION OF ENTITIES IN THE ENTNAME_LIST IN EACH ELEMENT OF THE LIST************************************************************************;
;OUTPUT LIST-----> (LIST (CONS (NTH 0 ENTNAME_LIST) ATTRIBUTE_VALUE) (CONS (NTH 1 ENTNAME_LIST) ATTRIBUTE_VALUE) (CONS (NTH 2 ENTNAME_LIST) ATTRIBUTE_VALUE).............*********************;
;EG:- (ATTRIBUTES_FROM_ENTITIES ENTNAME_LIST "SPAN1" 0)***************************************************************************************************************************************;
;****************************************GENERAL PROGRAM**************************************************************************************************************************************;
(DEFUN ATTRIBUTES_FROM_ENTITIES (ENTNAME_LIST ATTRIBUTE_TAG_NAME FLAG / I LIST1 ENTNAME_LIST TEMP_ELE SAFEARRAY_SET ENT_OBJECT J)
  (SETQ I 0)
  (SETQ LIST1 NIL)

(WHILE (< I (LENGTH ENTNAME_LIST))
  (SETQ TEMP_ELE NIL)
  (SETQ SAFEARRAY_SET NIL) 
  (IF (NOT (ATOM (NTH I ENTNAME_LIST))) (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT (NTH FLAG (NTH I ENTNAME_LIST)))) (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT (NTH I ENTNAME_LIST))))

(IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
    (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

(SETQ J 0)
(SETQ TEMP_ELE NIL)

(WHILE (< J (LENGTH SAFEARRAY_SET))
  (IF (= (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TAGSTRING") ATTRIBUTE_TAG_NAME)
  (PROGN (IF (ATOM (NTH I ENTNAME_LIST))
    (SETQ TEMP_ELE (LIST (NTH I ENTNAME_LIST) (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TEXTSTRING")))
    (SETQ TEMP_ELE (APPEND (NTH I ENTNAME_LIST) (LIST (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TEXTSTRING"))))
  )))
    
    (SETQ J (+ J 1))

)))
(SETQ LIST1 (CONS TEMP_ELE LIST1))
(SETQ I (+ I 1))

)
(REVERSE LIST1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;**************************************************************GENERAL PROGRAM**************************************************************************************************************************************;
;************************************************REMOVES NTH ELEMENT OF SUB ELEMENTS OF A LIST**********************************************************************************************************************;
;EG1: (REMOVE_NTH_ELE '((1 2 3) (2 3 4) (4 5 6)) 0)-------> OUTPUT='((2 3) (3 4) (5 6))*****************************************************************************************************************************;
;EG2: (REMOVE_NTH_ELE '((1 2 3) (2 3) (4 5 6)) 2)---------> OUTPUT='((1 2)(2 3)(4 5))*******************************************************************************************************************************:
;EG3: (REMOVE_NTH_ELE '((1 2 3) (2 3) 4)) 2)---------> OUTPUT='( (1 2) (2 3) 4)*************************************************************************************************************************************:
(DEFUN REMOVE_NTH_ELE (LIST1 ELE_NUMBER / I TEMP_ELE LIST2)
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH I LIST1))
    (IF (AND (NOT (ATOM TEMP_ELE)) (/= TEMP_ELE NIL))
    (PROGN
    (SETQ TEMP_ELE (VL-REMOVE (NTH ELE_NUMBER TEMP_ELE) TEMP_ELE))
    ))
    (SETQ LIST2 (CONS TEMP_ELE LIST2))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;**************************************************************GENERAL PROGRAM**************************************************************************************************************************************;
;__________________________________________________________COMBINES TWO SINGLE ELEMENT LISTS WITHOUT FORMING DOTTED PAIRS___________________________________________________________________________________________;
;______EG: (ADD_LISTS2 '(1 2 3) '(2 3 4))----------------->OUTPUT==> '((1 2) (2 3) (3 4))___________________________________________________________________________________________________________________________;
(DEFUN ADD_LISTS2 (LIST1 LIST2 / I TEMP_ELE LIST3)
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (LIST (NTH I LIST1) (NTH I LIST2)))
    (SETQ LIST3 (CONS TEMP_ELE LIST3))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST3)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;












;**************************************************************GENERAL PROGRAM*****************************************************************************;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;COMPLEMENT FUNCTION OF FILTER LIST FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;FILTERS A GIVEN LIST BASED ON THE ELEMENTS IN OTHER GIVEN LIST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;USAGE : LIST1----> '(A B C) ; LIST2 -----> '((A D) (C D) (E B) (E C)) ; POSITION = 0 ===> OUTPUT = '((E B) (E C))
                                                                             ; POSITION = 1 ===> OUTPUT = '((A D) (C D))
(DEFUN FILTER_LIST1 (LIST1 LIST2 POSITION / I LOOP_ELE TEMP_ELE LIST3)
  (SETQ I 0)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST2))
    (SETQ LOOP_ELE (NTH POSITION (NTH I LIST2)))
    (IF	(= (VL-POSITION LOOP_ELE LIST1) NIL)
      (PROGN (SETQ TEMP_ELE (NTH I LIST2))
	     (SETQ LIST3 (CONS TEMP_ELE LIST3))
      )
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                               ;               SUPPORTING/GENERAL FUNCTIONS-3                              
                                               ;FUNCTION FOR EXTRACTING EFFECTIVE NAME WITH LIST OF ENTITIY NAMES AS INPUTS
                                               ;               CANNOT BE USED AS GENERAL FUNCTION                          

;USAGE:  (EXTRACT_ENTITY_INFO2 ENTITY_NAME_LIST)                                           
;OUTPUT: ((ENTITY_NAME1 EFFECTIVE_NAME1) (ENTITY_NAME1 EFFECTIVE_NAME1).......)            
;ONLY LIST OF BLOCK ENTITY NAMES SHOULD BE PASSED AS AURGUMENTS...UNLESS RETURNS ERROR     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN EXTRACT_ENTITY_INFO2 (LIST1 / I TEMP_ELE LIST2 )

  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (IF (AND (AND (/= (NTH I LIST1) NIL) (VLAX-PROPERTY-AVAILABLE-P (VLAX-ENAME->VLA-OBJECT (NTH I LIST1)) 'EFFECTIVENAME)) (/= (ENTGET (NTH I LIST1)) NIL)) (SETQ TEMP_ELE (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT (NTH I LIST1)) "EFFECTIVENAME")) (SETQ TEMP_ELE NIL))
    (SETQ LIST2 (CONS (LIST (NTH I LIST1) TEMP_ELE) LIST2))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST2)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









;****************************************************CHAINAGE UPDATE FUNCTION********************************************************************************;



(DEFUN C:UPDATE_CHAINAGES ( / SSSET LOC_NO ST_CH ENT_LIST BS_PT ENT_INFO VALUES VALUES1 VALUES2 VALUES3 VALUES4 FDN_REFERENCE FDN_REFERENCE_X FDN_REFERENCE_Y RD_REFERENCE RD_REFERENCE_X RD_REFERENCE_Y TC_REFERENCE TC_REFERENCE_X TC_REFERENCE_Y SPAN_REFERENCE_X SPAN_REFERENCE_Y ENT_LIST_SPAN POINT POINT1 POINT2 POINT3 ENT_LIST_SPAN VISIBILITY_SPAN VISIBILITY_TC VISIBILITY_FDN VISIBILITY_RD I LOC CH TEMP_ELE SCALE POINT_FDN POINT_RD POINT_TC TEMP_SPAN)
  (PROMPT "\n SELECT MASTS")
  (SETQ SSSET (SSGET))
  (SETQ SCALE (GETREAL "\n ENTER SCALE : "))
  (SETQ LOC_NO (GETSTRING "\n ENTER STARTING MAST LOCATION NUMBER :"))
  (SETQ ST_CH (GETSTRING "\n ENTER STARTING MAST CHAINAGE :"))
  (SETQ	KM_NO (SUBSTR LOC_NO 1 (VL-STRING-POSITION (ASCII "/") LOC_NO)))
  (SETQ	LOC_NO	(SUBSTR	LOC_NO (+ (VL-STRING-POSITION (ASCII "/") LOC_NO) 2)))
  (SETQ	ST_CH	(SUBSTR	ST_CH (+ (VL-STRING-POSITION (ASCII "/") ST_CH) 2)))
  
  (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST") (EXTRACT_ENTITY_INFO2 (FORM_SSSET SSSET)) 1) 0))
  (SETQ ENT_LIST (EXTRACT_ENTITY_INFO ENT_LIST 1 3))
  (SETQ ENT_LIST (SORT_FUN ENT_LIST 1 0))
  (SETQ ENT_LIST (SINGLE_ELE_LIST ENT_LIST 0))
  (SETQ BS_PT (CDR (ASSOC 10 (ENTGET (NTH 0 ENT_LIST)))))
  (SETQ ENT_INFO (YARD_STRUCTURE_INFO (NTH 0 ENT_LIST)))
  (SETQ VALUES (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LOM1 X" "LOM1 Y")))
  (SETQ VALUES1 (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LNM1 X" "LNM1 Y")))
  (SETQ VALUES2 (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LCM1 X" "LCM1 Y")))
  (SETQ VALUES3 (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LTM1 X" "LTM1 Y")))
  (SETQ FDN_REFERENCE (NTH 1 (NTH 0 (GET_TEXT_ALIGNMENT_POINT (LIST (NTH 0 ENT_LIST)) "FOUNDATION_TYPE1" 0))))
  (SETQ RD_REFERENCE (NTH 1 (NTH 0 (GET_TEXT_ALIGNMENT_POINT (LIST (NTH 0 ENT_LIST)) "RD1" 0))))
  (SETQ POINT (LIST (+ (CAR BS_PT) (CADR (ASSOC "LOM1 X" VALUES))) (+ (CADR BS_PT) (CADR (ASSOC "LOM1 Y" VALUES))) 0.0))
  (SETQ POINT1 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LNM1 X" VALUES1))) (+ (CADR BS_PT) (CADR (ASSOC "LNM1 Y" VALUES1))) 0.0))
  (SETQ POINT2 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LCM1 X" VALUES2))) (+ (CADR BS_PT) (CADR (ASSOC "LCM1 Y" VALUES2))) 0.0))
  (SETQ POINT3 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LTM1 X" VALUES3))) (+ (CADR BS_PT) (CADR (ASSOC "LTM1 Y" VALUES3))) 0.0))
  (SETQ ENT_LIST_SPAN (ATTRIBUTES_FROM_ENTITIES ENT_LIST "SPAN1" 0))
  (SETQ I 0)
   (setq dcl_id (load_dialog "DISPLAY_INFO.dcl"))
   (new_dialog "DISPLAY_INFO" dcl_id)
  (WHILE (< I (LENGTH ENT_LIST_SPAN))
    (SETQ LOC (STRCAT KM_NO "/" LOC_NO))
    (SETQ CH (STRCAT KM_NO "/" ST_CH))
     (SET_TILE "INFO" (STRCAT "UPDATING LOC NO "  LOC " " "OF "  (ITOA I)))
     (SET_TILE "PERCENTAGE" (STRCAT "UPDATING CHAINAGE " CH))
    (IF (= I (- (LENGTH ENT_LIST_SPAN) 1))
      (PROGN
      (SET_TILE "INFO" (STRCAT "ALL "  (ITOA (+ I 1)) " LOCATIONS ARE UPDATED SUCESSFULLY"))
      (SET_TILE "PERCENTAGE" "PRESS OK TO VIEW RESULTS")
      )
    )
    
    (SETQ TEMP_ELE  (NTH I ENT_LIST_SPAN))
    (SETQ BASE_POINT (CDR (ASSOC 10 (ENTGET (NTH 0 TEMP_ELE)))))
    (MODIFY_ATTRIBUTES (NTH 0 TEMP_ELE) (LIST "MAST_NUMBER1" "CHAINAGE1") (LIST LOC CH))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LOM1") (LIST POINT))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LNM1") (LIST POINT1))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LCM1") (LIST POINT2))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LTM1") (LIST POINT3))
    (IF (/= I 0)
    (PROGN (CHANGE_TEXT_ALIGNMENT_POINT (NTH I ENT_LIST) '("FOUNDATION_TYPE1") (LIST FDN_REFERENCE))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH I ENT_LIST) '("RD1") (LIST RD_REFERENCE))
    )
    )
    (SETQ TEMP_SPAN (* (ATOF (NTH 1 TEMP_ELE)) SCALE))
    (IF (/= TEMP_SPAN 0)
    (PROGN
    (SETQ POINT (LIST (+ (CAR POINT) TEMP_SPAN) (CADR POINT) 0.0))
    (SETQ POINT1 (LIST (+ (CAR POINT1) TEMP_SPAN) (CADR POINT1) 0.0))
    (SETQ POINT2 (LIST (+ (CAR POINT2) TEMP_SPAN) (CADR POINT2) 0.0))
    (SETQ POINT3 (LIST (+ (CAR POINT3) TEMP_SPAN) (CADR POINT3) 0.0))
    (SETQ FDN_REFERENCE (LIST (+ (CAR FDN_REFERENCE) TEMP_SPAN)  (CADR FDN_REFERENCE)  0.0))
    (SETQ RD_REFERENCE  (LIST (+ (CAR RD_REFERENCE) TEMP_SPAN)  (CADR RD_REFERENCE)  0.0))
    )
    (PROGN
      (IF (/= I (- (LENGTH ENT_LIST_SPAN) 1)) (PROGN (SET_TILE "INFO" (STRCAT "SPAN HAS NOT BEEN ENTERED IN LOC NO "  LOC   ))
     (SET_TILE "PERCENTAGE" (STRCAT "PROGRAM STOPPED AT CHAINAGE " CH " PLEASE CHECK..."))))
      (SETQ I (LENGTH ENT_LIST_SPAN))
    ))
    (SETQ LOC_NO (ITOA (+ (ATOI LOC_NO) 2)))
    (SETQ ST_CH (RTOS (+ (ATOF ST_CH) (ATOF (NTH 1 TEMP_ELE))) 2 2))
    (SETQ I (+ I 1))
    )(setq ddiag(start_dialog))
)

(DEFUN C:UPDATE_CHAINAGES_SS ( / SSSET LOC_NO ST_CH ENT_LIST BS_PT ENT_INFO VALUES VALUES1 VALUES2 VALUES3 VALUES4 FDN_REFERENCE FDN_REFERENCE_X FDN_REFERENCE_Y RD_REFERENCE RD_REFERENCE_X RD_REFERENCE_Y TC_REFERENCE TC_REFERENCE_X TC_REFERENCE_Y SPAN_REFERENCE_X SPAN_REFERENCE_Y ENT_LIST_SPAN POINT POINT1 POINT2 POINT3 ENT_LIST_SPAN VISIBILITY_SPAN VISIBILITY_TC VISIBILITY_FDN VISIBILITY_RD I LOC CH TEMP_ELE SCALE POINT_FDN POINT_RD POINT_TC TEMP_SPAN)
  (PROMPT "\n SELECT LOOP LINE MASTS")
  (SETQ SSSET (SSGET))
  (SETQ SCALE (GETREAL "\n ENTER SCALE : "))
  (SETQ LOC_NO (GETSTRING "\n ENTER STARTING MAST LOCATION NUMBER :"))
  (SETQ ST_CH (GETSTRING "\n ENTER STARTING MAST CHAINAGE :"))
  (SETQ	KM_NO (SUBSTR LOC_NO 1 (VL-STRING-POSITION (ASCII "/") LOC_NO)))
  (SETQ	LOC_NO	(SUBSTR	LOC_NO (+ (VL-STRING-POSITION (ASCII "/") LOC_NO) 2)))
  (SETQ	ST_CH	(SUBSTR	ST_CH (+ (VL-STRING-POSITION (ASCII "/") ST_CH) 2)))
  
  (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "SS0" "SS1" "SS2" "SS3") (EXTRACT_ENTITY_INFO2 (FORM_SSSET SSSET)) 1) 0))
  (SETQ ENT_LIST (EXTRACT_ENTITY_INFO ENT_LIST 1 3))
  (SETQ ENT_LIST (SORT_FUN ENT_LIST 1 0))
  (SETQ ENT_LIST (SINGLE_ELE_LIST ENT_LIST 0))
  (SETQ BS_PT (CDR (ASSOC 10 (ENTGET (NTH 0 ENT_LIST)))))
  (SETQ ENT_INFO (YARD_STRUCTURE_INFO (NTH 0 ENT_LIST)))
  
  (SETQ ENT_LIST_SPAN (ATTRIBUTES_FROM_ENTITIES ENT_LIST "SPAN1" 0))
  (SETQ I 0)
   (setq dcl_id (load_dialog "DISPLAY_INFO.dcl"))
   (new_dialog "DISPLAY_INFO" dcl_id)
  (WHILE (< I (LENGTH ENT_LIST_SPAN))
    (SETQ LOC (STRCAT KM_NO "/" LOC_NO))
    (SETQ CH (STRCAT KM_NO "/" ST_CH))
     (SET_TILE "INFO" (STRCAT "UPDATING LOC NO "  LOC " " "OF "  (ITOA I)))
     (SET_TILE "PERCENTAGE" (STRCAT "UPDATING CHAINAGE " CH))
    (IF (= I (- (LENGTH ENT_LIST_SPAN) 1))
      (PROGN
      (SET_TILE "INFO" (STRCAT "ALL "  (ITOA (+ I 1)) " LOCATIONS ARE UPDATED SUCESSFULLY"))
      (SET_TILE "PERCENTAGE" "PRESS OK TO VIEW RESULTS")
      )
    )
    
    (SETQ TEMP_ELE  (NTH I ENT_LIST_SPAN))
    (SETQ BASE_POINT (CDR (ASSOC 10 (ENTGET (NTH 0 TEMP_ELE)))))
    (MODIFY_ATTRIBUTES (NTH 0 TEMP_ELE) (LIST "SS_NUMBER1" "SS_CHAINAGE1") (LIST LOC CH))

    
    (SETQ TEMP_SPAN (* (ATOF (NTH 1 TEMP_ELE)) SCALE))
    (IF (= TEMP_SPAN 0)
    
    (PROGN
      (IF (/= I (- (LENGTH ENT_LIST_SPAN) 1)) (PROGN (SET_TILE "INFO" (STRCAT "SPAN HAS NOT BEEN ENTERED IN LOC NO "  LOC   ))
     (SET_TILE "PERCENTAGE" (STRCAT "PROGRAM STOPPED AT CHAINAGE " CH " PLEASE CHECK..."))))
      (SETQ I (LENGTH ENT_LIST_SPAN))
    ))
    (SETQ LOC_NO (ITOA (+ (ATOI LOC_NO) 2)))
    (SETQ ST_CH (RTOS (+ (ATOF ST_CH) (ATOF (NTH 1 TEMP_ELE))) 2 2))
    (SETQ I (+ I 1))
    )(setq ddiag(start_dialog))
)


(DEFUN ATTRIBUTES_FROM_ENTITIES (ENTNAME_LIST ATTRIBUTE_TAG_NAME FLAG / I LIST1 ENTNAME_LIST TEMP_ELE SAFEARRAY_SET ENT_OBJECT J)
  (SETQ I 0)
  (SETQ LIST1 NIL)

(WHILE (< I (LENGTH ENTNAME_LIST))
  (SETQ TEMP_ELE NIL)
  (SETQ SAFEARRAY_SET NIL) 
  (IF (NOT (ATOM (NTH I ENTNAME_LIST))) (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT (NTH FLAG (NTH I ENTNAME_LIST)))) (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT (NTH I ENTNAME_LIST))))

(IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
    (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

(SETQ J 0)
(SETQ TEMP_ELE NIL)

(WHILE (< J (LENGTH SAFEARRAY_SET))
  (IF (= (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TAGSTRING") ATTRIBUTE_TAG_NAME)
  (PROGN (IF (ATOM (NTH I ENTNAME_LIST))
    (SETQ TEMP_ELE (LIST (NTH I ENTNAME_LIST) (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TEXTSTRING")))
    (SETQ TEMP_ELE (APPEND (NTH I ENTNAME_LIST) (LIST (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TEXTSTRING"))))
  )))
    
    (SETQ J (+ J 1))

)))
(SETQ LIST1 (CONS TEMP_ELE LIST1))
(SETQ I (+ I 1))

)
(REVERSE LIST1)
)






(DEFUN FORM_SSSET (SSSET / I TEMP_ELE LIST1)
  (SETQ I 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST1 NIL_)
  (WHILE (< I (SSLENGTH SSSET))
    (SETQ TEMP_ELE (SSNAME SSSET I))
    (SETQ LIST1 (CONS TEMP_ELE LIST1))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST1)
)


;****************************************************CHAINAGE UPDATE FUNCTION ENDS****************************************************************************;

;                                                                                                                                                             ;
;                                                     DCL RELATED TO BOM                                                                                      ;
;                                                                                                                                                             ;

;******************************************************DCL LINKED CODE STARTS*********************************************************************************;


(DEFUN FILL_DATA_FINAL (ENTNAME SPAN_LIST WIRE_NATURE_LIST VISIBILITY_LIST WIRE_TYPE_LIST / I J )
(setq dcl_id (load_dialog "BRACKETS_WIRES.dcl"))
(new_dialog "BRACKETS_WIRES" dcl_id)
(SETQ I (LENGTH SPAN_LIST))

(SETQ J 1)
(WHILE (< J 4)
(IF (< I J)
(PROGN
(MODE_TILE (STRCAT "Stag" (ITOA J)) 1)
(MODE_TILE (STRCAT "Vtag" (ITOA  J)) 1)
(MODE_TILE (STRCAT "tag" (ITOA  J)) 1)
)
(PROGN
(SET_TILE (STRCAT "Stag" (ITOA  J)) (NTH (- J 1) SPAN_LIST))
(SET_TILE (STRCAT "Vtag" (ITOA  J)) (ITOA (NTH (- J 1) VISIBILITY_LIST)))
(START_LIST (STRCAT "tag" (ITOA  J)) 3)
           (mapcar 'add_list (LIST "IR" "OOR" "LSW" "TIR" "TOOR" "TLSW"))
           (end_list)
(SET_TILE (STRCAT "tag" (ITOA  J)) (ITOA (VL-POSITION (NTH (- J 1) WIRE_NATURE_LIST) (LIST "IR" "OOR" "LSW" "TIR" "TOOR" "TLSW")))))
)
  (SETQ J (+ J 1))
)
(IF (= (NTH 0 WIRE_TYPE_LIST) NIL)
(PROGN
(MODE_TILE "ACC" 1))
(PROGN
  (START_LIST  "ACC"  3)
           (mapcar 'add_list (LIST "0" "1"))
           (end_list)
(SET_TILE "ACC" (ITOA (VL-POSITION (NTH 0 WIRE_TYPE_LIST) (LIST "0" "1"))))))


(IF (= (NTH 1 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "AEW" 1))
(PROGN (START_LIST  "AEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "AEW" (ITOA (VL-POSITION (NTH 1 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

  
(IF (= (NTH 2 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "APW" 1))
(PROGN (START_LIST  "APW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "APW" (ITOA (VL-POSITION (NTH 2 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))


(IF (= (NTH 3 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "BEW" 1))
  
(PROGN (START_LIST  "BEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "BEW" (ITOA (VL-POSITION (NTH 3 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

(IF (= (NTH 4 WIRE_TYPE_LIST) NIL)
(PROGN
(MODE_TILE "TW" 1)) 
(PROGN (START_LIST  "TW"  3)
           (mapcar 'add_list (LIST "TRAMWAY" "NORMAL" "107" "150"))
           (end_list)
(SET_TILE "TW" (ITOA (VL-POSITION (NTH 4 WIRE_TYPE_LIST) (LIST "TRAMWAY" "NORMAL" "107" "150"))))))

  (action_tile "accept" "(GET_DCL_DATA_FINAL ENTNAME) (done_dialog 2)")
           (action_tile "cancel" "(done_dialog 1)")

(setq ddiag(start_dialog))
)






(DEFUN EXTRACT_ATTRIBUTES ( ENTNAME TAG_NAME_LIST FLAG / STRUCTURE_INFO ATTRIB_INFO I LIST2 TEMP_ELE)
  
  (SETQ ATTRIB_INFO (GET_ATTRIBUTES1 ENTNAME))
  (SETQ I 0)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH TAG_NAME_LIST))
    (SETQ TEMP_LIST (FILTER_LIST (LIST (NTH I TAG_NAME_LIST)) ATTRIB_INFO 0))
    (IF (/= TEMP_LIST NIL) (PROGN (SETQ TEMP_ELE (NTH FLAG (NTH 0 TEMP_LIST)))
    (SETQ TEMP_ELE (LIST (NTH I TAG_NAME_LIST) TEMP_ELE))
    (SETQ LIST2 (CONS TEMP_ELE LIST2))))
    (SETQ I (+ I 1))
 )
(REVERSE LIST2)
)

(DEFUN GET_ATTRIBUTES1	(ENTNAME / ENT_OBJECT SAFEARRAY_SET I LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

  (SETQ I 0)
  (SETQ LIST1 NIL)
  (WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ
      LIST1 (CONS
	      (LIST
		(VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING")
		(VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TEXTSTRING")
		(VLAX-SAFEARRAY->LIST
		  (VLAX-VARIANT-VALUE
		    (VLAX-GET-PROPERTY
		      (NTH I SAFEARRAY_SET)
		      "InsertionPoint"
		    )
		  )
		)
	        (IF ( = (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) 'INVISIBLE) :VLAX-TRUE ) 0 1)
	      )
	      LIST1
	    )
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST1 (REVERSE LIST1))
  (SETQ LIST1 (SORT_FUN LIST1 0 0)))
    (SETQ LIST1 NIL)
    )LIST1
  
)



(DEFUN CHANGE_VISIBILITY (ENTNAME IDENTIFIER VALUE / TEMP_ELE ENT_OBJECT SAFEARRAY_SET I J LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

  (SETQ I 0)
  (SETQ J 0)
  (SETQ LIST1 NIL)
  (SETQ TEMP_ELE NIL)
  (WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ TEMP_ELE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING"))
    (IF (/= (VL-POSITION TEMP_ELE IDENTIFIER) NIL) (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "INVISIBLE" (IF (= (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) "1") :VLAX-FALSE :VLAX-TRUE) ) ))
    (SETQ I (+ I 1))
  )  
)))




(DEFUN GET_DCL_DATA_FINAL (ENTNAME  / LIST2 SPAN_LIST I TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 SPAN_LIST WIRE_NATURE_LIST VISIBILITY_LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 TEMP_ELE4 TEMP_ELE5 TEMP_ELE6 TEMP_ELE7 TEMP_ELE8)
(SETQ I 1)
  (SETQ LIST2 NIL)
(WHILE (<= I 3)
  (SETQ TEMP_ELE1 (GET_TILE  (STRCAT "Stag" (ITOA  I))))
(IF (/= TEMP_ELE1 "")
(SETQ TEMP_ELE1 (LIST (STRCAT "SPAN" (ITOA  I)) (GET_TILE (STRCAT "Stag" (ITOA  I)) )))
(SETQ TEMP_ELE1 (LIST (STRCAT "SPAN" (ITOA  I)) "")))
(SETQ TEMP_ELE2 (GET_TILE  (STRCAT "tag" (ITOA  I))))
(IF (/= TEMP_ELE2 "")
(SETQ TEMP_ELE2 (LIST (STRCAT "WIRE_NATURE" (ITOA  I)) (NTH (ATOI TEMP_ELE2) (LIST "IR" "OOR" "LSW" "TIR" "TOOR" "TLSW"))))
(SETQ TEMP_ELE2 (LIST (STRCAT "WIRE_NATURE" (ITOA  I)) TEMP_ELE2)))
(IF (/= (NTH 1 TEMP_ELE1) "") 
(SETQ TEMP_ELE3 (LIST (STRCAT "SPAN" (ITOA  I)) (GET_TILE  (STRCAT "Vtag" (ITOA  I)))))
(SETQ TEMP_ELE3 (LIST (STRCAT "SPAN" (ITOA  I)) ))  )
  (SETQ LIST2 (CONS (LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3) LIST2))
  (SETQ I (+ I 1))
)
(SETQ LIST2 (REVERSE LIST2))
(SETQ SPAN_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 0) 1))
(SETQ WIRE_NATURE_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 1) 1))
(SETQ VISIBILITY_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 2) 1))
(SETQ TEMP_ELE4 (GET_TILE  "ACC") TEMP_ELE5 (GET_TILE  "AEW")  TEMP_ELE6 (GET_TILE  "APW") TEMP_ELE7 (GET_TILE  "BEW") TEMP_ELE8 (GET_TILE  "TW"))
(SETQ WIRE_TYPE_LIST (LIST (IF (/= TEMP_ELE4 "") (NTH (ATOI TEMP_ELE4) (LIST "0" "1")) "")
			   (IF (/= TEMP_ELE5 "") (NTH (ATOI TEMP_ELE5) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE6 "") (NTH (ATOI TEMP_ELE6) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE7 "") (NTH (ATOI TEMP_ELE7) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE8 "") (NTH (ATOI TEMP_ELE8) (LIST "TRAMWAY" "NORMAL" "107" "150")) "")))
(SETQ WIRE_TYPE_LIST (FILTER_LIST1 '("") (ADD_LISTS2 (LIST "ACC" "AEW" "APEC" "BEC" "WIRE_SYSTEM") WIRE_TYPE_LIST) 1))
(IF (/= SPAN_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST SPAN_LIST 0) (SINGLE_ELE_LIST SPAN_LIST 1)))
(IF (/= WIRE_NATURE_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST WIRE_NATURE_LIST 0) (SINGLE_ELE_LIST WIRE_NATURE_LIST 1)))
(IF (/= WIRE_TYPE_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST WIRE_TYPE_LIST 0) (SINGLE_ELE_LIST WIRE_TYPE_LIST 1)))
(IF (/= VISIBILITY_LIST NIL) (CHANGE_VISIBILITY ENTNAME (SINGLE_ELE_LIST VISIBILITY_LIST 0) (SINGLE_ELE_LIST VISIBILITY_LIST 1)))
)

(DEFUN DDATTE_MODIFIED (ENTNAME /  SPAN_LENGTHS_LIST WIRE_NATURE_LIST VISIBILITY_LIST  LIST1 WIRE_TYPE_LIST)
  
  (SETQ SPAN_LENGTHS_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "SPAN1" "SPAN2" "SPAN3") 1) 1))
  (SETQ WIRE_NATURE_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "WIRE_NATURE1" "WIRE_NATURE2" "WIRE_NATURE3") 1) 1))
  (SETQ VISIBILITY_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "SPAN1" "SPAN2" "SPAN3") 3) 1))
  (SETQ LIST1 (EXTRACT_ATTRIBUTES ENTNAME (LIST "AEW" "BEC" "APEC" "WIRE_SYSTEM" "ACC") 1))
  (SETQ WIRE_TYPE_LIST (LIST (CADR(ASSOC "ACC" LIST1)) (CADR(ASSOC "AEW" LIST1)) (CADR(ASSOC "APEC" LIST1)) (CADR(ASSOC "BEC" LIST1)) (CADR(ASSOC "WIRE_SYSTEM" LIST1))))
  (FILL_DATA_FINAL ENTNAME SPAN_LENGTHS_LIST WIRE_NATURE_LIST VISIBILITY_LIST WIRE_TYPE_LIST)
)



(DEFUN C:INFO_ENTRY ( / ENTNAME TYPE_OF_STRUCTURE)
  (SETQ ENTNAME (CAR(ENTSEL)))
  (SETQ TYPE_OF_STRUCTURE (NTH 1 (ASSOC 2 (YARD_STRUCTURE_INFO ENTNAME))))
  (IF (/= (VL-POSITION TYPE_OF_STRUCTURE '("SINGLE_CANT_MAST"  "SINGLE_CANT_DA" "DOUBLE_CANT_MAST"  "DOUBLE_CANT_DA" "TRIPLE_CANT_MAST"  "TRIPLE_CANT_DA" "SINGLE_CANT_UPRIGHT" "DOUBLE_CANT_UPRIGHT" "TRIPLE_CANT_UPRIGHT" "SS1" "SS2" "SS3" "PORTAL" "TTC" "SS4" "SS5")) NIL)
    (DDATTE_MODIFIED ENTNAME)
    (ALERT "INVALID BLOCK SELECTED")
  )
)



;******************************************************DCL LINKED CODE ENDS************************************************************************************;


;                                                                                                                                                             ;
;                                                     DCL RELATED TO BOM ENDS                                                                                 ;
;                                                                                                                                                             ;



;                                                                                                                                                             ;
;                                                     DCL RELATED TO SED DATA                                                                                 ;
;                                                                                                                                                             ;


;******************************************************DCL LINKED CODE STARTS*********************************************************************************;

;                                                                                                                                              ;
;                                                   DATA FILLING FUNCTIONS                                                                     ;
;                                                                                                                                              ;
(DEFUN FILL_DATA_FINAL_SED (ENTNAME SPAN_LIST WIRE_NATURE_LIST VISIBILITY_LIST WIRE_TYPE_LIST TRACK_INFO_LIST VISIBILITY_LIST2 / I J )
(setq dcl_id (load_dialog "SED_BRACKETS.dcl"))
(new_dialog "BRACKETS_WIRES" dcl_id)
(SETQ I (LENGTH SPAN_LIST))

(SETQ J 1)
(WHILE (< J 4)
(IF (< I J)
(PROGN
(MODE_TILE (STRCAT "Stag" (ITOA J)) 1)
(MODE_TILE (STRCAT "Vtag" (ITOA  J)) 1)
(MODE_TILE (STRCAT "tag" (ITOA  J)) 1)
)
(PROGN
(SET_TILE (STRCAT "Stag" (ITOA  J)) (NTH (- J 1) SPAN_LIST))
(SET_TILE (STRCAT "Vtag" (ITOA  J)) (ITOA (NTH (- J 1) VISIBILITY_LIST)))
(START_LIST (STRCAT "tag" (ITOA  J)) 3)
           (mapcar 'add_list (LIST "IR" "OOR" "LSW" "TIR" "TOOR" "TLSW"))
           (end_list)
(SET_TILE (STRCAT "tag" (ITOA  J)) (ITOA (VL-POSITION (NTH (- J 1) WIRE_NATURE_LIST) (LIST "IR" "OOR" "LSW" "TIR" "TOOR" "TLSW")))))
)
  (SETQ J (+ J 1))
)
(IF (= (NTH 0 WIRE_TYPE_LIST) NIL)
(PROGN
(MODE_TILE "ACC" 1))
(PROGN
  (START_LIST  "ACC"  3)
           (mapcar 'add_list (LIST "0" "1"))
           (end_list)
(SET_TILE "ACC" (ITOA (VL-POSITION (NTH 0 WIRE_TYPE_LIST) (LIST "0" "1"))))))


(IF (= (NTH 1 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "AEW" 1))
(PROGN (START_LIST  "AEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "AEW" (ITOA (VL-POSITION (NTH 1 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

  
(IF (= (NTH 2 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "APW" 1))
(PROGN (START_LIST  "APW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "APW" (ITOA (VL-POSITION (NTH 2 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))


(IF (= (NTH 3 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "BEW" 1))
  
(PROGN (START_LIST  "BEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "BEW" (ITOA (VL-POSITION (NTH 3 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

(IF (= (NTH 4 WIRE_TYPE_LIST) NIL)
(PROGN
(MODE_TILE "TW" 1)) 
(PROGN (START_LIST  "TW"  3)
           (mapcar 'add_list (LIST "TRAMWAY" "NORMAL" "107" "150"))
           (end_list)
(SET_TILE "TW" (ITOA (VL-POSITION (NTH 4 WIRE_TYPE_LIST) (LIST "TRAMWAY" "NORMAL" "107" "150"))))))

(IF (/= (NTH 0 TRACK_INFO_LIST) NIL)
(SET_TILE "TC" (NTH 0 TRACK_INFO_LIST))(MODE_TILE  "TC"  1))

(IF (/= (NTH 1 TRACK_INFO_LIST) NIL)
(SET_TILE "TN" (NTH 1 TRACK_INFO_LIST))(MODE_TILE  "TN"  1))

(IF (= (NTH 2 TRACK_INFO_LIST) NIL)
(PROGN
(MODE_TILE "TL" 1)) 
(PROGN (START_LIST "TL"  3)
           (mapcar 'add_list (LIST "NA" "I"))
           (end_list)
(SET_TILE "TL" (ITOA (VL-POSITION (NTH 2 TRACK_INFO_LIST) (LIST "NA" "I"))))))

(IF (/= (NTH 3 TRACK_INFO_LIST) NIL)
(SET_TILE "TS" (NTH 3 TRACK_INFO_LIST))(MODE_TILE  "TS"  1))


(IF (/= (NTH 4 TRACK_INFO_LIST) NIL)
(SET_TILE "OS" (NTH 4 TRACK_INFO_LIST))(MODE_TILE  "OS"  1))

(IF (/= (NTH 0 VISIBILITY_LIST2) NIL) (SET_TILE "TCtag1" (ITOA (NTH 0 VISIBILITY_LIST2))) (MODE_TILE  "TCtag1"  1))
(IF (/= (NTH 1 VISIBILITY_LIST2) NIL) (SET_TILE "Otag1" (ITOA (NTH 1 VISIBILITY_LIST2))) (MODE_TILE  "Otag1"  1))
  

  (action_tile "accept" "(GET_DCL_DATA_FINAL_SED ENTNAME) (done_dialog 2)")

           (action_tile "cancel" "(done_dialog 1)")

(setq ddiag(start_dialog))
)


(DEFUN FILL_DATA_FINAL_PORTAL (ENTNAME TC_LIST TN_LIST WIRE_TYPE_LIST TC_VISIBILITY_LIST  / I  )
(setq dcl_id (load_dialog "PORTAL.dcl"))
(new_dialog "PORTAL" dcl_id)


(SETQ I 0)
(WHILE (< I (LENGTH TC_LIST))
(SET_TILE (STRCAT "TC" (ITOA  (+ I 1))) (NTH I TC_LIST))
(SET_TILE (STRCAT "VTC" (ITOA  (+ I 1))) (ITOA (NTH I TC_VISIBILITY_LIST)))
(SET_TILE (STRCAT "TN" (ITOA  (+ I 1))) (NTH I TN_LIST))
(SETQ I (+ I 1))
)


(IF (= (NTH 0 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "AEW" 1))
(PROGN (START_LIST  "AEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "AEW" (ITOA (VL-POSITION (NTH 0 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

  
(IF (= (NTH 1 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "APW" 1))
(PROGN (START_LIST  "APW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "APW" (ITOA (VL-POSITION (NTH 1 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))


(IF (= (NTH 2 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "BEW" 1))
  
(PROGN (START_LIST  "BEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "BEW" (ITOA (VL-POSITION (NTH 2 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

  (action_tile "accept" "(GET_DCL_DATA_FINAL_PORTAL ENTNAME) (done_dialog 2)")

  (action_tile "cancel" "(done_dialog 1)")

(setq ddiag(start_dialog))
)


(DEFUN FILL_DATA_FINAL_TTC (ENTNAME TC_LIST TN_LIST TC WIRE_TYPE_LIST_VISIBILITY_LIST  / I  )
(setq dcl_id (load_dialog "PORTAL.dcl"))
(new_dialog "TTC" dcl_id)


(SETQ I 0)
(WHILE (< I (LENGTH TC_LIST))
(SET_TILE (STRCAT "TC" (ITOA  (+ I 1))) (NTH I TC_LIST))
(SET_TILE (STRCAT "VTC" (ITOA  (+ I 1))) (ITOA (NTH I TC_VISIBILITY_LIST)))
(SET_TILE (STRCAT "TN" (ITOA  (+ I 1))) (NTH I TN_LIST))
(SETQ I (+ I 1))
)


(IF (= (NTH 0 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "AEW" 1))
(PROGN (START_LIST  "AEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "AEW" (ITOA (VL-POSITION (NTH 0 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

  
(IF (= (NTH 1 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "APW" 1))
(PROGN (START_LIST  "APW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "APW" (ITOA (VL-POSITION (NTH 1 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))


(IF (= (NTH 2 WIRE_TYPE_LIST) NIL)
  (PROGN
(MODE_TILE "BEW" 1))
  
(PROGN (START_LIST  "BEW"  3)
           (mapcar 'add_list (LIST "0" "1" "2" "3"))
           (end_list)
(SET_TILE "BEW" (ITOA (VL-POSITION (NTH 2 WIRE_TYPE_LIST) (LIST "0" "1" "2" "3"))))))

  (action_tile "accept" "(GET_DCL_DATA_FINAL_PORTAL ENTNAME) (done_dialog 2)")

  (action_tile "cancel" "(done_dialog 1)")

(setq ddiag(start_dialog))
)



;                                                                                                                                              ;
;                                                   ATTRIBUTE EXTRACTION FUNCTIONS                                                             ;
;                                                                                                                                              ;



(DEFUN EXTRACT_ATTRIBUTES (ENTNAME TAG_NAME_LIST FLAG / STRUCTURE_INFO ATTRIB_INFO I LIST2 TEMP_ELE)
  
  (SETQ ATTRIB_INFO (GET_ATTRIBUTES1 ENTNAME))
  (SETQ I 0)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH TAG_NAME_LIST))
    (SETQ TEMP_LIST (FILTER_LIST (LIST (NTH I TAG_NAME_LIST)) ATTRIB_INFO 0))
    (IF (/= TEMP_LIST NIL) (PROGN (SETQ TEMP_ELE (NTH FLAG (NTH 0 TEMP_LIST)))
    (SETQ TEMP_ELE (LIST (NTH I TAG_NAME_LIST) TEMP_ELE))
    (SETQ LIST2 (CONS TEMP_ELE LIST2))))
    (SETQ I (+ I 1))
 )
(REVERSE LIST2)
)

(DEFUN GET_ATTRIBUTES1	(ENTNAME / ENT_OBJECT SAFEARRAY_SET I LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

  (SETQ I 0)
  (SETQ LIST1 NIL)
  (WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ
      LIST1 (CONS
	      (LIST
		(VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING")
		(VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TEXTSTRING")
		(VLAX-SAFEARRAY->LIST
		  (VLAX-VARIANT-VALUE
		    (VLAX-GET-PROPERTY
		      (NTH I SAFEARRAY_SET)
		      "InsertionPoint"
		    )
		  )
		)
	        (IF ( = (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) 'INVISIBLE) :VLAX-TRUE ) 0 1)
	      )
	      LIST1
	    )
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST1 (REVERSE LIST1))
  (SETQ LIST1 (SORT_FUN LIST1 0 0)))
    (SETQ LIST1 NIL)
    )LIST1
  
)



(DEFUN CHANGE_VISIBILITY (ENTNAME IDENTIFIER VALUE / TEMP_ELE ENT_OBJECT SAFEARRAY_SET I J LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT))
  (IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

  (SETQ I 0)
  (SETQ J 0)
  (SETQ LIST1 NIL)
  (SETQ TEMP_ELE NIL)
  (WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ TEMP_ELE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING"))
    (IF (/= (VL-POSITION TEMP_ELE IDENTIFIER) NIL) (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "INVISIBLE" (IF (= (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) "1") :VLAX-FALSE :VLAX-TRUE) ) ))
    (SETQ I (+ I 1))
  )  
)))


;                                                                                                                                              ;
;                                                   DIALOGUE BOX EXTRACTION FUNCTIONS                                                          ;
;                                                                                                                                              ;

(DEFUN GET_DCL_DATA_FINAL_SED (ENTNAME  / LIST2 SPAN_LIST I TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 SPAN_LIST WIRE_NATURE_LIST VISIBILITY_LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 TEMP_ELE4 TEMP_ELE5 TEMP_ELE6 TEMP_ELE7 TEMP_ELE8 SED_DATA)
(SETQ I 1)
  (SETQ LIST2 NIL)
(WHILE (<= I 3)
  (SETQ TEMP_ELE1 (GET_TILE  (STRCAT "Stag" (ITOA  I))))
(IF (/= TEMP_ELE1 "")
(SETQ TEMP_ELE1 (LIST (STRCAT "SPAN" (ITOA  I)) (GET_TILE (STRCAT "Stag" (ITOA  I)) )))
(SETQ TEMP_ELE1 (LIST (STRCAT "SPAN" (ITOA  I)) "")))
(SETQ TEMP_ELE2 (GET_TILE  (STRCAT "tag" (ITOA  I))))
(IF (/= TEMP_ELE2 "")
(SETQ TEMP_ELE2 (LIST (STRCAT "WIRE_NATURE" (ITOA  I)) (NTH (ATOI TEMP_ELE2) (LIST "IR" "OOR" "LSW" "TIR" "TOOR" "TLSW"))))
(SETQ TEMP_ELE2 (LIST (STRCAT "WIRE_NATURE" (ITOA  I)) TEMP_ELE2)))
(IF (/= (NTH 1 TEMP_ELE1) "") 
(SETQ TEMP_ELE3 (LIST (STRCAT "SPAN" (ITOA  I)) (GET_TILE  (STRCAT "Vtag" (ITOA  I)))))
(SETQ TEMP_ELE3 (LIST (STRCAT "SPAN" (ITOA  I)) ))  )
  (SETQ LIST2 (CONS (LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3) LIST2))
  (SETQ I (+ I 1))
)
(SETQ LIST2 (REVERSE LIST2))
(SETQ SPAN_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 0) 1))
(SETQ WIRE_NATURE_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 1) 1))
(SETQ VISIBILITY_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 2) 1))
(SETQ VISIBILITY_LIST2 (FILTER_LIST1 '("") (LIST (LIST "TC1" (GET_TILE  "TCtag1")) (LIST "TS" (GET_TILE  "Otag1"))) 1))
(SETQ TEMP_ELE4 (GET_TILE  "ACC") TEMP_ELE5 (GET_TILE  "AEW")  TEMP_ELE6 (GET_TILE  "APW") TEMP_ELE7 (GET_TILE  "BEW") TEMP_ELE8 (GET_TILE  "TW"))
(SETQ WIRE_TYPE_LIST (LIST (IF (/= TEMP_ELE4 "") (NTH (ATOI TEMP_ELE4) (LIST "0" "1")) "")
			   (IF (/= TEMP_ELE5 "") (NTH (ATOI TEMP_ELE5) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE6 "") (NTH (ATOI TEMP_ELE6) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE7 "") (NTH (ATOI TEMP_ELE7) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE8 "") (NTH (ATOI TEMP_ELE8) (LIST "TRAMWAY" "NORMAL")) "")))
(SETQ WIRE_TYPE_LIST (FILTER_LIST1 '("") (ADD_LISTS2 (LIST "ACC" "AEW" "APEC" "BEC" "WIRE_SYSTEM") WIRE_TYPE_LIST) 1))
(SETQ SED_DATA (GET_SED_RELATED_DATA))
(IF (/= SPAN_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST SPAN_LIST 0) (SINGLE_ELE_LIST SPAN_LIST 1)))
(IF (/= WIRE_NATURE_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST WIRE_NATURE_LIST 0) (SINGLE_ELE_LIST WIRE_NATURE_LIST 1)))
(IF (/= WIRE_TYPE_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST WIRE_TYPE_LIST 0) (SINGLE_ELE_LIST WIRE_TYPE_LIST 1)))
(IF (/= SED_DATA NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST SED_DATA 0) (SINGLE_ELE_LIST SED_DATA 1)))
(IF (/= VISIBILITY_LIST NIL) (CHANGE_VISIBILITY ENTNAME (SINGLE_ELE_LIST VISIBILITY_LIST 0) (SINGLE_ELE_LIST VISIBILITY_LIST 1)))
(IF (/= VISIBILITY_LIST2 NIL) (CHANGE_VISIBILITY ENTNAME (SINGLE_ELE_LIST VISIBILITY_LIST2 0) (SINGLE_ELE_LIST VISIBILITY_LIST2 1)))
)

(DEFUN GET_SED_RELATED_DATA ( / TC TN TS OS TL)
  (SETQ TC (GET_TILE "TC") TN (GET_TILE "TN") TS (GET_TILE "TS") OS (GET_TILE "OS"))
  (SETQ TL (NTH (ATOI (GET_TILE "TL")) (LIST "NA" "I")))
  (LIST (LIST "TC1" TC)  (LIST "TRACK_TYPE" TN)  (LIST "TYPE_OF_LOCATION" TL)  (LIST "TS" TS) (LIST "OBLIGATORY_WIRE_SEQUENCE" OS))
)

(DEFUN GET_DCL_DATA_FINAL_PORTAL (ENTNAME  / LIST2 SPAN_LIST I TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 SPAN_LIST WIRE_NATURE_LIST VISIBILITY_LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3 TEMP_ELE4 TEMP_ELE5 TEMP_ELE6 TEMP_ELE7 TEMP_ELE8 SED_DATA)
(SETQ I 1)
(SETQ LIST2 NIL)
(WHILE (<= I 8)
  (SETQ TEMP_ELE1 (GET_TILE  (STRCAT "TC" (ITOA  I))))
(IF (/= TEMP_ELE1 "")
(SETQ TEMP_ELE1 (LIST (STRCAT "TC" (ITOA  I)) (GET_TILE (STRCAT "TC" (ITOA  I)) )))
(SETQ TEMP_ELE1 (LIST (STRCAT "TC" (ITOA  I)) "")))
(SETQ TEMP_ELE2 (GET_TILE  (STRCAT "TN" (ITOA  I))))
(IF (/= TEMP_ELE2 "")
(SETQ TEMP_ELE2 (LIST (STRCAT "TRACK_TYPE" (ITOA  I)) (GET_TILE (STRCAT "TN" (ITOA  I)) )))
(SETQ TEMP_ELE2 (LIST (STRCAT "TRACK_TYPE" (ITOA  I)) TEMP_ELE2)))
(IF (/= (NTH 1 TEMP_ELE1) "") 
(SETQ TEMP_ELE3 (LIST (STRCAT "TC" (ITOA  I)) (GET_TILE  (STRCAT "VTC" (ITOA  I)))))
(SETQ TEMP_ELE3 (LIST (STRCAT "TC" (ITOA  I)) ))  )
  (SETQ LIST2 (CONS (LIST TEMP_ELE1 TEMP_ELE2 TEMP_ELE3) LIST2))
  (SETQ I (+ I 1))
)
(SETQ LIST2 (REVERSE LIST2))
(SETQ TC_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 0) 1))
(SETQ TN_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 1) 1))
(SETQ TC_VISIBILITY_LIST (FILTER_LIST1 '("") (SINGLE_ELE_LIST LIST2 2) 1))

(SETQ TEMP_ELE1 (GET_TILE  "AEW")  TEMP_ELE2 (GET_TILE  "APW") TEMP_ELE3 (GET_TILE  "BEW") )
(SETQ WIRE_TYPE_LIST (LIST (IF (/= TEMP_ELE1 "") (NTH (ATOI TEMP_ELE1) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE2 "") (NTH (ATOI TEMP_ELE2) (LIST "0" "1" "2" "3")) "")
			   (IF (/= TEMP_ELE3 "") (NTH (ATOI TEMP_ELE3) (LIST "0" "1" "2" "3")) "")
			   
			   ))
(SETQ WIRE_TYPE_LIST (FILTER_LIST1 '("") (ADD_LISTS2 (LIST "AEW" "APEC" "BEC") WIRE_TYPE_LIST) 1))

(IF (/= TC_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST TC_LIST 0) (SINGLE_ELE_LIST TC_LIST 1)))
(IF (/= TN_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST TN_LIST 0) (SINGLE_ELE_LIST TN_LIST 1)))
(IF (/= WIRE_TYPE_LIST NIL) (MODIFY_ATTRIBUTES ENTNAME (SINGLE_ELE_LIST WIRE_TYPE_LIST 0) (SINGLE_ELE_LIST WIRE_TYPE_LIST 1)))

(IF (/= TC_VISIBILITY_LIST NIL) (CHANGE_VISIBILITY ENTNAME (SINGLE_ELE_LIST TC_VISIBILITY_LIST 0) (SINGLE_ELE_LIST TC_VISIBILITY_LIST 1)))

)


;___________________________________________________________________________________________________________________________________________________________________________________;


;                                                                                                                                              ;
;                                                   ATTRIBUTE EXTRACTION FUNCTIONS                                                             ;
;                                                                                                                                              ;
  

(DEFUN DDATTE_MODIFIED_SED (ENTNAME /  SPAN_LENGTHS_LIST WIRE_NATURE_LIST VISIBILITY_LIST  LIST1 WIRE_TYPE_LIST LIST2 TRACK_INFO_LIST)
  
  (SETQ SPAN_LENGTHS_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "SPAN1" "SPAN2" "SPAN3") 1) 1))
  (SETQ WIRE_NATURE_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "WIRE_NATURE1" "WIRE_NATURE2" "WIRE_NATURE3") 1) 1))
  (SETQ VISIBILITY_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "SPAN1" "SPAN2" "SPAN3") 3) 1))
  (SETQ VISIBILITY_TC (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "TC1") 3) 1))
  (SETQ VISIBILITY_TS (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "TS") 3) 1))
  (IF (/= VISIBILITY_TC NIL) (SETQ VISIBILITY_TC (NTH 0 VISIBILITY_TC)))
  (IF (/= VISIBILITY_TS NIL) (SETQ VISIBILITY_TS (NTH 0 VISIBILITY_TS)))
  (SETQ VISIBILITY_LIST2 (LIST VISIBILITY_TC VISIBILITY_TS))
  (SETQ LIST1 (EXTRACT_ATTRIBUTES ENTNAME (LIST "AEW" "BEC" "APEC" "WIRE_SYSTEM" "ACC") 1))
  (SETQ WIRE_TYPE_LIST (LIST (CADR(ASSOC "ACC" LIST1)) (CADR(ASSOC "AEW" LIST1)) (CADR(ASSOC "APEC" LIST1)) (CADR(ASSOC "BEC" LIST1)) (CADR(ASSOC "WIRE_SYSTEM" LIST1))))
  (SETQ LIST2 (EXTRACT_ATTRIBUTES ENTNAME (LIST "TC1" "TRACK_TYPE" "TYPE_OF_LOCATION" "TS" "OBLIGATORY_WIRE_SEQUENCE" ) 1))
  (SETQ TRACK_INFO_LIST (LIST (CADR(ASSOC "TC1" LIST2)) (CADR(ASSOC "TRACK_TYPE" LIST2)) (CADR(ASSOC "TYPE_OF_LOCATION" LIST2)) (CADR(ASSOC "TS" LIST2)) (CADR(ASSOC "OBLIGATORY_WIRE_SEQUENCE" LIST2))))
  (IF (= VISIBILITY_LIST2 NIL) (SETQ VISIBILITY_LIST2 (LIST NIL NIL)))
  (FILL_DATA_FINAL_SED ENTNAME SPAN_LENGTHS_LIST WIRE_NATURE_LIST VISIBILITY_LIST WIRE_TYPE_LIST TRACK_INFO_LIST VISIBILITY_LIST2)
)



(DEFUN DATTE_PORTAL/TTC (ENTNAME / TC_LIST TN_LIST TC_VISIBILITY_LIST)
(SETQ BLOCK_NAME (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ENTNAME) "EFFECTIVENAME"))
(SETQ TC_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "TC1" "TC2" "TC3" "TC4" "TC5" "TC6" "TC7" "TC8") 1) 1))
(SETQ TN_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "TRACK_TYPE1" "TRACK_TYPE2" "TRACK_TYPE3" "TRACK_TYPE4" "TRACK_TYPE5" "TRACK_TYPE6" "TRACK_TYPE7" "TRACK_TYPE8") 1) 1))
(SETQ WIRE_TYPE_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "AEW" "APEC" "BEC") 1) 1))
(SETQ TC_VISIBILITY_LIST (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "TC1" "TC2" "TC3" "TC4" "TC5" "TC6" "TC7" "TC8") 3) 1))
(IF (/= (VL-POSITION BLOCK_NAME (LIST "PORTAL" "SS5")) NIL) (FILL_DATA_FINAL_PORTAL ENTNAME TC_LIST TN_LIST WIRE_TYPE_LIST TC_VISIBILITY_LIST))
(IF (/= (VL-POSITION BLOCK_NAME (LIST "TTC" "SS4")) NIL) (FILL_DATA_FINAL_TTC ENTNAME TC_LIST TN_LIST WIRE_TYPE_LIST TC_VISIBILITY_LIST))
)

;____________________________________________________________________________________________________________________________________________________________________________________;

;                                                                                                                                              ;
;                                                   FINAL FUNCTION                                                                             ;
;                                                                                                                                              ;

(DEFUN C:INFO_ENTRY2 ( / ENTNAME TYPE_OF_STRUCTURE)
  (SETQ ENTNAME (CAR(ENTSEL)))
  (SETQ TYPE_OF_STRUCTURE (NTH 1 (ASSOC 2 (YARD_STRUCTURE_INFO ENTNAME))))
  (IF (/= (VL-POSITION TYPE_OF_STRUCTURE '("SINGLE_CANT_MAST"  "SINGLE_CANT_DA" "DOUBLE_CANT_MAST"  "DOUBLE_CANT_DA" "TRIPLE_CANT_MAST"  "TRIPLE_CANT_DA" "SINGLE_CANT_UPRIGHT" "DOUBLE_CANT_UPRIGHT" "TRIPLE_CANT_UPRIGHT" "SS0" "SS1" "SS2" "SS3" "PORTAL" "TTC" "SS4" "SS5")) NIL) 
  (PROGN (IF (/= (VL-POSITION TYPE_OF_STRUCTURE '("SINGLE_CANT_MAST"  "SINGLE_CANT_DA" "DOUBLE_CANT_MAST"  "DOUBLE_CANT_DA" "TRIPLE_CANT_MAST"  "TRIPLE_CANT_DA" "SINGLE_CANT_UPRIGHT" "DOUBLE_CANT_UPRIGHT" "TRIPLE_CANT_UPRIGHT" "SS0" "SS1" "SS2" "SS3")) NIL) (DDATTE_MODIFIED_SED ENTNAME))
	 (IF (/= (VL-POSITION TYPE_OF_STRUCTURE '("PORTAL" "TTC" "SS4" "SS5")) NIL) (DATTE_PORTAL/TTC ENTNAME)))
    (ALERT "INVALID BLOCK SELECTED")
  )
)



;******************************************************DCL LINKED CODE ENDS************************************************************************************;



;                                                                                                                                                             ;
;                                                     DCL RELATED TO SED DATA ENDS                                                                            ;
;                                                                                                                                                             ;






;******************************************************GATTE FUNCTION STARTS******************************************************************************************;




(DEFUN C:GATTE1	(/    N			;SELECTION SET COUNTER
		 CC			;CHANGED COUNTER
		 BN			;BLOCK NAME
		 TG			;TAG NAME
		 ESEL			;ENTITY PICK/NAME/LIST
		 EL			;ENTITY LIST
		 EN			;ENTITY NAME
		 PASS			;LOOP PASS FLAG
		 TAGL			;LIST OF VALID TAGS FOR A BLOCK
		 TAGS			;STRING OF VALID TAGS FOR A BLOCK
		 TAGT			;TEMP TAG LIST
					;TAG    ;TAG NAME IN LOOP
		 TMP			;TEMPORARY VARIABLE
		 SS1			;SELECTION SET OF INSERT OBJECTS
		 XX   X			;FLAG AND COUNTER
		 OLDCC			;PREVIOUS COUNT OF CHANGES FOR UPDATE TEST
		 A			;ENTITY INFORMATION IN CHANGE LOOP
		 FL   LA		;FROZEN LAYER CHECK VARIABLES
		 NA   B
		)
;;;;;;;ORIGINAL NAME FUNCTION IN WHICH INPUT IS ENTITY_NAME AND OUTPUT IS ORIGINAL
(DEFUN ORG_NAME	(ENT /)
  (VLAX-GET-PROPERTY
    (VLAX-ENAME->VLA-OBJECT ENT)
    "EFFECTIVENAME"
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FORMATION OF SELECTION SET;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ENT_NAME IS BLOCK NAME;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SS_FORM (ENT_NAME / SS_SET_ALL I SS_SET_MAIN)
  (SETQ	SS_SET_ALL
	 (SSGET	"_X"
		(LIST
		  (CONS 0 "INSERT")
		  (CONS 66 1)
		)
	 )
  )
  (SETQ SS_SET_MAIN (SSADD))
  (SETQ I 0)
  (WHILE (< I (SSLENGTH SS_SET_ALL))
    (IF	(= (ORG_NAME (SSNAME SS_SET_ALL I)) ENT_NAME)
      (SETQ SS_SET_MAIN (SSADD (SSNAME SS_SET_ALL I) SS_SET_MAIN))
    )
    (SETQ I (+ I 1))
  )
  SS_SET_MAIN
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;FORMATION OF SELECTION SET UNDER USER SELECTION ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;ENT_NAME IS BLOCK_NAME;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SS_FORM1	(ENT_NAME / PASS SS_MAIN I)
  (SETQ SS_MAIN (SSADD))
  (SETQ PASS 'T)
  (WHILE PASS
    (SETQ PASS
	   (ENTSEL
	     "\nSELECT OBJECT WHICH IS IDENTICAL TO SOURE OBJECT:"
	   )
    )
    (IF	(/= PASS NIL)
      (IF (AND (= (CDR (ASSOC 0 (ENTGET (CAR PASS)))) "INSERT")
	       (= ENT_NAME (ORG_NAME (CAR PASS)))
	  )
	(PROGN
	  (SETQ SS_MAIN (SSADD (CAR PASS) SS_MAIN))
	  (VLAX-INVOKE-METHOD
	    (VLAX-ENAME->VLA-OBJECT (CAR PASS))
	    "HIGHLIGHT"
	    1
	  )
	)
	(PROGN
	  (PROMPT
	    "\nSELECTED OBJECT IS NOT A COPY OF SOUTCE OBJECT IT IS DELETE FROM SELECTION ,CONTINUE"
	  )
	  (VLAX-INVOKE-METHOD
	    (VLAX-ENAME->VLA-OBJECT (CAR PASS))
	    "HIGHLIGHT"
	    0
	  )
	)
      )
    )
  )
  (IF (/= (SSLENGTH SS_MAIN) 0)
    (PROGN
      (SETQ I 0)
      (WHILE (< I (SSLENGTH SS_MAIN))
	(VLAX-INVOKE-METHOD
	  (VLAX-ENAME->VLA-OBJECT (SSNAME SS_MAIN I))
	  "HIGHLIGHT"
	  0
	)
	(SETQ I (+ I 1))
      )
      SS_MAIN
    )
    (SETQ SS_MAIN NIL)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;MAIN AUTOCAD ORIGINAL PROGRAM STARTS HERE;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ACET-ERROR-INIT
    (LIST
      (LIST "CMDECHO" 0)
      T					;FLAG. TRUE MEANS USE UNDO FOR ERROR CLEAN UP.
    )					;LIST
  )					;ACET-ERROR-INIT
  (SSSETFIRST NIL NIL)
  ;;
  (SETQ	N  0
	CC 0
  )
  (WHILE (NULL PASS)
    (INITGET "BLOCK _BLOCK")
    ;;      (SETQ ESEL (ENTSEL "\NBLOCK NAME/<SELECT BLOCK OR ATTRIBUTE>: "))
    (SETQ ESEL (ENTSEL "\nSELECT BLOCK OR ATTRIBUTE [BLOCK NAME]: "))
    (COND
      ((NULL ESEL)
       (SETQ PASS 'T
	     BN	NIL
       )
      )
      ((= (TYPE ESEL) 'LIST)
       ;;PICK SELECTION
       (SETQ EL (ENTGET (CAR ESEL)))
       (IF (= (CDR (ASSOC 0 EL)) "INSERT")
	 (SETQ BN   (ORG_NAME (CAR ESEL))
	       PASS 'T
	       ESEL (NENTSELP (CADR ESEL))
	       EL   (ENTGET (CAR ESEL))
	       TG   (IF	(= (CDR (ASSOC 0 EL)) "ATTRIB")
		      (CDR (ASSOC 2 EL))
		      NIL
		    )
	 )
	 (PROMPT "\nSELECTED ITEM NOT AN INSERT.")
       )				;END IF
      )					;END SECOND CONDITIONAL FOR PICKING ATTRIB
      ((AND (= (TYPE ESEL) 'STR) (= ESEL "BLOCK"))
       (SETQ BN (GETSTRING "\nENTER BLOCK NAME: "))
       (IF (TBLSEARCH "BLOCK" BN)
	 (SETQ PASS 'T)
	 (PROMPT "\nINVALID BLOCK NAME.")
       )
      )					;END THIRD CONDITIONAL
    )					;THE CONDITIONAL STATEMENT ENDS
  )
  ;;END OF BLOCK NAME ENTRY.
  (IF BN
    (PROGN
      (SETQ PASS NIL
	    EN	 (CDR (ASSOC -2 (TBLSEARCH "BLOCK" BN)))
      )
      (WHILE EN
	(SETQ EL (ENTGET EN))
	(IF (= (CDR (ASSOC 0 EL)) "ATTDEF")
	  (SETQ TAGL (CONS (CDR (ASSOC 2 EL)) TAGL))
	)
	(SETQ EN (ENTNEXT EN))
      )
    )
  )
  ;;END IF BN PROGN
  (IF TG
    (SETQ PASS 'T)
  )
  (IF TAGL
    (PROGN
      (SETQ TAGS (CAR TAGL)
	    TAGT (CDR TAGL)
      )
      (FOREACH TAG TAGT
	(SETQ TAGS (STRCAT TAGS " " TAG))
      )
    )
  )
  (WHILE (AND TAGS (NULL PASS))
    (INITGET TAGS)
    (PROMPT (STRCAT "\nKNOWN TAG NAMES FOR BLOCK: " TAGS))
    (SETQ ESEL (NENTSEL "\nSELECT ATTRIBUTE OR TYPE ATTRIBUTE NAME: "))
    (COND
      ((= (TYPE ESEL) 'STR)
       (SETQ ESEL (XSTRCASE ESEL))
       (IF (MEMBER ESEL TAGL)
	 (SETQ PASS 'T
	       TG ESEL
	 )
	 (PROMPT "\nINVALID ATTRIBUTE NAME.")
       )
      )
      ((= (TYPE ESEL) 'LIST)
       ;;PICK SELECTION
       (SETQ TG (CDR (ASSOC 2 (ENTGET (CAR ESEL)))))
       (IF TG
	 (SETQ PASS 'T)
       )
      )
    )					;THE CONDITIONAL STATEMENT ENDS
  )
  ;;END OF ATTRIBUTE NAME ENTRY.
  (IF (AND BN (NULL TAGL))
    (SETQ BN (PROMPT "\nTHE BLOCK SELECTED HAS NO ATTRIBUTES!"))
  )
;;;;;;;;;;;MAIN;;;;;;;;;;;
  (IF (AND BN TG)
    (PROGN
      (PROMPT
	(ACET-STR-FORMAT "\nBLOCK: %1   ATTRIBUTE TAG: %2" BN TG)
      )
      (SETQ
	NA  (GETSTRING T "\nENTER NEW TEXT: ")
	SS1 (SS_FORM BN)
	N   (IF	SS1
	      (SSLENGTH SS1)
	      0
	    )
      )
      (INITGET 0 "YES NO _YES NO")
      (SETQ TMP
	     (GETKWORD
	       (ACET-STR-FORMAT
		 "\nNUMBER OF INSERTS IN DRAWING = %1  PROCESS ALL OF THEM? [YES/NO] <YES>: "
		 (ITOA N)
	       )
	     )
      )
      (IF (AND TMP (= TMP "NO"))
	(SETQ SS1 (SS_FORM1 BN)
	      N	  (IF SS1
		    (SSLENGTH SS1)
		    0
		  )
	)
      )
      (IF (> N 0)
	(PRINC "\nPLEASE WAIT...")
      )
      (SETQ X 0)
      (REPEAT N
	(SETQ A	 (SSNAME SS1 X)
	      B	 (ENTGET A)
	      LA (CDR (ASSOC 8 B))	;LAYER NAME FROM OBJECT
	      FL (TBLSEARCH "LAYER" LA)	;TABLE ENTRY FOR LAYER
	      FL (CDR (ASSOC 70 FL))	;LAYER STATUS FLAG
	)
	(IF (/= FL 65)			;IF LAYER NOT FROZEN
	  (PROGN
	    (SETQ XX 1
		  OLDCC	CC
	    )
	    (WHILE XX
	      (SETQ
		B (ENTGET (ENTNEXT (CDR (ASSOC -1 B))))
	      )
	      (IF (= (CDR (ASSOC 0 B)) "SEQEND")
		(SETQ XX NIL)
		(PROGN
		  (IF (= (CDR (ASSOC 2 B)) TG)
		    (PROGN
		      (SETQ B  (SUBST (CONS 1 NA) (ASSOC 1 B) B)
			    CC (1+ CC)
		      )
		      (ENTMOD B)
		    )			;PROGN
		  )			;IF
		)			;PROGN
	      )				;IF
	    )				;WHILE
	    (IF	(/= CC OLDCC)
	      (ENTUPD A)
	    )
	  )				;PROGN
	)				;IF
	(SETQ X (1+ X))
      )					;REPEAT
      (IF (/= 1 CC)
	(PRINC
	  (ACET-STR-FORMAT "\n%1 ATTRIBUTES CHANGED." (ITOA CC))
	)
	(PRINC (ACET-STR-FORMAT "\n%1 ATTRIBUTE CHANGED." (ITOA CC))
	)
      )
    )					;PROGN
  )
  (ACET-ERROR-RESTORE)
  (PRINC)
)					;DEFUN
(PRINC)



;******************************GATTE FUNCTION ENDS******************************************************************************************************************;






;************************************FUNCTIONS LINKED TO DCL FOR OHE TOOL BAR ICONS**********************************************************************************;



(defun C:MAST ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI )

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "MAST" dcl_id))
	(progn
	  (alert
	    "The MAST definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "SINGLE_CANT_MAST")
	  (SETQ CANTI2 "DOUBLE_CANT_MAST")
	  (SETQ CANTI3 "TRIPLE_CANT_MAST")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "but3" "(SETQ CANTI CANTI3)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)






(defun C:DROPARM ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI )

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "DROPARM" dcl_id))
	(progn
	  (alert
	    "The DROPARM definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "SINGLE_CANT_DA")
	  (SETQ CANTI2 "DOUBLE_CANT_DA")
	  (SETQ CANTI3 "TRIPLE_CANT_DA")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "but3" "(SETQ CANTI CANTI3)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)




(defun C:UPRIGHTCANTILEVER ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI )

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "UPRIGHTCANTILEVER" dcl_id))
	(progn
	  (alert
	    "The DROPARM definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "SINGLE_CANT_UPRIGHT")
	  (SETQ CANTI2 "DOUBLE_CANT_UPRIGHT")
	  (SETQ CANTI3 "TRIPLE_CANT_UPRIGHT")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "but3" "(SETQ CANTI CANTI3)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)




(defun C:SPECIALSTRUCTURE ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI5 CANTI)

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "SPECIALSTRUCTURE" dcl_id))
	(progn
	  (alert
	    "The SPECIALSTRUCTURE definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "SS0")
	  (SETQ CANTI2 "SS1")
	  (SETQ CANTI3 "SS2")
          (SETQ CANTI4 "SS3")
          (SETQ CANTI5 "SS4")
          (SETQ CANTI6 "SS5")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "but3" "(SETQ CANTI CANTI3)")
          (action_tile "but4" "(SETQ CANTI CANTI4)")
          (action_tile "but5" "(SETQ CANTI CANTI5)")
          (action_tile "but6" "(SETQ CANTI CANTI6)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)



(defun C:INS_ANC ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI )

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "ANCHOR" dcl_id))
	(progn
	  (alert
	    "The ANCHOR definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "ANCHOR")
	  (SETQ CANTI2 "BTB_ANC")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)



(defun C:INSULATOR ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI )

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "INSULATOR" dcl_id))
	(progn
	  (alert
	    "The INSULATOR definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "CUTIN_INSULATOR")
	  (SETQ CANTI2 "SI")
	  (SETQ CANTI3 "PTFE")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "but3" "(SETQ CANTI CANTI3)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)

(defun C:ISOLATOR ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI )

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "ISOLATOR" dcl_id))
	(progn
	  (alert
	    "The ISOLATOR definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "SPS")
	  (SETQ CANTI2 "DPS")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)



(defun C:MISC ( / CANTI1 CANTI2 CANTI3 CANTI4 CANTI5 CANTI)

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "MAST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "MISC" dcl_id))
	(progn
	  (alert
	    "The MISC definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn
	  (SETQ CANTI1 "PLATFORM")
	  (SETQ CANTI2 "TENSION_LENGTH")
	  (SETQ CANTI3 "ACC")
          (SETQ CANTI4 "SADDLE_PLATE")
	  (SETQ CANTI5 "UB")
          (SETQ CANTI6 "AUXILIARY_TRANSFORMER")
	  (SETQ CANTI NIL)

;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ CANTI CANTI1)")
	  (action_tile "but2" "(SETQ CANTI CANTI2)")
	  (action_tile "but3" "(SETQ CANTI CANTI3)")
          (action_tile "but4" "(SETQ CANTI CANTI4)")
	  (action_tile "but5" "(SETQ CANTI CANTI5)")
          (action_tile "but6" "(SETQ CANTI CANTI6)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (IF (/= CANTI NIL)
	    (COMMAND "INSERT" CANTI (GETPOINT) "1" "0")
	  )
	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)
)



;***********************************FOR DRAWING WIRES************************************************************************;



(defun CONTACTWIRE1 ()
  (prompt "\nPick points to draw a line.")
  (command "._pline")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )
  (princ)
)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun CONTACTWIRE (/ f)

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "CONTACTWIRE.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "contact" dcl_id))
	(progn
	  (alert
	    "The MAST definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn


;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ F 1)")
	  (action_tile "but2" "(SETQ F 2)")
	  (action_tile "but3" "(SETQ F 3)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (prompt "\nPick points to draw a line.")
(alert "DON'T HIT ESC BUTTON WHILE DRAWING.")
	  (COND
	    ((= F 1) (command "._pline"))
	    ((= F 2) (command "._arc"))
	    ((= F 3) (command "._spline"))
	  )
	  (while (> (getvar "CMDACTIVE") 0)
	    (command pause)
	  )
	  (princ)



	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN DRAWCONTACTWIRE (/ ED)

  (CONTACTWIRE)
  (SETQ ED (ENTGET (ENTLAST)))
  (ENTDEL (ENTLAST))
  (SETQ
    ED (SUBST (CONS 8 "CONTACT_WIRE") (ASSOC 8 ED) ED)
  )
  (ENTMAKE ED)
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINEWEIGHT"
    30
  )
)


(DEFUN DRAWOORWIRE (/ ED)

  (CONTACTWIRE)
  (SETQ ED (ENTGET (ENTLAST)))
  (ENTDEL (ENTLAST))
  (SETQ
    ED (SUBST (CONS 8 "OOR_WIRE") (ASSOC 8 ED) ED)
  )
  (ENTMAKE ED)
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINEWEIGHT"
    30
  )
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINETYPE"
    "__________{ Diamond }"
  )
)


(DEFUN DRAWLSWWIRE (/ ED)

  (CONTACTWIRE)
  (SETQ ED (ENTGET (ENTLAST)))
  (ENTDEL (ENTLAST))
  (SETQ
    ED (SUBST (CONS 8 "LARGESPAN_WIRE") (ASSOC 8 ED) ED)
  )
  (ENTMAKE ED)
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINEWEIGHT"
    30
  )
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINETYPE"
    "DASHDOT"
  )
)



(DEFUN DRAWFEEDERWIRE (/ ED)

  (CONTACTWIRE)
  (SETQ ED (ENTGET (ENTLAST)))
  (ENTDEL (ENTLAST))
  (SETQ
    ED (SUBST (CONS 8 "FEEDER_WIRE") (ASSOC 8 ED) ED)
  )
  (ENTMAKE ED)
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINEWEIGHT"
    9
  )
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINETYPE"
    "FEEDER"
  )
)


(DEFUN DRAWAEWIRE (/ ED)

  (CONTACTWIRE)
  (SETQ ED (ENTGET (ENTLAST)))
  (ENTDEL (ENTLAST))
  (SETQ
    ED (SUBST (CONS 8 "ARIALEARTH_WIRE") (ASSOC 8 ED) ED)
  )
  (ENTMAKE ED)
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINEWEIGHT"
    -1
  )
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINETYPE"
    "AEW"
  )
)


(DEFUN DRAWACAWIRE (/ ED)

  (CONTACTWIRE)
  (SETQ ED (ENTGET (ENTLAST)))
  (ENTDEL (ENTLAST))
  (SETQ
    ED (SUBST (CONS 8 "ANTICREEP_WIRE") (ASSOC 8 ED) ED)
  )
  (ENTMAKE ED)
  (VLAX-PUT-PROPERTY
    (VLAX-ENAME->VLA-OBJECT (ENTLAST))
    "LINEWEIGHT"
    -1
  )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun C:DRAWWIRE (/ f)

;;;--- Load the dcl file from disk into memory
  (if (not (setq dcl_id (load_dialog "CONTACTWIRE.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )

;;;--- Else, the file was loaded
    (progn

;;;--- Load the dialog definition inside the DCL file
      (if (not (new_dialog "selectwire" dcl_id))
	(progn
	  (alert
	    "The MAST definition could not be found in the DCL file!"
	  )
	  (exit)
	)

;;;--- Else, the definition was loaded 
	(progn


;;;--- If an action event occurs, do this function
	  (action_tile "but1" "(SETQ F 1)")
	  (action_tile "but2" "(SETQ F 2)")
	  (action_tile "but3" "(SETQ F 3)")
	  (action_tile "but4" "(SETQ F 4)")
	  (action_tile "but5" "(SETQ F 5)")
	  (action_tile "but6" "(SETQ F 6)")
	  (action_tile "cancel" "(done_dialog)")


;;;--- Display the dialog box
	  (start_dialog)

;;;--- Unload the dialog box
	  (unload_dialog dcl_id)
	  (prompt "\nPick points to draw a line.")
	  (COND
	    ((= F 1) (DRAWCONTACTWIRE))
	    ((= F 2) (DRAWOORWIRE))
	    ((= F 3) (DRAWLSWWIRE))
	    ((= F 4) (DRAWACAWIRE))
	    ((= F 5) (DRAWFEEDERWIRE))
	    ((= F 6) (DRAWAEWIRE))
	  )
	  (while (> (getvar "CMDACTIVE") 0)
	    (command pause)
	  )
	  (princ)



	)
      )
    )
  )

;;;--- Suppress the last echo for a clean exit
  (princ)

)


;************************************FUNCTIONS LINKED TO DCL FOR OHE TOOL BAR ICONS ENDS**********************************************************************************;





;********************************LOP DRAFTING****************************************************************************************************************************;






(DEFUN PRINT_OFFSETS (LIST1 / ENT_INFO ENTNAME POINT I )
(SETQ ENTNAME (CAR(ENTSEL)))
(SETQ ENT_INFO (ENTGET ENTNAME))
(SETQ POINT (CDR(ASSOC 10 ENT_INFO)))
(COMMAND "TEXT" (LIST (+ (NTH 0  POINT) (/ (NTH 0 LIST1) 2)) (NTH 1 POINT) 0.0) 5 0 (NTH 0 LIST1) "" "")
(COMMAND "OFFSET" (NTH 0 LIST1) ENTNAME (LIST (+ (NTH 0  POINT) 1) (NTH 1 POINT) 0.0) "")
(SETQ I 1)
(WHILE (< I (- (LENGTH LIST1) 1))
  (SETQ ENTNAME (ENTLAST))
(SETQ POINT (CDR(ASSOC 10 (ENTGET ENTNAME))))
(COMMAND "TEXT" (LIST (+ (NTH 0  POINT) (/ (NTH I LIST1) 2)) (NTH 1 POINT) 0.0) 5 0 (NTH I LIST1) "" "")
(COMMAND "OFFSET" (NTH I LIST1) ENTNAME (LIST (+ (NTH 0  POINT) 1) (NTH 1 POINT) 0.0) "")
  (SETQ I (+ I 1))
  )
  )





(DEFUN GET_MAST_INSERTION_POINTS (SCALE / OFFSET_ENTITIES TRACK_ENTITY I TEMP_ELE1 TEMP_ELE2 LIST2 )
  (PROMPT "SELECT OFFSETS (OFFSET COLOUR CODE : GREEN->SINGLE CANTILEVER YELLOW->DOUBLE CANTILEVER")
  (SETQ OFFSET_ENTITIES1 (SSGET))
  (SETQ TRACK_ENTITY (CAR(ENTSEL "\nSELECT TRACK ALIGNMENT:")))
  (SETQ OFFSET_ENTITIES1 (FORM_SSSET OFFSET_ENTITIES1))
  (SETQ I 0)
  (SETQ OFFSET_ENTITIES NIL)
  (WHILE (< I (LENGTH OFFSET_ENTITIES1))
    (SETQ TEMP_ELE1 (CDR (ASSOC 10 (ENTGET (NTH I OFFSET_ENTITIES1)))))
    (SETQ OFFSET_ENTITIES (CONS (LIST (NTH I OFFSET_ENTITIES1) TEMP_ELE1) OFFSET_ENTITIES))
    (SETQ I (+ I 1))
  )
  (SETQ OFFSET_ENTITIES (REVERSE OFFSET_ENTITIES))
  (SETQ OFFSET_ENTITIES (SORT_FUN OFFSET_ENTITIES 1 0))
  
  (SETQ I 0)
  (SETQ TEMP_ELE1 NIL)
  (SETQ TEMP_ELE2 NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH OFFSET_ENTITIES))
  (SETQ TEMP_ELE1 (NTH 0 (ACET-GEOM-INTERSECTWITH TRACK_ENTITY (NTH 0 (NTH I OFFSET_ENTITIES)) 0.1)))
  (IF (/= (+ I 1) (LENGTH OFFSET_ENTITIES))
  (SETQ TEMP_ELE2 (ATOF  (RTOS (/ (ABS (- (CAR (NTH 1 (NTH (+ I 1) OFFSET_ENTITIES))) (CAR (NTH 1 (NTH I OFFSET_ENTITIES))))) SCALE) 2 1)))
  (SETQ TEMP_ELE2 NIL))
  (SETQ LIST2 (CONS (LIST  TEMP_ELE1 TEMP_ELE2 (NTH I OFFSET_ENTITIES)) LIST2))
  (SETQ I (+ I 1))
  )
  (SORT_FUN (REVERSE LIST2) 0 0)
)







(DEFUN C:INSERT_MASTS ( / SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SCALE)
(SETQ SCALE (GETREAL "\nENTER SCALE : "))
(SETQ SNAP_POINTS (GET_MAST_INSERTION_POINTS SCALE))

(SETQ ENTNAME (CAR(ENTSEL "\nSELECT REFERENCE MAST:")))

(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1" "SGM1 X" "IMM1 X" "SGM1 Y" "IMM1 Y")))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ IMP_REFERENCE (LIST (CADR(ASSOC "IMM1 X" AS)) (CADR(ASSOC "IMM1 Y" AS)) 0.0))
(SETQ SG_REFERENCE (LIST (CADR(ASSOC "SGM1 X" AS)) (CADR(ASSOC "SGM1 Y" AS)) 0.0))
(SETQ SPAN_REFERENCE (- (NTH 1 (NTH 2 (ASSOC "SPAN1" (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO ENTNAME))))))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
(SETQ FLAG (GETSTRING "\nINCLUDE SUPERMAST (Y/N):"))
(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS)))
(SETQ COLOUR_CODE (CDR (ASSOC 62  (ENTGET (NTH 0 (NTH 2 (NTH I SNAP_POINTS)))))))
(IF (= COLOUR_CODE 3)
(PROGN
(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (+ (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "SINGLE_CANT_MAST" INSERTION_POINT "1" "0" )
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0)))))

(SETQ IMP_POINT (ADDITION INSERTION_POINT IMP_REFERENCE))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(SETQ POINT1 (LIST  (CAR TEMP_ELE)  (+  (CADR TEMP_ELE) 175.00) 0.0))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "IMM1") (LIST IMP_POINT))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "SGM1") (LIST STAG_POINT))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "LOM1") (LIST POINT1))
(IF (= FLAG "Y")
(PROGN (COMMAND "INSERT" "SUPER_MAST" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1"))
)
))
(IF (= COLOUR_CODE 2)
(PROGN
  (SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (+ (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "DOUBLE_CANT_MAST" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1")
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2) (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0) (LIST (+ (CAR INSERTION_POINT) (/ (NTH 1 (NTH I SNAP_POINTS)) 2))  (+ (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 3) 0.0)))))
(SETQ IMP_POINT (ADDITION INSERTION_POINT IMP_REFERENCE))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(SETQ POINT1 (LIST  (CAR TEMP_ELE)  (+  (CADR TEMP_ELE) 175.00) 0.0))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "IMM1") (LIST IMP_POINT))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "LOM1") (LIST POINT1))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(IF (= FLAG "Y")
(PROGN (COMMAND "INSERT" "SUPER_MAST" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1"))
)
  ))
(SETQ I (+ I 1))
)
)




(DEFUN C:INSERT_MASTS_DN ( / ENTNAME SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT POINT1 IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SPAN_REFERENCE SCALE)
(SETQ SCALE (GETREAL "\nENTER SCALE : "))
(SETQ SNAP_POINTS (GET_MAST_INSERTION_POINTS SCALE))
(SETQ ENTNAME (CAR(ENTSEL "\nSELECT REFERENCE MAST:")))
(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1" "SGM1 X" "IMM1 X" "SGM1 Y" "IMM1 Y")))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ IMP_REFERENCE (LIST (CADR(ASSOC "IMM1 X" AS)) (CADR(ASSOC "IMM1 Y" AS)) 0.0))
(SETQ SG_REFERENCE (LIST (CADR(ASSOC "SGM1 X" AS)) (CADR(ASSOC "SGM1 Y" AS)) 0.0))
(SETQ SPAN_REFERENCE (- (NTH 1 (NTH 2 (ASSOC "SPAN1" (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO ENTNAME))))))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
(SETQ FLAG (GETSTRING "\nINCLUDE SUPERMAST (Y/N):"))
(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS)))
(SETQ COLOUR_CODE (CDR (ASSOC 62  (ENTGET (NTH 0 (NTH 2 (NTH I SNAP_POINTS)))))))
(IF (= COLOUR_CODE 3)
(PROGN
(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (- (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "SINGLE_CANT_MAST" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1")
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0)))))
(SETQ IMP_POINT (ADDITION INSERTION_POINT IMP_REFERENCE))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(SETQ POINT1 (LIST  (CAR TEMP_ELE)  (-  (CADR TEMP_ELE) 185.00) 0.0))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "IMM1") (LIST IMP_POINT))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "SGM1") (LIST STAG_POINT))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "LOM1") (LIST POINT1))
(IF (= FLAG "Y")
(PROGN (COMMAND "INSERT" "SUPER_MAST" INSERTION_POINT "1" "0" )
))
))
(IF (= COLOUR_CODE 2)
(PROGN
  (SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (- (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "DOUBLE_CANT_MAST" INSERTION_POINT "1" "0" )
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2) (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
  (INSERT_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0) (LIST (+ (CAR INSERTION_POINT) (/ (NTH 1 (NTH I SNAP_POINTS)) 2))  (+ (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 3) 0.0)))))
(SETQ IMP_POINT (ADDITION INSERTION_POINT IMP_REFERENCE))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
 (SETQ POINT1 (LIST  (CAR TEMP_ELE)  (-  (CADR TEMP_ELE) 185.00) 0.0))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "IMM1") (LIST IMP_POINT))
 (INSERT_ATTRIBUTES1 (ENTLAST) (LIST "LOM1") (LIST POINT1))
 (IF (= FLAG "Y")
 (PROGN (COMMAND "INSERT" "SUPER_MAST" INSERTION_POINT "1" "0" )
))
  ))
(SETQ I (+ I 1))
)
)



;****************************************LOP DRAFTING ENDS************************************************************************************************************;


;****************************************YARD LOP DRAFTING FUNCTIONS************************************************************************************************************;



(DEFUN C:INSERT_DA ( / SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SCALE)
(SETQ SCALE (GETREAL "\nENTER SCALE : "))
(SETQ SNAP_POINTS (GET_MAST_INSERTION_POINTS SCALE))
(SETQ ENTNAME (CAR(ENTSEL "\nSELECT REFERENCE MAST:")))
(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1" "SGM1 X" "SGM1 Y" )))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ SG_REFERENCE (LIST (CADR(ASSOC "SGM1 X" AS)) (CADR(ASSOC "SGM1 Y" AS)) 0.0))
(SETQ SPAN_REFERENCE (- (NTH 1 (NTH 2 (ASSOC "SPAN1" (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO ENTNAME))))))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS)))
(SETQ COLOUR_CODE (CDR (ASSOC 62  (ENTGET (NTH 0 (NTH 2 (NTH I SNAP_POINTS)))))))
(IF (= COLOUR_CODE 3)
(PROGN
(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (+ (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "SINGLE_CANT_DA" INSERTION_POINT "1" "0" )
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "SGM1") (LIST STAG_POINT))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
))
(IF (= COLOUR_CODE 2)
(PROGN
  (SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (+ (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "DOUBLE_CANT_DA" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1")
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2) (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0) (LIST (+ (CAR INSERTION_POINT) (/ (NTH 1 (NTH I SNAP_POINTS)) 2))  (+ (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 3) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
  ))
(SETQ I (+ I 1))
)
)




(DEFUN C:INSERT_DA_DN ( / ENTNAME SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT POINT1 IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SPAN_REFERENCE SCALE)
(SETQ SCALE (GETREAL "\nENTER SCALE : "))
(SETQ SNAP_POINTS (GET_MAST_INSERTION_POINTS SCALE))
(SETQ ENTNAME (CAR(ENTSEL "\nSELECT REFERENCE MAST:")))
(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1" "SGM1 X" "SGM1 Y" )))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ SG_REFERENCE (LIST (CADR(ASSOC "SGM1 X" AS)) (CADR(ASSOC "SGM1 Y" AS)) 0.0))
(SETQ SPAN_REFERENCE (- (NTH 1 (NTH 2 (ASSOC "SPAN1" (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO ENTNAME))))))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS)))
(SETQ COLOUR_CODE (CDR (ASSOC 62  (ENTGET (NTH 0 (NTH 2 (NTH I SNAP_POINTS)))))))
(IF (= COLOUR_CODE 3)
(PROGN
(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (- (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "SINGLE_CANT_DA" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1")
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "SGM1") (LIST STAG_POINT))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
))
(IF (= COLOUR_CODE 2)
(PROGN
  (SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (- (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "DOUBLE_CANT_DA" INSERTION_POINT "1" "0" )
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2) (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
  (INSERT_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0) (LIST (+ (CAR INSERTION_POINT) (/ (NTH 1 (NTH I SNAP_POINTS)) 2))  (+ (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 3) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
))
(SETQ I (+ I 1))
)
)







(DEFUN C:UPDATE_CHAINAGES_PORTAL ( / SSSET LOC_NO ST_CH ENT_LIST BS_PT VALUES VALUES1 VALUES2 VALUES3 POINT POINT1 POINT2 POINT3 ENT_LIST_SPAN I LOC CH TEMP_ELE SCALE LOC_NO_DN VALUES_DN VALUES1_DN VALUES2_DN VALUES3_DN POINT_DN POINT1_DN POINT2_DN POINT3_DN LOC_DN CS_REFERENCE_X CS_REFERENCE_Y CS_REFERENCE CS_REFERENCE1 FDN_REFERENCE FDN_REFERENCE1)
  (PROMPT "\n SELECT PORTALS")
  (SETQ SSSET (SSGET))
  (SETQ SCALE (GETREAL "\nENTER SCALE : "))
  (SETQ LOC_NO (GETSTRING "\nENTER STARTING PORTAL LOCATION NUMBER TOWARDS UPLINE SIDE :"))
  (SETQ LOC_NO_DN (GETSTRING "\nENTER STARTING PORTAL LOCATION NUMBER TOWARDS DNLINE SIDE :"))
  (SETQ ST_CH (GETSTRING "\nENTER STARTING PORTAL CHAINAGE :"))(SETQ	KM_NO (SUBSTR LOC_NO 1 (VL-STRING-POSITION (ASCII "/") LOC_NO)))
  (SETQ	LOC_NO	(SUBSTR	LOC_NO (+ (VL-STRING-POSITION (ASCII "/") LOC_NO) 2)))
  (SETQ	LOC_NO_DN	(SUBSTR	LOC_NO_DN (+ (VL-STRING-POSITION (ASCII "/") LOC_NO_DN) 2)))
  (SETQ	ST_CH	(SUBSTR	ST_CH (+ (VL-STRING-POSITION (ASCII "/") ST_CH) 2)))
  (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "PORTAL") (EXTRACT_ENTITY_INFO2 (FORM_SSSET SSSET)) 1) 0))
  (SETQ ENT_LIST (EXTRACT_ENTITY_INFO ENT_LIST 1 3))
  (SETQ ENT_LIST (SORT_FUN ENT_LIST 1 0))
  (SETQ ENT_LIST (SINGLE_ELE_LIST ENT_LIST 0))
  (SETQ CS_REFERENCE (NTH 1 (NTH 0 (GET_TEXT_ALIGNMENT_POINT (LIST (NTH 0 ENT_LIST)) "CS1" 0))))

  (SETQ CS_REFERENCE1 (NTH 1 (NTH 0 (GET_TEXT_ALIGNMENT_POINT (LIST (NTH 0 ENT_LIST)) "CS2" 0))))
  

  (SETQ FDN_REFERENCE (NTH 1 (NTH 0 (GET_TEXT_ALIGNMENT_POINT (LIST (NTH 0 ENT_LIST)) "FOUNDATION_TYPE1" 0))))

  (SETQ FDN_REFERENCE1 (NTH 1 (NTH 0 (GET_TEXT_ALIGNMENT_POINT (LIST (NTH 0 ENT_LIST)) "FOUNDATION_TYPE2" 0))))
  
  (SETQ BS_PT (CDR (ASSOC 10 (ENTGET (NTH 0 ENT_LIST)))))
  (SETQ VALUES (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LOM1 X" "LOM1 Y")))
  (SETQ VALUES1 (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LNM1 X" "LNM1 Y")))
  (SETQ VALUES2 (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LCM1 X" "LCM1 Y")))
  (SETQ VALUES3 (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LTM1 X" "LTM1 Y")))
  (SETQ VALUES_DN (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LOM2 X" "LOM2 Y")))
  (SETQ VALUES1_DN (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LNM2 X" "LNM2 Y")))
  (SETQ VALUES2_DN (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LCM2 X" "LCM2 Y")))
  (SETQ VALUES3_DN (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "LTM2 X" "LTM2 Y")))
  (SETQ POINT (LIST (+ (CAR BS_PT) (CADR (ASSOC "LOM1 X" VALUES))) (+ (CADR BS_PT) (CADR (ASSOC "LOM1 Y" VALUES))) 0.0))
  (SETQ POINT1 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LNM1 X" VALUES1))) (+ (CADR BS_PT) (CADR (ASSOC "LNM1 Y" VALUES1))) 0.0))
  (SETQ POINT2 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LCM1 X" VALUES2))) (+ (CADR BS_PT) (CADR (ASSOC "LCM1 Y" VALUES2))) 0.0))
  (SETQ POINT3 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LTM1 X" VALUES3))) (+ (CADR BS_PT) (CADR (ASSOC "LTM1 Y" VALUES3))) 0.0))
  (SETQ POINT_DN (LIST (+ (CAR BS_PT) (CADR (ASSOC "LOM2 X" VALUES_DN))) (+ (CADR BS_PT) (CADR (ASSOC "LOM2 Y" VALUES_DN))) 0.0))
  (SETQ POINT1_DN (LIST (+ (CAR BS_PT) (CADR (ASSOC "LNM2 X" VALUES1_DN))) (+ (CADR BS_PT) (CADR (ASSOC "LNM2 Y" VALUES1_DN))) 0.0))
  (SETQ POINT2_DN (LIST (+ (CAR BS_PT) (CADR (ASSOC "LCM2 X" VALUES2_DN))) (+ (CADR BS_PT) (CADR (ASSOC "LCM2 Y" VALUES2_DN))) 0.0))
  (SETQ POINT3_DN (LIST (+ (CAR BS_PT) (CADR (ASSOC "LTM2 X" VALUES3_DN))) (+ (CADR BS_PT) (CADR (ASSOC "LTM2 Y" VALUES3_DN))) 0.0))
  (SETQ ENT_LIST_SPAN (GET_PORTAL_SPAN ENT_LIST))
   (setq dcl_id (load_dialog "DISPLAY_INFO.dcl"))
   (new_dialog "DISPLAY_INFO1" dcl_id)
  
  (SETQ I 0)
  (WHILE (< I (LENGTH ENT_LIST_SPAN))
    (SETQ LOC (STRCAT KM_NO "/" LOC_NO))
    (SETQ LOC_DN (STRCAT KM_NO "/" LOC_NO_DN))
    (SETQ CH (STRCAT KM_NO "/" ST_CH))
    (SETQ TEMP_ELE  (NTH I ENT_LIST_SPAN))
    (SETQ BS_PT1 (CDR (ASSOC 10 (ENTGET (NTH 0 TEMP_ELE)))))
    (MODIFY_ATTRIBUTES (NTH 0 TEMP_ELE) (LIST "MAST_NUMBER1" "CHAINAGE1") (LIST LOC CH))
    (MODIFY_ATTRIBUTES (NTH 0 TEMP_ELE) (LIST "MAST_NUMBER2" "CHAINAGE2") (LIST LOC_DN CH))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LOM1") (LIST POINT))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LNM1") (LIST POINT1))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LCM1") (LIST POINT2))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LTM1") (LIST POINT3))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LOM2") (LIST POINT_DN))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LNM2") (LIST POINT1_DN))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LCM2") (LIST POINT2_DN))
    (INSERT_ATTRIBUTES1 (NTH 0 TEMP_ELE) (LIST "LTM2") (LIST POINT3_DN))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH 0 TEMP_ELE) '("CS1") (LIST CS_REFERENCE))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH 0 TEMP_ELE) '("CS2") (LIST CS_REFERENCE1))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH 0 TEMP_ELE) '("FOUNDATION_TYPE1") (LIST FDN_REFERENCE))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH 0 TEMP_ELE) '("FOUNDATION_TYPE2") (LIST FDN_REFERENCE1))
     (SET_TILE "INFO" (STRCAT "UPDATING LOC NOS' "  LOC "&" LOC_DN  "OF "  (ITOA I)))
     (SET_TILE "PERCENTAGE" (STRCAT "UPDATING CHAINAGE " CH))
    (IF (= I (- (LENGTH ENT_LIST_SPAN) 1))
      (PROGN
      (SET_TILE "INFO" (STRCAT "ALL "  (ITOA I) " PORTAL LOCATIONS ARE UPDATED SUCESSFULLY"))
      (SET_TILE "PERCENTAGE" "PRESS OK TO VIEW RESULTS")
      )
    )
    (IF (/= (ATOF (NTH 1 TEMP_ELE)) 0)
    (PROGN
    (SETQ LOC_NO (ITOA (+ (ATOI LOC_NO) 2)))
    (SETQ LOC_NO_DN (ITOA (+ (ATOI LOC_NO_DN) 2)))
    (SETQ ST_CH (RTOS (+ (ATOF ST_CH) (ATOF (NTH 1 TEMP_ELE))) 2 2))
    (SETQ POINT (LIST (+ (CAR POINT) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT) (CADDR POINT)))
    (SETQ POINT1 (LIST (+ (CAR POINT1) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT1) (CADDR POINT1)))
    (SETQ POINT2 (LIST (+ (CAR POINT2) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT2) (CADDR POINT2)))
    (SETQ POINT3 (LIST (+ (CAR POINT3) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT3) (CADDR POINT3)))
    (SETQ POINT_DN (LIST (+ (CAR POINT_DN) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT_DN) (CADDR POINT_DN)))
    (SETQ POINT1_DN (LIST (+ (CAR POINT1_DN) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT1_DN) (CADDR POINT1_DN)))
    (SETQ POINT2_DN (LIST (+ (CAR POINT2_DN) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT2_DN) (CADDR POINT2_DN)))
    (SETQ POINT3_DN (LIST (+ (CAR POINT3_DN) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR POINT3_DN) (CADDR POINT3_DN)))
    (SETQ CS_REFERENCE (LIST (+ (CAR CS_REFERENCE) (* (ATOF (NTH 1 TEMP_ELE)) SCALE))  (CADR CS_REFERENCE) 0.0))
    (SETQ CS_REFERENCE1 (LIST (+ (CAR CS_REFERENCE1) (* (ATOF (NTH 1 TEMP_ELE)) SCALE))  (CADR CS_REFERENCE1) 0.0))
    (SETQ FDN_REFERENCE (LIST (+ (CAR FDN_REFERENCE) (* (ATOF (NTH 1 TEMP_ELE)) SCALE))  (CADR FDN_REFERENCE) 0.0))
    (SETQ FDN_REFERENCE1 (LIST (+ (CAR FDN_REFERENCE1) (* (ATOF (NTH 1 TEMP_ELE)) SCALE)) (CADR FDN_REFERENCE1) 0.0))
    )
    (PROGN
    (SETQ I (LENGTH ENT_LIST_SPAN))
     (IF (/= I (- (LENGTH ENT_LIST_SPAN) 1)) (PROGN (SET_TILE "INFO" (STRCAT "SPAN HAS NOT BEEN ENTERED PROPERLY OR UNEQUAL SPANS FOUND IN LOC NOS' "  LOC " & " LOC_DN  ))
     (SET_TILE "PERCENTAGE" (STRCAT "PROGRAM STOPPED AT CHAINAGE " CH " PLEASE CHECK..."))))
    )
    )
    (SETQ I (+ I 1))
    )(setq ddiag(start_dialog))
)




(DEFUN REP_REMOVE (LIST1 / TEMP_ELE TEMP_LIST NEW_LIST)
  (SETQ TEMP_LIST LIST1)
  (SETQ NEW_LIST NIL)
  (WHILE (/= TEMP_LIST NIL)
    (SETQ TEMP_ELE (CAR TEMP_LIST))
    (SETQ TEMP_LIST (VL-REMOVE TEMP_ELE TEMP_LIST))
    (SETQ NEW_LIST (CONS TEMP_ELE NEW_LIST))
  )
  (SETQ NEW_LIST (REVERSE NEW_LIST))
)


(DEFUN GET_MAX_LENGTH_ELE (LIST1 / LIST2 I TEMP_ELE1 LIST3)
  (SETQ LIST2 (REP_REMOVE LIST1))
  (SETQ I 0)
  (SETQ TEMP_ELE1 NIL)
  (SETQ LIST3 NIL)
  (WHILE (< I (LENGTH LIST2))
    (SETQ TEMP_ELE1 (LENGTH (FILTER_LIST (LIST (NTH I LIST2)) (ADD_LISTS1 LIST1 (N_ELE_LIST '(1) (LENGTH LIST2))) 0)))
    (SETQ LIST3 (CONS (LIST TEMP_ELE1 (NTH I LIST2)) LIST3))
    (SETQ I (+ I 1))
  )
  (NTH 1 (NTH 0 (REVERSE (SORT_FUN (REVERSE LIST3) 0 0))))
)




(DEFUN GET_PORTAL_SPAN (LIST1 / STRUCTURE_TYPE  CANTILEVER_TYPE  CANTILEVERS  SPAN_LIST LIST2 )
(SETQ I 0)
(SETQ STRUCTURE_TYPE NIL CANTILEVER_TYPE NIL CANTILEVERS NIL SPAN_LIST NIL LIST2 NIL)
(WHILE (< I (LENGTH LIST1))
  (SETQ STRUCTURE_TYPE (YARD_STRUCTURE_INFO (NTH I LIST1)))
  (SETQ CANTILEVER_TYPE (NTH 1 (ASSOC 6 STRUCTURE_TYPE)))
  (SETQ CANTILEVERS (SINGLE_ELE_LIST (FILTER_LIST '("SINGLE_CANT_DA" "DOUBLE_CANT_DA" "TRIPLE_CANT_DA" "SINGLE_CANT_UPRIGHT" "DOUBLE_CANT_UPRIGHT" "TRIPLE_CANT_UPRIGHT") (EXTRACT_ENTITY_INFO CANTILEVER_TYPE 1 2) 1) 0))
  (SETQ SPAN_LIST (REP_REMOVE (SINGLE_ELE_LIST (ATTRIBUTES_FROM_ENTITIES CANTILEVERS "SPAN1" 0) 1)))
  (SETQ LIST2 (CONS (LIST (NTH I LIST1) (IF (= (LENGTH SPAN_LIST) 1) (NTH 0 SPAN_LIST) "XX")) LIST2))
  (SETQ I (+ I 1))
)
(REVERSE LIST2)
)



(DEFUN GET_TEXT_ALIGNMENT_POINT (ENTNAME_LIST ATTRIBUTE_TAG_NAME FLAG / I LIST1 ENTNAME_LIST TEMP_ELE SAFEARRAY_SET ENT_OBJECT J)
  (SETQ I 0)
  (SETQ LIST1 NIL)

(WHILE (< I (LENGTH ENTNAME_LIST))
  (SETQ TEMP_ELE NIL)
  (SETQ SAFEARRAY_SET NIL) 
  (IF (NOT (ATOM (NTH I ENTNAME_LIST))) (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT (NTH FLAG (NTH I ENTNAME_LIST)))) (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT (NTH I ENTNAME_LIST))))

(IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
    (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

(SETQ J 0)
(SETQ TEMP_ELE NIL)

(WHILE (< J (LENGTH SAFEARRAY_SET))
  (IF (= (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TAGSTRING") ATTRIBUTE_TAG_NAME)
  (PROGN (IF (ATOM (NTH I ENTNAME_LIST))
    (SETQ TEMP_ELE (LIST (NTH I ENTNAME_LIST) (VLAX-SAFEARRAY->LIST (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TextAlignmentPoint")))))
    (SETQ TEMP_ELE (APPEND (NTH I ENTNAME_LIST) (LIST (VLAX-SAFEARRAY->LIST (VLAX-VARIANT-VALUE (VLAX-GET-PROPERTY (NTH J SAFEARRAY_SET) "TextAlignmentPoint"))))))
  )))
    
    (SETQ J (+ J 1))

)))
(SETQ LIST1 (CONS TEMP_ELE LIST1))
(SETQ I (+ I 1))

)
(REVERSE LIST1)
)



(DEFUN CHANGE_TEXT_ALIGNMENT_POINT (ENTNAME IDENTIFIER POINT / TEMP_ELE ENT_OBJECT SAFEARRAY_SET I J LIST1)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT)) 
(IF (= (VLAX-GET-PROPERTY ENT_OBJECT "HASATTRIBUTES") :VLAX-TRUE)
  (PROGN
  (SETQ	SAFEARRAY_SET
	 (VLAX-SAFEARRAY->LIST
	   (VLAX-VARIANT-VALUE
	     (VLAX-INVOKE-METHOD ENT_OBJECT "GETATTRIBUTES")
	   )
	 )
  )

(SETQ I 0)
  (SETQ J 0)
  (SETQ LIST1 NIL)
(WHILE (< I (LENGTH SAFEARRAY_SET))
    (SETQ TEMP_ELE (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "TAGSTRING"))
    (IF (/= (VL-POSITION TEMP_ELE IDENTIFIER) NIL) (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "TextAlignmentPoint" (MAKE_SAFEARRAY (NTH (VL-POSITION TEMP_ELE IDENTIFIER) POINT))) ))
    (SETQ I (+ I 1))
  )
)))








(DEFUN C:INSERT_UR ( / SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SCALE UR_WIDTH)
(SETQ SCALE (GETREAL "ENTER SCALE : "))
(SETQ SNAP_POINTS (GET_MAST_INSERTION_POINTS SCALE))
(SETQ ENTNAME (CAR(ENTSEL "SELECT REFERENCE MAST:")))
(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1" "SGM1 X" "SGM1 Y" "STS1")))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ UR_WIDTH (CADR(ASSOC "STS1" AS)))
(SETQ SG_REFERENCE (LIST (CADR(ASSOC "SGM1 X" AS)) (CADR(ASSOC "SGM1 Y" AS)) 0.0))
(SETQ SPAN_REFERENCE (- (NTH 1 (NTH 2 (ASSOC "SPAN1" (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO ENTNAME))))))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS)))
(SETQ COLOUR_CODE (CDR (ASSOC 62  (ENTGET (NTH 0 (NTH 2 (NTH I SNAP_POINTS)))))))
(IF (= COLOUR_CODE 3)
  (PROGN
(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (+ (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "SINGLE_CANT_UPRIGHT" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1")
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "SGM1") (LIST STAG_POINT))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STS1") (LIST UR_WIDTH))
))
;;;
(IF (= COLOUR_CODE 2)
(PROGN
  (SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (+ (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "DOUBLE_CANT_UPRIGHT" INSERTION_POINT "1" "0" )
(CHANGE_FLIP (ENTLAST) "STF1")
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2) (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0) (LIST (+ (CAR INSERTION_POINT) (/ (NTH 1 (NTH I SNAP_POINTS)) 2))  (+ (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 3) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STS1") (LIST UR_WIDTH))
  ))
(SETQ I (+ I 1))
)
)




(DEFUN C:INSERT_UR_DN ( / ENTNAME SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT POINT1 IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SPAN_REFERENCE SCALE UR_WIDTH)
(SETQ SCALE (GETREAL "ENTER SCALE : "))
(SETQ SNAP_POINTS (GET_MAST_INSERTION_POINTS SCALE))
(SETQ ENTNAME (CAR(ENTSEL "SELECT REFERENCE MAST:")))
(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1" "SGM1 X" "SGM1 Y" "STS1" )))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ UR_WIDTH (CADR(ASSOC "STS1" AS)))
(SETQ SG_REFERENCE (LIST (CADR(ASSOC "SGM1 X" AS)) (CADR(ASSOC "SGM1 Y" AS)) 0.0))
(SETQ SPAN_REFERENCE (- (NTH 1 (NTH 2 (ASSOC "SPAN1" (NTH 1 (ASSOC 4 (YARD_STRUCTURE_INFO ENTNAME))))))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS)))
(SETQ COLOUR_CODE (CDR (ASSOC 62  (ENTGET (NTH 0 (NTH 2 (NTH I SNAP_POINTS)))))))
(IF (= COLOUR_CODE 1)
(PROGN
(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (- (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "SINGLE_CANT_UPRIGHT" INSERTION_POINT "1" "0" )
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (RTOS (NTH 1 (NTH I SNAP_POINTS)) 2 2)))
(INSERT_ATTRIBUTES (ENTLAST) '("SPAN1") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "SGM1") (LIST STAG_POINT))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STS1") (LIST UR_WIDTH))
)
(PROGN
  (SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (- (CADR TEMP_ELE) MAST_LENGTH) 0.0))
(COMMAND "INSERT" "DOUBLE_CANT_UPRIGHT" INSERTION_POINT "1" "0" )
(IF (/= I (- (LENGTH SNAP_POINTS) 1)) (PROGN (MODIFY_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (NTH 1 (NTH I SNAP_POINTS)) (NTH 1 (NTH I SNAP_POINTS))))
  (INSERT_ATTRIBUTES (ENTLAST) '("SPAN1" "SPAN2") (LIST (LIST (+ (CAR INSERTION_POINT) (* (/ (NTH 1 (NTH I SNAP_POINTS)) 2) SCALE))  (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 0.0) (LIST (+ (CAR INSERTION_POINT) (/ (NTH 1 (NTH I SNAP_POINTS)) 2))  (+ (+ (CADR INSERTION_POINT) SPAN_REFERENCE) 3) 0.0)))))
(SETQ STAG_POINT (ADDITION INSERTION_POINT SG_REFERENCE))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STS1") (LIST UR_WIDTH))
))
(SETQ I (+ I 1))
)
)

;****************************************YARD LOP DRAFTING FUNCTIONS ENDS************************************************************************************************************;






(DEFUN C:INSERT_PORTAL ( / SNAP_POINTS I TEMP_ELE INSERTION_POINT MAST_LENGTH VALUES POINT IMP_REFERENCE SG_REFERENCE FLAG IMP_POINT STAG_POINT SCALE UR_WIDTH)
(SETQ SCALE (GETREAL "ENTER SCALE : "))
(SETQ SNAP_POINTS1 (GET_MAST_INSERTION_POINTS SCALE))
(SETQ ENTNAME (CAR(ENTSEL "SELECT REFERENCE PORTAL:")))
(SETQ AS (GET_DYNAMIC_PROPERTIES ENTNAME '("STL1")))
(SETQ MAST_LENGTH (CADR(ASSOC "STL1" AS)))
(SETQ UR_WIDTH (CADR(ASSOC "STS1" AS)))

(SETQ I 0)
(WHILE (< I (LENGTH SNAP_POINTS1))
(SETQ TEMP_ELE (NTH 0 (NTH I SNAP_POINTS1)))


(SETQ INSERTION_POINT (LIST (CAR TEMP_ELE) (CADR TEMP_ELE) 0.0))
(COMMAND "INSERT" "PORTAL" INSERTION_POINT "1" "0" )
(CHANGE_DYNAMIC_PROPERTIES (ENTLAST) '("STL1") (LIST MAST_LENGTH))
(COMMAND "MOVE" (ENTLAST) "" (CDR (ASSOC 10 (ENTGET (ENTLAST)))) INSERTION_POINT)
(SETQ POINT1 (LIST  (CAR TEMP_ELE)  (+  (CADR TEMP_ELE) 350.00) 0.0))
(SETQ POINT2 (LIST  (CAR TEMP_ELE)  (-  (CADR TEMP_ELE) 350.00) 0.0))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "LOM1") (LIST POINT1))
(INSERT_ATTRIBUTES1 (ENTLAST) (LIST "LOM2") (LIST POINT2))


(SETQ I (+ I 1))
)
)



;                                                                                                                                                                  ;
;                                                            VISIBILITY MANUPULATION FUNCTIONS                                                                     ;
;                                                                                                                                                                  ;

(defun c:invis_all (/ LIST1 FLAG)
  (vl-load-com)
  (SETQ  FLAG (GETSTRING "SELECT VISIBILITY STATE 0/1 VISIBLE-->1 INVISIBLE---->0 :- "))
  (SETQ FLAG1 (GETSTRING "/N SELECT ATTRIBUTE--->S OR ENTER TAG NAME-->E"))
  (IF (= FLAG1 "S")
   (SETQ ATTR_TAG (CDR (ASSOC 2 (ENTGET (CAR (NENTSEL "SELECT ATTRIBUTE"))))))
   (SETQ ATTR_TAG (GETSTRING "/N ENTER TAGNAME : ")))
  (SETQ LIST1 (SSGET))
  (SETQ LIST1 (FORM_SSSET LIST1))
  (SETQ I 0)
  (while (< I (LENGTH LIST1))
      (CHANGE_VISIBILITY (NTH I LIST1) (LIST ATTR_TAG) (LIST FLAG))
      (SETQ  I (+ I 1)))
  (princ))



(defun c:invis (/ ent obj)
  (vl-load-com)

  (while (setq ent (car (nentsel "\nSelect Attribute: ")))
    (if (eq "ATTRIB" (cdr (assoc 0 (entget ent))))
      (vlax-put (setq obj (vlax-ename->vla-object ent)) 'invisible -1)))

  (princ))


;                                                                                                                                                                  ;
;                                                            VISIBILITY MANUPULATION FUNCTIONS ENDS                                                                ;
;                                                                                                                                                                  ;

;DYNAMIC PROPERTY FUNCTION

(DEFUN INSERT_DYNAMIC_PROPERTIES (ENTNAME IDENTIFIER VALUE  / TEMP_ELE ENT_OBJECT SAFEARRAY_SET  I J LIST1 X)
  (SETQ SAFEARRAY_SET NIL)
  (SETQ ENT_OBJECT ENTNAME)
  
  (SETQ ENT_OBJECT (VLAX-ENAME->VLA-OBJECT ENT_OBJECT)) 

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
      (PROGN
	  (SETQ X (VLAX-GET-PROPERTY (NTH I SAFEARRAY_SET) "VALUE"))
	  (COND ((AND (/= (TYPE X) 'variant) (/= (TYPE X) 'SAFEARRAY)) (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE"  (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) ))
		( (= (TYPE X) 'variant)  (IF (= (TYPE (VLAX-VARIANT-VALUE X)) 'INT) (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (vlax-make-variant (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) vlax-vbinteger) ))
		                        (IF (= (TYPE (VLAX-VARIANT-VALUE X)) 'REAL) (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (vlax-make-variant (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) vlax-vbdouble) ))
		                        (IF (= (TYPE (VLAX-VARIANT-VALUE X)) 'STR) (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (vlax-make-variant (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE) vlax-vbstring) ))
		                        (IF (= (TYPE (VLAX-VARIANT-VALUE X)) 'safearray) (PROGN (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (MAKE_SAFEARRAY (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE))) ))
		)
		( (= (TYPE X) 'SAFEARRAY) (VLAX-PUT-PROPERTY (NTH I SAFEARRAY_SET) "VALUE" (MAKE_SAFEARRAY (NTH (VL-POSITION TEMP_ELE IDENTIFIER) VALUE))))
	  )
      )
    )
    (SETQ I (+ I 1))
  )
)


;                                                                                                                                                                  ;
;                                                            DRAFTING REDUCTION FUNCTIONS                                                                          ;
;                                                            MATCH BLOCKS AND SPAN UPDATE FUNCTIONS                                                                ;


(DEFUN C:MATCH_BLOCKS ( / ENTNAME EFF_NAME SCALE SSSET ENT_LIST ENT_INFO FDN_REFERENCE FDN_REFERENCE_X FDN_REFERENCE_Y RD_REFERENCE RD_REFERENCE_X RD_REFERENCE_Y TC_REFERENCE TC_REFERENCE_X TC_REFERENCE_Y SPAN_REFERENCE_X SPAN_REFERENCE_Y BS_PT VALUES VALUES1 VALUES2 VALUES3 VALUES4 VALUES5 POINT POINT1 POINT2 POINT3 POINT4 POINT5 VISIBILITY_SPAN VISIBILITY_TC VISIBILITY_FDN VISIBILITY_RD I BASE_POINT TEMP_SPAN) 
  (SETQ ENTNAME (CAR(ENTSEL "\n SELECT REFERENCE MAST:")))
  (SETQ EFF_NAME (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ENTNAME) 'EFFECTIVENAME))
  (SETQ SCALE (GETINT "/n ENTER SCALE"))
  (PROMPT "SELECT STRUCTURES (MASTS/DA'S/UPRIGHT CANTILEVERS")
  (SETQ SSSET (SSGET))
  (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "SINGLE_CANT_DA" "DOUBLE_CANT_DA" "SINGLE_CANT_UR" "DOUBLE_CANT_UR") (EXTRACT_ENTITY_INFO2 (FORM_SSSET SSSET)) 1) 0))
  (SETQ ENT_LIST (EXTRACT_ENTITY_INFO ENT_LIST 1 3))
  (SETQ ENT_LIST (SORT_FUN ENT_LIST 1 0))
  (SETQ ENT_LIST (SINGLE_ELE_LIST ENT_LIST 0))
  
  (SETQ ENT_INFO (YARD_STRUCTURE_INFO ENTNAME))
  (SETQ BS_PT (CDR (ASSOC 10 (ENTGET ENTNAME))))
  (IF (/= (VL-POSITION EFF_NAME (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST")) NIL)
  (PROGN
  (SETQ FDN_REFERENCE (GET_TEXT_ALIGNMENT_POINT (LIST ENTNAME) "FOUNDATION_TYPE1" 0))
  (SETQ FDN_REFERENCE_X (- (NTH 0 (NTH 1 (NTH 0 FDN_REFERENCE)))  (NTH 0 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ FDN_REFERENCE_Y (- (NTH 1 (NTH 1 (NTH 0 FDN_REFERENCE)))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ RD_REFERENCE (GET_TEXT_ALIGNMENT_POINT (LIST ENTNAME) "RD1" 0))
  (SETQ RD_REFERENCE_X (- (NTH 0 (NTH 1 (NTH 0 RD_REFERENCE)))  (NTH 0 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ RD_REFERENCE_Y (- (NTH 1 (NTH 1 (NTH 0 RD_REFERENCE)))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ TC_REFERENCE (GET_TEXT_ALIGNMENT_POINT (LIST ENTNAME) "TC1" 0))
  (SETQ TC_REFERENCE_X (- (NTH 0 (NTH 1 (NTH 0 TC_REFERENCE)))  (NTH 0 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ TC_REFERENCE_Y (- (NTH 1 (NTH 1 (NTH 0 TC_REFERENCE)))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))

  
  (SETQ VALUES (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "LOM1 X" "LOM1 Y")))
  (SETQ VALUES1 (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "LNM1 X" "LNM1 Y")))
  (SETQ VALUES2 (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "LCM1 X" "LCM1 Y")))
  (SETQ VALUES3 (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "LTM1 X" "LTM1 Y")))
  
  (SETQ VALUES5 (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "IMM1 X" "IMM1 Y")))
  (SETQ POINT (LIST (+ (CAR BS_PT) (CADR (ASSOC "LOM1 X" VALUES))) (+ (CADR BS_PT) (CADR (ASSOC "LOM1 Y" VALUES))) 0.0))
  (SETQ POINT1 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LNM1 X" VALUES1))) (+ (CADR BS_PT) (CADR (ASSOC "LNM1 Y" VALUES1))) 0.0))
  (SETQ POINT2 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LCM1 X" VALUES2))) (+ (CADR BS_PT) (CADR (ASSOC "LCM1 Y" VALUES2))) 0.0))
  (SETQ POINT3 (LIST (+ (CAR BS_PT) (CADR (ASSOC "LTM1 X" VALUES3))) (+ (CADR BS_PT) (CADR (ASSOC "LTM1 Y" VALUES3))) 0.0))
  (SETQ POINT5 (LIST (+ (CAR BS_PT) (CADR (ASSOC "IMM1 X" VALUES5))) (+ (CADR BS_PT) (CADR (ASSOC "IMM1 Y" VALUES5))) 0.0))

  (SETQ VISIBILITY_FDN (NTH 0 (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "FOUNDATION_TYPE1" ) 3) 1)))
  (SETQ VISIBILITY_RD (NTH 0 (SINGLE_ELE_LIST (EXTRACT_ATTRIBUTES ENTNAME (LIST "RD1" ) 3) 1)))
  ))
  
  
  (SETQ SPAN_REFERENCE (GET_TEXT_ALIGNMENT_POINT (LIST ENTNAME) "SPAN1" 0))
  (SETQ SPAN_REFERENCE_X (- (NTH 0 (NTH 1 (NTH 0 SPAN_REFERENCE)))  (NTH 0 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ SPAN_REFERENCE_Y (- (NTH 1 (NTH 1 (NTH 0 SPAN_REFERENCE)))  (NTH 1 (CDR (ASSOC 10 (ENTGET ENTNAME))))))
  (SETQ VALUES4 (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "SGM1 X" "SGM1 Y")))
  (SETQ POINT4 (LIST (+ (CAR BS_PT) (CADR (ASSOC "SGM1 X" VALUES4))) (+ (CADR BS_PT) (CADR (ASSOC "SGM1 Y" VALUES4))) 0.0))

  


  (SETQ I 0)
  (WHILE (< I (LENGTH ENT_LIST))
    (SETQ BASE_POINT (CDR (ASSOC 10 (ENTGET (NTH I ENT_LIST)))))
    (SETQ EFF_NAME (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT (NTH I ENT_LIST)) 'EFFECTIVENAME))
  (IF (/= (VL-POSITION EFF_NAME (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST")) NIL)
  (PROGN
  (SETQ POINT (LIST (NTH 0 BASE_POINT) (NTH 1 POINT) 0.0))
  (SETQ POINT1 (LIST (NTH 0 BASE_POINT) (NTH 1 POINT1) 0.0))
  (SETQ POINT2 (LIST (NTH 0 BASE_POINT) (NTH 1 POINT2) 0.0))
  (SETQ POINT3 (LIST (NTH 0 BASE_POINT) (NTH 1 POINT3) 0.0))
  (SETQ POINT5 (LIST (+ (CAR BASE_POINT) (CADR (ASSOC "IMM1 X" VALUES5))) (+ (CADR BASE_POINT) (CADR (ASSOC "IMM1 Y" VALUES5))) 0.0))
    (INSERT_ATTRIBUTES1 (NTH I ENT_LIST) (LIST "LOM1") (LIST POINT))
    (INSERT_ATTRIBUTES1 (NTH I ENT_LIST) (LIST "LNM1") (LIST POINT1))
    (INSERT_ATTRIBUTES1 (NTH I ENT_LIST) (LIST "LCM1") (LIST POINT2))
    (INSERT_ATTRIBUTES1 (NTH I ENT_LIST) (LIST "LTM1") (LIST POINT3))
    (IF (/= EFF_NAME "DOUBLE_CANT_MAST") (INSERT_ATTRIBUTES1 (NTH I ENT_LIST) (LIST "IMM1") (LIST POINT5)))
    ;(INSERT_DYNAMIC_PROPERTIES (NTH I ENT_LIST) (LIST "IMM1 Y") (LIST  (CADR (ASSOC "IMM1 Y" VALUES5))))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH I ENT_LIST) '("FOUNDATION_TYPE1") (LIST (LIST (CAR POINT)  (+ (CADR BS_PT) FDN_REFERENCE_Y) 0.0)))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH I ENT_LIST) '("RD1") (LIST (LIST (CAR POINT)  (+ (CADR BS_PT) RD_REFERENCE_Y) 0.0)))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH I ENT_LIST) '("TC1") (LIST (LIST (+ (CAR BASE_POINT) TC_REFERENCE_X)  (+ (CADR BASE_POINT) TC_REFERENCE_Y) 0.0)))
    (CHANGE_VISIBILITY (NTH I ENT_LIST) (LIST "FOUNDATION_TYPE1" "RD1") (LIST (ITOA VISIBILITY_FDN) (ITOA VISIBILITY_RD)))
   ))
    
    (SETQ POINT4 (LIST (+ (CAR BASE_POINT) (CADR (ASSOC "SGM1 X" VALUES4))) (NTH 1 POINT4) 0.0))
    (IF (/= (VL-POSITION EFF_NAME (LIST "SINGLE_CANT_MAST" "SINGLE_CANT_DA" "SINGLE_CANT_UR")) NIL)  (INSERT_ATTRIBUTES1 (NTH I ENT_LIST) (LIST "SGM1") (LIST POINT4)))
    (SETQ TEMP_SPAN (NTH 1 (NTH 0 (EXTRACT_ATTRIBUTES (NTH I ENT_LIST) (LIST "SPAN1") 1))))
    (CHANGE_TEXT_ALIGNMENT_POINT (NTH I ENT_LIST) '("SPAN1") (LIST (LIST (IF (/= (ATOF TEMP_SPAN) 0) (+ (CAR BASE_POINT) (* (/ (ATOF TEMP_SPAN) 2) SCALE)) (+ (CAR BASE_POINT) SPAN_REFERENCE_X))  (+ (CADR BASE_POINT) SPAN_REFERENCE_Y) 0.0)))
    
    (SETQ I (+ I 1))
  )
  )




(DEFUN C:UPDATE_SPANS ( / SSSET SCALE ENT_LIST I BASE_POINT BASE_POINT1 SPAN)
   (PROMPT "\n SELECT MASTS/DAS'/UPRIGHT CANTILEVERS' (MAIN LINE OR LOOP LINE) (MASTS/DAS'/UPRIGHT CANTILEVERS' SHOULD BE IN THE SAME ALIGNMENT)")
   (SETQ SSSET (SSGET))
   (SETQ SCALE (GETINT "\n ENTER SCALE"))
   (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "SINGLE_CANT_DA" "DOUBLE_CANT_DA" "TRIPLE_CANT_DA" "SINGLE_CANT_UR" "DOUBLE_CANT_UR" "TRIPLE_CANT_UR" "SS0" "SS1" "SS2" "SS3") (EXTRACT_ENTITY_INFO2 (FORM_SSSET SSSET)) 1) 0))
   (SETQ ENT_LIST (EXTRACT_ENTITY_INFO ENT_LIST 1 3))
   (SETQ ENT_LIST (SORT_FUN ENT_LIST 1 0))
   (SETQ ENT_LIST (SINGLE_ELE_LIST ENT_LIST 0))
   (SETQ I 0)
  (WHILE (< I (LENGTH ENT_LIST))
    (SETQ BASE_POINT (CDR (ASSOC 10 (ENTGET (NTH I ENT_LIST)))))
    (IF (/= (NTH (+ I 1) ENT_LIST) NIL)
    (PROGN (SETQ BASE_POINT1 (CDR (ASSOC 10 (ENTGET (NTH (+ I 1) ENT_LIST)))))
    (SETQ SPAN (/ (- (NTH 0  BASE_POINT1) (NTH 0  BASE_POINT)) SCALE))
    (SETQ SPAN (RTOS SPAN 2 2))
    (MODIFY_ATTRIBUTES (NTH I ENT_LIST) (LIST "SPAN1" "SPAN2" "SPAN3" ) (LIST SPAN SPAN SPAN))))
    (SETQ I (+ I 1))
  )
)


(DEFUN C:GIVE_STAGGERING ( / SSSET ENT_LIST I FLAG1 FLAG2)
   (PROMPT "\n SELECT MASTS/DAS'/UPRIGHT CANTILEVERS' (MASTS/DAS'/UPRIGHT CANTILEVERS' SHOULD BE IN THE SAME ALIGNMENT)")
   (SETQ SSSET (SSGET))
   (SETQ ENT_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "SINGLE_CANT_DA" "DOUBLE_CANT_DA" "TRIPLE_CANT_DA" "SINGLE_CANT_UR" "DOUBLE_CANT_UR" "TRIPLE_CANT_UR") (EXTRACT_ENTITY_INFO2 (FORM_SSSET SSSET)) 1) 0))
   (SETQ ENT_LIST (EXTRACT_ENTITY_INFO ENT_LIST 1 3))
   (SETQ ENT_LIST (SORT_FUN ENT_LIST 1 0))
   (SETQ ENT_LIST (SINGLE_ELE_LIST ENT_LIST 0))
   (SETQ FLAG1 (NTH 1 (ASSOC "SGF1" (GET_DYNAMIC_PROPERTIES (NTH 0 ENT_LIST) (LIST "SGF1")))))
   (SETQ FLAG2 (IF (= FLAG1 1) 0 1))
   (SETQ I 0)
  (WHILE (< I (LENGTH ENT_LIST))
    (IF (= (REM I 2) 0) (INSERT_DYNAMIC_PROPERTIES (NTH I ENT_LIST) (LIST "SGF1") (LIST FLAG1)))
    (IF (= (REM I 2) 1) (INSERT_DYNAMIC_PROPERTIES (NTH I ENT_LIST) (LIST "SGF1") (LIST FLAG2)))
    (SETQ I (+ I 1))
  )
)


;                                                                                                                                                                  ;
;                                                            DRAFTING REDUCTION FUNCTIONS ENDS                                                                     ;
;                                                            MATCH BLOCKS AND SPAN UPDATE FUNCTIONS ENDS                                                           ;




;                                                                                                                                                                  ;
;                                                Modifiled flip logic determination function                                                                       ;
;                                                                                                                                                                  ;

(DEFUN GET_FLIPSTATES (ENTNAME BLKNAME / DN_ORIENTATION_STRUCTURES UP_ORIENTATION_STRUCTURES STRUCTURES_WITH_STAGGERS TEMP_ELE TEMP_ELE1 LIST2 STF ANF SGM_X J )
  (SETQ DN_ORIENTATION_STRUCTURES (LIST "SINGLE_CANT_MAST" "SINGLE_CANT_DA" "SINGLE_CANT_UPRIGHT"  "SS1" "BOX_TYPE" "SPS" ))
  (SETQ UP_ORIENTATION_STRUCTURES (LIST "DOUBLE_CANT_MAST"  "DOUBLE_CANT_DA" "TRIPLE_CANT_MAST"  "TRIPLE_CANT_DA" "SS2" "SS3" "DOUBLE_CANT_UPRIGHT" "TRIPLE_CANT_UPRIGHT" "TTC" "SS4"))
  (SETQ STRUCTURES_WITH_STAGGERS (LIST "SINGLE_CANT_MAST" "SINGLE_CANT_DA" "SS1" "BOX_TYPE" "DOUBLE_CANT_MAST"  "DOUBLE_CANT_DA" "TRIPLE_CANT_MAST"  "TRIPLE_CANT_DA" "SS2" "SS3" "SINGLE_CANT_UPRIGHT" "DOUBLE_CANT_UPRIGHT" "TRIPLE_CANT_UPRIGHT"))
  (SETQ TEMP_ELE NIL TEMP_ELE1 NIL LIST2 NIL)
  ;(SETQ BLKNAME (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ENTNAME) "EFFECTIVENAME"))
  (SETQ STF (ASSOC "STF1" (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "STF1"))))
  (SETQ ANF (ASSOC "ANF1" (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "ANF1"))))
  (IF (/= STF NIL) (SETQ STF (NTH 1 STF)))
  (IF (/= ANF NIL) (SETQ ANF (NTH 1 ANF)))
  (IF (/= STF NIL)
  (PROGN 
  (IF (/= (VL-POSITION BLKNAME DN_ORIENTATION_STRUCTURES) NIL)
    (PROGN (IF (= STF 0) (SETQ TEMP_ELE1 "DN") (SETQ TEMP_ELE1 "UP")))
  )
  (IF (/= (VL-POSITION BLKNAME UP_ORIENTATION_STRUCTURES) NIL)
    (PROGN (IF (= STF 0) (SETQ TEMP_ELE1 "UP") (SETQ TEMP_ELE1 "DN")))
  )
  
  )
  )
  (IF (/= ANF NIL)
  (PROGN (IF (= BLKNAME "ANCHOR")
    (PROGN (IF (= ANF 0) (SETQ TEMP_ELE1 "END") (SETQ TEMP_ELE1 "START")))
  ))
  )
  (IF (/= (VL-POSITION BLKNAME STRUCTURES_WITH_STAGGERS) NIL)
  (PROGN (SETQ SGF (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "SGF1" "SGF2" "SGF3")))
         (SETQ SGM_X (GET_DYNAMIC_PROPERTIES ENTNAME (LIST "SGM1 X" "SGM2 X" "SGM3 X")))
         (SETQ  J 0)
         (WHILE (< J (LENGTH SGF))
	   (SETQ TEMP_ELE (LIST (NTH 1 (ASSOC (STRCAT "SGF" (ITOA (+ J 1))) SGF)) (NTH 1 (ASSOC (STRCAT "SGM" (ITOA (+ J 1)) " " "X") SGM_X))))
	   (SETQ LIST2 (CONS TEMP_ELE LIST2))
	   (SETQ J (+ J 1))
	 ))
  )
  (IF (/=  LIST2 NIL) 
    (SETQ LIST2 (MAP_ELEMENTS (SINGLE_ELE_LIST (SORT_FUN LIST2 0 1) 0) (LIST 0 1) (LIST "DN" "UP")))
  )
  (LIST TEMP_ELE1 LIST2)
)

;                                                                                                                                                                     ;
;                                              Modifiled flip logic determination function ends                                                                       ;
;                                                                                                                                                                     ;
(DEFUN C:OO (/ A1)
  (SETQ A1 (GETREAL "\n ENTER OFFSET DISTANCE:"))
  (COMMAND "OFFSET" (* 2 A1))
)