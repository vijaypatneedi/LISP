;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TABLE FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN PORTAL_END_POINTS_GPS (LIST1 / I ENAME TEMP_ELE LIST2)
  (SETQ I 0)
  (SETQ ENAME NIL)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ ENAME (NTH I LIST1))
    (SETQ
      TEMP_ELE (SUM_LIST (NTH 1 (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ENAME)))
			 (BLOCK_REFERENCE_LENGTHS (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT ENAME) 'EFFECTIVENAME) ENAME)
	       )
    )
    (SETQ TEMP_ELE (LIST (LIST ENAME (NTH 0 TEMP_ELE))
			 (LIST ENAME (NTH 1 TEMP_ELE))
		   )
    )
    (SETQ LIST2 (CONS TEMP_ELE LIST2))
    (SETQ I (+ I 1))
  )
  (REVERSE LIST2)
)




(DEFUN BLOCK_REFERENCE_LENGTHS (BLOCKNAME   EBLOCKNAME	/
				BASE_POINT  POINTS_LIST	I
				LIST2	    TEMP_ELE	TEMP_ELE1
				J	    LIST1
			       )

  (SETQ	BASE_POINT
	 (CDR (ASSOC 10 (ENTGET (TBLOBJNAME "BLOCK" BLOCKNAME))))
  )
  (SETQ	POINTS_LIST
	 (BUILD_LIST
	   (FILTER_LIST '("LINE") (BLOCK_ENAME1 EBLOCKNAME) 1)
	   '(3 4 5)
	 )
  )
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
      (SETQ J (+ J 1))
    )
    (SETQ LIST2 (CONS (REVERSE LIST1) LIST2))
    (SETQ I (+ I 1))
  )
  (SETQ LIST2 (SORT_FUN (REVERSE LIST2) 0 1))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GENERAL FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GENERAL FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GENERAL FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;SINGLE_ELEMENT FUNCTION;;;;
(DEFUN SINGLE_ELE_LIST (LIST1 POS / N TEMP_ELE LIST2)
  (SETQ N 0)
  (SETQ TEMP_ELE NIL)
  (SETQ LIST2 NIL)
  (WHILE (< N (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH N LIST1))
    (IF	(NOT (ATOM TEMP_ELE))
      (PROGN
	(SETQ TEMP_ELE (NTH POS TEMP_ELE))
	(SETQ LIST2 (CONS TEMP_ELE LIST2))
      )
      (PROGN (SETQ TEMP_ELE TEMP_ELE)
	     (SETQ LIST2 (CONS TEMP_ELE LIST2))
      )
    )
    (SETQ N (+ N 1))
  )
  (SETQ LIST2 (REVERSE LIST2))
)
;;;;;;;;;;;;;;;;;FOR BUILDIING OF LIST;;;;;;;;;;;
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
    (SETQ TEMP_ELE1 (REVERSE TEMP_ELE1))
    (SETQ LIST3 (CONS TEMP_ELE1 LIST3))
    (SETQ I (+ I 1))
  )
  (SETQ LIST3 (REVERSE LIST3))
)
;;;;;;;;;;;;;;;;;;;;;FOR ADDING OF TWO LISTS;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN LIST_FORM (LIST1 / TEMP_LIST I)
  (SETQ TEMP_LIST NIL)
  (SETQ I 0)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_LIST (APPEND (REVERSE (NTH I LIST1)) TEMP_LIST))
    (SETQ I (+ I 1))
  )
  (SETQ TEMP_LIST (REVERSE TEMP_LIST))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;SPACE_REMOVE IN A GIVEN STRING
(DEFUN SPACE_REMOVE (STR /)
  (IF (OR (= (TYPE STR) 'STR) (= (TYPE STR) 'NIL))
    (PROGN
      (IF (AND (/= STR NIL))
	(VL-LIST->STRING
	  (VL-REMOVE 32 (VL-STRING->LIST STR))
	)
      )
    )
    (PROGN
      (ALERT
	"TO PROGARMMER INPUT IS GOING WRONG WITH  SPACE_REMOVE FUNCTION"
      )
      (QUIT)
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;LIST1 IS INPUT LIST PATTERN (()()()) AND K IS STRING POSITION
  (DEFUN STRING_SORT (LIST1 K / LIST2 ASCII_SUM I J TEMP_ELE NEW_ELE)
    (SETQ I 0)
    (SETQ LIST2 NIL)
    (WHILE (< I (LENGTH LIST1))
      (SETQ J 1)
      (SETQ TEMP_ELE (NTH K (NTH I LIST1)))
      (SETQ ASCII_SUM 0)
      (WHILE (<= J (STRLEN TEMP_ELE))
	(SETQ ASCII_SUM (+ ASCII_SUM (ASCII (SUBSTR TEMP_ELE J 1))))
	(SETQ J (+ J 1))
      )
      (SETQ NEW_ELE (CONS ASCII_SUM (NTH I LIST1)))
      (SETQ LIST2 (CONS NEW_ELE LIST2))
      (SETQ I (+ I 1))
    )
    (SETQ LIST2 (SORT_FUN LIST2 0 0))
    (SETQ LIST2 (MAPCAR '(LAMBDA (X) (CDR X)) LIST2))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;TO CREATE LIST OF LENGTH LEN
(DEFUN N_ELE_LIST (XYZ LEN / I)
  (SETQ I 0)
  (SETQ ELE_LIST NIL)
  (WHILE (< I LEN)
    (SETQ ELE_LIST (CONS XYZ ELE_LIST))
    (SETQ I (+ I 1))
  )
  (SETQ ELE_LIST (REVERSE ELE_LIST))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;REPLACE ONE ELEMENT IN THE AT PARTICULA POSITION IN LIST
(DEFUN REPLACE_ELE_POS (LIST1 ELE POS / I LIST2)
  (SETQ I 0)
  (SETQ LIST2 NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ TEMP_ELE (NTH I LIST1))
    (IF	(= I POS)
      (SETQ LIST2 (CONS ELE LIST2))
      (SETQ LIST2 (CONS TEMP_ELE LIST2))
    )
    (SETQ I (+ I 1))
  )
  (SETQ LIST2 (REVERSE LIST2))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;TO FIND ROW COLUMN OF GIVEN EXCEL TYPE STRING 
(DEFUN ROW_COL (STR /)
;;;;;;;;;;;;;;;;;;;;INTERNAL FUNCTIONS
  (DEFUN STR_BREAK (STR / I J)
    (SETQ STR (VL-STRING->LIST (STRCASE STR)))
    (SETQ I 0)
    (WHILE (< I (LENGTH STR))
      (IF (AND (>= (NTH I STR) 65) (<= (NTH I STR) 90))
	(PROGN (SETQ I (+ I 1)))
	(PROGN (SETQ J I) (SETQ I (LENGTH STR)))
      )
    )
    (SETQ J (+ J 1))
  )
  (DEFUN ATON (STR /)
    (- (ASCII (STRCASE (SUBSTR STR 1 1))) 64)
  )
  (DEFUN EXCEL_ALPA (STR / I SUM)
    (SETQ STR (VL-LIST->STRING (REVERSE (VL-STRING->LIST STR))))
    (SETQ I 0)
    (SETQ SUM 0)
    (WHILE (< I (STRLEN STR))
      (SETQ SUM (+ SUM (* (ATON (SUBSTR STR (+ I 1) 1)) (EXPT 26 I))))
      (SETQ I (+ I 1))
    )
    SUM
  )
;;;;;;;;;;;;;;;;;;;;INTERNAL FUNCTIONS END
;;;;;;MAIN PROGRAM FOR ROW_COL START
  (SETQ STR (SPACE_REMOVE STR))
  (LIST	(- (ATOI (SUBSTR STR (STR_BREAK STR))) 1)
	(- (EXCEL_ALPA (SUBSTR STR 1 (- (STR_BREAK STR) 1))) 1)
  )
;;;;;;;;;MAIN PROGRAM FOR ROW_COL END
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PRINTING SUB MODULE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;INPUTS DISCRIPTION TBL--->TABLE OBJECT
;;;;;;;;;;;;;;;;;;;;;;;;;;DATA--->DATA TO PRINT EXAMPLE:(()())
;;;;;;;;;;;;;DATA WHICH IS NOT GOING TO PRINT IS REPRESENT AS ""
;;;;;;;;;;;;;;;;;;;;;;;;;;EMPTY--->EMPTY ROW FROM WHICH PRINTING HAS TO BE START
;;;;;;;;;;;;;;;;;;;;;;;;;;ROW_WIDTH--->ROW WIDTH 
;;;;IT IS VERTICAL PRINTING FUNCTION
(DEFUN TBL_PRINT
       (TBL DATA EMPTY_ROW ROW_WIDTH / ST_ROW PRESENT_ROW I J)
  (SETQ I 0)
  (SETQ ST_ROW EMPTY_ROW)
  (SETQ TBL (VLAX-ENAME->VLA-OBJECT TBL))
;;;;;;;;ST_ROW MAKING
  (WHILE (< I (LENGTH DATA))
    (SETQ PRESENT_ROW (+ I ST_ROW))
    (VLAX-INVOKE-METHOD
      TBL "INSERTROWS" PRESENT_ROW ROW_WIDTH 1)
    (SETQ J 0)
;;;;ADDING ROW FOR PRINTING
;;;;PRINTING DATA
    (WHILE (< J (LENGTH (NTH I DATA)))
      (IF (/= "" (NTH J (NTH I DATA)))
	(PROGN
	  (VLAX-INVOKE-METHOD
	    TBL
	    "SETCELLVALUE"
	    PRESENT_ROW
	    J
	    (NTH J (NTH I DATA))
	  )
	  (SETQ J (+ J 1))
	)
	(SETQ J (+ J 1))
      )
    )
    (SETQ I (+ I 1))
  )
  (PRINC)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;FINDING OF EFFECTIVE NAME;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN EFFECTIVE_NAME (ENAME /)
  (VLAX-GET-PROPERTY
    (VLAX-ENAME->VLA-OBJECT ENAME)
    "EFFECTIVENAME"
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GENERAL FUNCTIONS END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 ;_________________________________________FUNCTION FOR MAPPING ANCHORS WITH MASTS, PORTAL AND TTC'S_________________________________________________________________________________;
 ;__________________________________INPUT---> TWO POINTS ON THE SCREEN______________________________________________________________________________________________________________________;
 ;_________________________OUTPUT IS ANCHOR_STRUCTURE_MAP_DATA---> LIST PATTERN==> '((STRUCTURE_ENTITY1,ANCHOR_ENTITY1)(STRUCTURE_ENTITY2,ANCHOR_ENTITY2)...............))   _______________;
;_________________________________________FUNCTION FOR MAPPING ANCHORS WITH MASTS, PORTAL AND TTC'S_________________________________________________________________________________;
 ;__________________________________INPUT---> TWO POINTS ON THE SCREEN______________________________________________________________________________________________________________________;
 ;_________________________OUTPUT IS ANCHOR_STRUCTURE_MAP_DATA---> LIST PATTERN==> '((STRUCTURE_ENTITY1,ANCHOR_ENTITY1)(STRUCTURE_ENTITY2,ANCHOR_ENTITY2)...............))   _______________;
(DEFUN ANCHOR_STRUCTURE_MAPPING_GPS	(/
				 WPT1
				 WPT2
				 MAST_TTC
				 PORTAL_ENTITIES
				 PORTAL_ENDS
				 PORTAL_END_UP
				 PORTAL_END_DN
				 PORTAL_END_UP_DN
				 MAST_PORTAL_TTC
				 ANCHOR_LIST
				 ANCHOR_STRUCTURE_MAP_DATA
				)
  (SETQ WPT1 (GETPOINT "ENTER STARTING POINT IN LOP"))
  (SETQ WPT2 (GETPOINT "\n ENTER ENDING POINT IN LOP"))
  (SETQ BLOCKS_LIST (FORM_SSSET (SSGET "W" WPT1 WPT2  (LIST (CONS 0 "INSERT")))))
  (SETQ	MAST_ENTITIES
	 (YARD_DATA_COLLECT_GPS
	   BLOCKS_LIST
	   '("SINGLE_CANT_MAST"			 "DOUBLE_CANT_MAST"
	     "TRIPLE_CANT_MAST"			 "TTC"
	     "SS1"	       "SS2"		 "SS3"
	     "SS4"
	    )
	   1
	   5
	 )
  )
  
  (SETQ UPLINE_MAST_ENTITIES (FILTER_LIST '("DN") MAST_ENTITIES 1))
  (SETQ DNLINE_MAST_ENTITIES (FILTER_LIST '("UP") MAST_ENTITIES 1))
  (SETQ	UPLINE_MAST_ENTITIES
	 (EXTRACT_ENTITY_INFO
	   (SINGLE_ELE_LIST UPLINE_MAST_ENTITIES 0)
	   1
	   3
	 )
  )
  (SETQ	DNLINE_MAST_ENTITIES
	 (EXTRACT_ENTITY_INFO
	   (SINGLE_ELE_LIST DNLINE_MAST_ENTITIES 0)
	   1
	   3
	 )
  )
  (SETQ	PORTAL_ENTITIES
	 (YARD_DATA_COLLECT_GPS BLOCKS_LIST '("PORTAL" "SS5") 1 3) 
  )
  (SETQ TTC_ENTITIES (YARD_DATA_COLLECT_GPS BLOCKS_LIST '("TTC" "SS4") 1 3))
  (SETQ BUFFER_END_ENTITIES (YARD_DATA_COLLECT_GPS BLOCKS_LIST '("SS0") 1 3))

  ;;***************NEWLY ADDED********************************************************************************************************************;--->START
					;(SETQ MAST_ENTITIES (YARD_DATA_COLLECT WPT1 WPT2 '("SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "TTC" "SS0" "SS1" "SS2" "SS3" "SS4") 1 3))
					;(SETQ PORTAL_ENTITIES (YARD_DATA_COLLECT WPT1 WPT2 '("PORTAL" "SS5") 1 3))
					;(SETQ PORTAL_ENDS (PORTAL_END_POINTS (SINGLE_ELE_LIST PORTAL_ENTITIES 0)))
					;(SETQ PORTAL_END_UP (SINGLE_ELE_LIST PORTAL_ENDS 0))
					;(SETQ PORTAL_END_DN (SINGLE_ELE_LIST PORTAL_ENDS 1))
					;(SETQ PORTAL_END_UP_DN (BUILD_LIST (ADD_LISTS PORTAL_END_UP PORTAL_END_DN) '(0 1 3)))
					;(SETQ MAST_WITH_LOC_NO (BUILD_LIST (ATTRIBUTES_FROM_ENTITIES MAST_ENTITIES "MAST_NUMBER1" 0) '(2 0 1)))
					;(SETQ PORTAL_WITH_LOC_NO (BUILD_LIST (ATTRIBUTES_FROM_ENTITIES PORTAL_END_UP_DN "MAST_NUMBER1" 0) '(3 0 1 2)) )
					;(SETQ WITH_LOC_NO (APPEND MAST_WITH_LOC_NO PORTAL_WITH_LOC_NO))
					;(SETQ WITH_LOC_NO (SORT_FUN WITH_LOC_NO 0 0))
					;(SETQ TOTAL_STRUCTURE_ENTITIES (REMOVE_NTH_ELE WITH_LOC_NO 0))
  ;;***************NEWLY ADDED********************************************************************************************************************;--->END

  (SETQ	CANTI_LIST
	 (YARD_DATA_COLLECT_GPS
            BLOCKS_LIST
	   '("SINGLE_CANT_MAST"	    "SINGLE_CANT_DA"
	     "DOUBLE_CANT_MAST"	    "DOUBLE_CANT_DA"
	     "TRIPLE_CANT_MAST"	    "TRIPLE_CANT_DA"
	     "SINGLE_CANT_UPRIGHT"  "DOUBLE_CANT_UPRIGHT"
	     "TRIPLE_CANT_UPRIGHT"  "SS0"
	     "SS1"		    "SS2"
	     "SS3"		    "BOX_TYPE"
	    )
	   1
	   3
	 )
  )
  (SETQ	MAST_TTC (YARD_DATA_COLLECT_GPS
                  BLOCKS_LIST
		   '("SINGLE_CANT_MAST"
		     "DOUBLE_CANT_MAST"
		     "TRIPLE_CANT_MAST"		       "TTC"
		     "SS0"	      "SS1"	       "SS2"
		     "SS3"	      "SS4"
		    )
		   1
		   3
		 )
  )
  (SETQ	MAST_E (YARD_DATA_COLLECT_GPS
                 BLOCKS_LIST
		 '("SINGLE_CANT_MAST" "DOUBLE_CANT_MAST"
		   "TRIPLE_CANT_MAST" "SS0"
		   "SS1"	      "SS2"
		   "SS3"
		  )
		 1
		 3
	       )
  )
  (SETQ	PORTAL_ENDS
	 (PORTAL_END_POINTS_GPS (SINGLE_ELE_LIST PORTAL_ENTITIES 0))
  )
  (SETQ PORTAL_END_UP (SINGLE_ELE_LIST PORTAL_ENDS 0))
  (SETQ PORTAL_END_DN (SINGLE_ELE_LIST PORTAL_ENDS 1))
  (SETQ	PORTAL_END_UP_DN
	 (BUILD_LIST
	   (ADD_LISTS PORTAL_END_UP PORTAL_END_DN)
	   '(0 1 3)
	 )
  )
  (SETQ MAST_PORTAL_TTC (APPEND MAST_TTC PORTAL_END_UP PORTAL_END_DN))
  (SETQ	ANCHOR_LIST
	 (YARD_DATA_COLLECT_GPS BLOCKS_LIST '("ANCHOR" "BTB_ANC") 1 3)
  )
  (SETQ	ANCHOR_STRUCTURE_MAP_DATA
	 (DATA_TO_STRUCTURE_MAP1
	   ANCHOR_LIST
	   MAST_PORTAL_TTC
	   1
	   1
	 )
  )
  (SETQ	ANCHOR_STRUCTURE_MAP_DATA
	 (BUILD_LIST
	   ANCHOR_STRUCTURE_MAP_DATA
	   '(1 0)
	 )
  )
)
 ;_____________________________________________________________END__________________________________________________________________________________________________________________________;


 ;_____________________________________________________________END__________________________________________________________________________________________________________________________;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN C:TABLE_FUN_GPS (/		TBL	    I		MAIN_LIST3
		    MAIN_LIST4	MAIN_LIST1  MAIN_LIST	MAIN_LIST2
		    EMPTY1	EMPTY2	    EMPTY3	EMPTY4
		    ROW_WIDTH	LIST1	    TENSION_MUL
		   )
;;;;;;;;;;;;;;;;;;;;;SUB_FUNCTION_MODULES_START;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAST AND ANCHOR TAG RETERIVAL FUNCTION;;;;;;;;;;;;;;;;;;;;;
  (DEFUN MAST_ANC_TAG (ST ANC_ST / TEMP_ELE FLAG)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN STR_BREAK (STR / TEMP_ELE)
      (IF (/= NIL
	      (SETQ TEMP_ELE (VL-STRING-POSITION (ASCII "/") STR))
	  )
	(SUBSTR STR (+ 2 TEMP_ELE))
	(PROGN
	  (IF (/= NIL
		  (SETQ TEMP_ELE (VL-STRING-POSITION (ASCII "\\") STR))
	      )
	    (SUBSTR STR (+ 2 TEMP_ELE))
	    NIL
	  )
	)
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN ANCHOR_STR_MAKE (STR /)
      (SETQ STR (SPACE_REMOVE STR))
      (SUBSTR STR 1 (VL-STRING-POSITION (ASCII "(") STR))
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN SERIAL_NO (STR /)
      (SETQ STR (SPACE_REMOVE STR))
      (IF (/= STR "")
	(SUBSTR STR (+ 2 (VL-STRING-POSITION (ASCII "-") STR)))
	" "
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;IN THIS FLAG 0 OR 1 IS ALLOWED;;THIS FUNCTION IS FOR BTB_ANCHOR ONLY
    (DEFUN BTB_WIRE_ADJUST (BTB_DATA FLAG /)
      (LIST
	(NTH
	  1
	  (NTH
	    FLAG
	    (SORT_FUN (FILTER_LIST '("WIRE_NO1" "WIRE_NO2") BTB_DATA 0)
		      2
		      0
	    )
	  )
	)
	(NTH
	  1
	  (NTH
	    FLAG
	    (SORT_FUN
	      (FILTER_LIST '("ANCHOR_TYPE1" "ANCHOR_TYPE2") BTB_DATA 0)
	      2
	      0
	    )
	  )
	)
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN ANC_FLAG (ST ANC_ST / FLAG TEMP_LIST TEMP_PT)
      (IF
	(OR (= "PORTAL" (EFFECTIVE_NAME ST)) (= "SS5" (EFFECTIVE_NAME ST)))
	 (PROGN	(SETQ
		  TEMP_LIST (SORT_FUN (LIST_FORM (SUB_ENT_DATA_COLLECT
						   ST
						   '((0 . "LINE"))
						   10
						   11
						 )
				      )
				      0
				      1
			    )
		)
		(SETQ TEMP_PT
		       (MAP_NEAREST1 TEMP_LIST (CDR (ASSOC 10 (ENTGET ANC_ST))) 0)
		)
		(IF (= 0 (VL-POSITION TEMP_PT TEMP_LIST))
		  (SETQ FLAG 1)
		  (SETQ FLAG 0)
		)
	 )
	 (PROGN
	   (SETQ FLAG 0)
	 )
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;SUB MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ TEMP_ELE (CADR (ASSOC 4 (YARD_STRUCTURE_INFO_GPS ST))))
    (SETQ TEMP_ELE1 (CADR (ASSOC 4 (YARD_STRUCTURE_INFO_GPS ANC_ST))))
    (SETQ FLAG (ANC_FLAG ST ANC_ST))
    (IF	(= "ANCHOR" (EFFECTIVE_NAME ANC_ST))
      (PROGN
	(IF (OR (= "PORTAL" (EFFECTIVE_NAME ST)) (= "SS5" (EFFECTIVE_NAME ST)))
	  (PROGN
	    (IF	(= FLAG 0)
	      (PROGN
		(LIST
		  (LIST
		    (SERIAL_NO (CADR (ASSOC "WIRE_RUN1" TEMP_ELE1)))
		    (CADR (ASSOC "WIRE_RUN1" TEMP_ELE1))
		    (CADR (CAR (FILTER_LIST
				 '("MAST_NUMBER1" "SS_NUMBER1")
				 TEMP_ELE
				 0
			       )
			  )
		    )
		    (STR_BREAK (CADR (CAR (FILTER_LIST
					    '("CHAINAGE1"
					      "SS_CHAINAGE1"
					      "MAST_CHAINAGE1"
					     )
					    TEMP_ELE
					    0
					  )
				     )
			       )
		    )
		    (ANCHOR_STR_MAKE
		      (CADR (ASSOC "ANCHOR_TYPE1" TEMP_ELE1))
		    )
		    (CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
		    (CADR (ASSOC 5 (YARD_STRUCTURE_INFO_GPS ANC_ST)))
		    (CADR
		      (CAR
			(FILTER_LIST
			  '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			  TEMP_ELE
			  0
			)
		      )
		    )
		  )
		)
	      )
;;;;;;ANCHOR PROGN FOR PORTALS FOR TOP
	      (PROGN
		(LIST (LIST
			(SERIAL_NO (CADR (ASSOC "WIRE_RUN1" TEMP_ELE1)))
			(CADR (ASSOC "WIRE_RUN1" TEMP_ELE1))
			(CADR (ASSOC "MAST_NUMBER2" TEMP_ELE))
			(STR_BREAK (CADR (ASSOC "CHAINAGE2" TEMP_ELE)))
			(ANCHOR_STR_MAKE
			  (CADR (ASSOC "ANCHOR_TYPE1" TEMP_ELE1))
			)
			(CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
			(CADR (ASSOC 5 (YARD_STRUCTURE_INFO_GPS ANC_ST)))
			(CADR (ASSOC "CHAINAGE2" TEMP_ELE))
		      )
		)
	      )
;;;;;;;ANCHOR PROGN FOR PORTALS FOR BOTTOM
	    )
	  )
;;;;;;ANCHOR PROGN FOR PORTALS END
	  (PROGN
	    (LIST
	      (LIST
		(SERIAL_NO (CADR (ASSOC "WIRE_RUN1" TEMP_ELE1)))
		(CADR (ASSOC "WIRE_RUN1" TEMP_ELE1))
		(CADR (CAR (FILTER_LIST
			     '("MAST_NUMBER1" "SS_NUMBER1")
			     TEMP_ELE
			     0
			   )
		      )
		)
		(STR_BREAK
		  (CADR
		    (CAR (FILTER_LIST
			   '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			   TEMP_ELE
			   0
			 )
		    )
		  )
		)
		(ANCHOR_STR_MAKE
		  (CADR (ASSOC "ANCHOR_TYPE1" TEMP_ELE1))
		)
		(CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
		(CADR (ASSOC 5 (YARD_STRUCTURE_INFO_GPS ANC_ST)))
		(CADR
		  (CAR (FILTER_LIST
			 '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			 TEMP_ELE
			 0
		       )
		  )
		)
	      )
	    )
	  )
;;;;ANCHOR PROGN FOR MASTS END
	)
      )
;;;PROGN FOR ANCHOR PROGN END
;;;;;;;;;;;;;PROGN FOR BACK TO BACK TO ANCHOR
      (PROGN
	(IF (OR (= "PORTAL" (EFFECTIVE_NAME ST)) (= "SS5" (EFFECTIVE_NAME ST)))
	  (PROGN
	    (IF	(= FLAG 0)
	      (PROGN
		(LIST
		  (LIST
		    (SERIAL_NO (CAR (BTB_WIRE_ADJUST TEMP_ELE1 0)))
		    (CAR (BTB_WIRE_ADJUST TEMP_ELE1 0))
		    (CADR (CAR (FILTER_LIST
				 '("MAST_NUMBER1" "SS_NUMBER1")
				 TEMP_ELE
				 0
			       )
			  )
		    )
		    (STR_BREAK (CADR (CAR (FILTER_LIST
					    '("CHAINAGE1"
					      "SS_CHAINAGE1"
					      "MAST_CHAINAGE1"
					     )
					    TEMP_ELE
					    0
					  )
				     )
			       )
		    )
		    (ANCHOR_STR_MAKE
		      (CADR (BTB_WIRE_ADJUST TEMP_ELE1 0))
		    )
		    (CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
		    "START"
		    (CADR
		      (CAR
			(FILTER_LIST
			  '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			  TEMP_ELE
			  0
			)
		      )
		    )
		  )
		  (LIST
		    (SERIAL_NO (CAR (BTB_WIRE_ADJUST TEMP_ELE1 1)))
		    (CAR (BTB_WIRE_ADJUST TEMP_ELE1 1))
		    (CADR (CAR (FILTER_LIST
				 '("MAST_NUMBER1" "SS_NUMBER1")
				 TEMP_ELE
				 0
			       )
			  )
		    )
		    (STR_BREAK (CADR (CAR (FILTER_LIST
					    '("CHAINAGE1"
					      "SS_CHAINAGE1"
					      "MAST_CHAINAGE1"
					     )
					    TEMP_ELE
					    0
					  )
				     )
			       )
		    )
		    (ANCHOR_STR_MAKE
		      (CADR (BTB_WIRE_ADJUST TEMP_ELE1 1))
		    )
		    (CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
		    "END"
		    (CADR
		      (CAR
			(FILTER_LIST
			  '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			  TEMP_ELE
			  0
			)
		      )
		    )
		  )
		)
	      )
;;;TRUE PROGN END
	      (PROGN
		(LIST
		  (LIST	(SERIAL_NO (CAR (BTB_WIRE_ADJUST TEMP_ELE1 0)))
			(CAR (BTB_WIRE_ADJUST TEMP_ELE1 0))
			(CADR (ASSOC "MAST_NUMBER2" TEMP_ELE))
			(STR_BREAK (CADR (ASSOC "CHAINAGE2" TEMP_ELE)))
			(ANCHOR_STR_MAKE
			  (CADR (BTB_WIRE_ADJUST TEMP_ELE1 0))
			)
			(CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
			"START"
			(CADR (ASSOC "CHAINAGE2" TEMP_ELE))
		  )
		  (LIST	(SERIAL_NO (CAR (BTB_WIRE_ADJUST TEMP_ELE1 1)))
			(CAR (BTB_WIRE_ADJUST TEMP_ELE1 1))
			(CADR (ASSOC "MAST_NUMBER2" TEMP_ELE))
			(STR_BREAK (CADR (ASSOC "CHAINAGE2" TEMP_ELE)))
			(ANCHOR_STR_MAKE
			  (CADR (BTB_WIRE_ADJUST TEMP_ELE1 1))
			)
			(CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
			"END"
			(CADR (ASSOC "CHAINAGE2" TEMP_ELE))
		  )
		)
	      )
	    )
	  )
	  (PROGN
	    (LIST
	      (LIST
		(SERIAL_NO (CAR (BTB_WIRE_ADJUST TEMP_ELE1 0)))
		(CAR (BTB_WIRE_ADJUST TEMP_ELE1 0))
		(CADR (CAR (FILTER_LIST
			     '("MAST_NUMBER1" "SS_NUMBER1")
			     TEMP_ELE
			     0
			   )
		      )
		)
		(STR_BREAK
		  (CADR
		    (CAR (FILTER_LIST
			   '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			   TEMP_ELE
			   0
			 )
		    )
		  )
		)
		(ANCHOR_STR_MAKE
		  (CADR (BTB_WIRE_ADJUST TEMP_ELE1 0))
		)
		(CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
		"START"
		(CADR
		  (CAR
		    (FILTER_LIST
		      '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
		      TEMP_ELE
		      0
		    )
		  )
		)
	      )
	      (LIST
		(SERIAL_NO (CAR (BTB_WIRE_ADJUST TEMP_ELE1 1)))
		(CAR (BTB_WIRE_ADJUST TEMP_ELE1 1))
		(CADR (CAR (FILTER_LIST
			     '("MAST_NUMBER1" "SS_NUMBER1")
			     TEMP_ELE
			     0
			   )
		      )
		)
		(STR_BREAK
		  (CADR
		    (CAR (FILTER_LIST
			   '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
			   TEMP_ELE
			   0
			 )
		    )
		  )
		)
		(ANCHOR_STR_MAKE
		  (CADR (BTB_WIRE_ADJUST TEMP_ELE1 1))
		)
		(CADR (ASSOC 3 (YARD_STRUCTURE_INFO_GPS ST)))
		"END"
		(CADR
		  (CAR
		    (FILTER_LIST
		      '("CHAINAGE1" "SS_CHAINAGE1" "MAST_CHAINAGE1")
		      TEMP_ELE
		      0
		    )
		  )
		)
	      )
	    )
	  )
	)
      )
    )
;;;TOATAL IF END
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;LIST1 IS LIBARAY
;;;;LIST2 IS SPECIFIC TO REQUIREMENT
  (DEFUN DATA_ADJUSTMENT (LIST2	TENSION_MUL	/
				MAIN_LIST	LIST1
				LEN		S.NO
				LOC_NO_ST	CHAINAGE_ST
				ANCHOR_ST	LOC_NO_END
				CHAINAGE_END	ANCHOR_END
				CAT_WIRE	CON_WIRE
				AC_WIRE		LS_WIRE
				FEEDER_WIRE	AEW_WIRE
				ELE_LINK_BWA2	ELE_LINK_ACA2
				ELE_LINK_FA2	ELE_LINK_AEWA2
				ELE_LINK_BWA1	ELE_LINK_ACA1
				ELE_LINK_FA1	ELE_LINK_AEWA1
			       )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ LIST1	(LIST (SETQ S.NO "A3")
		      (SETQ LOC_NO_ST "B3")
		      (SETQ CHAINAGE_ST "C3")
		      (SETQ ANCHOR_ST "D3")
		      (SETQ LOC_NO_END "E3")
		      (SETQ CHAINAGE_END "F3")
		      (SETQ ANCHOR_END "G3")
		      (SETQ CAT_WIRE "H3")
		      (SETQ CON_WIRE "I3")
		      (SETQ AC_WIRE "J3")
		      (SETQ LS_WIRE "K3")
		      (SETQ FEEDER_WIRE "L3")
		      (SETQ AEW_WIRE "M3")
		)
    )
    (SETQ LEN (LENGTH LIST1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;SUB_FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;TO CALCULATE TENSION LENGTH BETWEEN TO ANCHOR POINTS
    (DEFUN TENSION_EVAL	(LIST1 TENSION_MUL /)
      (DEFUN STR_BREAK_FIRST (STR / TEMP_ELE)
	(IF (/=	NIL
		(SETQ TEMP_ELE (VL-STRING-POSITION (ASCII "/") STR))
	    )
	  (SUBSTR STR (+ 2 TEMP_ELE))
	)
      )
      (DEFUN STR_BREAK_LAST (STR / TEMP_ELE)
	(IF (/=	NIL
		(SETQ TEMP_ELE (VL-STRING-POSITION (ASCII "/") STR))
	    )
	  (SUBSTR STR 1 TEMP_ELE)
	)
      )
      (RTOS
	(/ (ABS
	     (-
	       (+ (* 1000 (ATOF (STR_BREAK_LAST (NTH 7 (NTH 0 LIST1)))))
		  (ATOF (STR_BREAK_FIRST (NTH 7 (NTH 0 LIST1))))
	       )
	       (+ (* 1000 (ATOF (STR_BREAK_LAST (NTH 7 (NTH 1 LIST1)))))
		  (ATOF (STR_BREAK_FIRST (NTH 7 (NTH 1 LIST1))))
	       )
	     )
	   )
	   TENSION_MUL
	)
	2
	2
      )
    )
;;;;;;LIST1 IS MAPPED LIST AND ;;;;LEN IS LEN OF LSIT TO FORM
    (DEFUN LIST_ADJUSTMENT (LIST1 LEN / I MAIN_LIST)
      (SETQ I 0)
      (SETQ MAIN_LIST (N_ELE_LIST "" LEN))
      (WHILE (< I (LENGTH LIST1))
	(SETQ MAIN_LIST
	       (REPLACE_ELE_POS
		 MAIN_LIST
		 (CADR (NTH I LIST1))
		 (CADR (ROW_COL (CAR (NTH I LIST1))))
	       )
	)
	(SETQ I (+ I 1))
      )
      MAIN_LIST
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;SUB FUNCTIONS END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;MAIN PROGRAM IN DATA_ADJUSUMENT STARTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (IF	(= 2 (LENGTH LIST2))
      (PROGN
	(IF (FILTER_LIST '("BWA" "FTA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_BWA2
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
			 (LIST LOC_NO_END (NTH 2 (NTH 1 LIST2)))
			 (LIST CHAINAGE_END (NTH 3 (NTH 1 LIST2)))
			 (LIST ANCHOR_END (NTH 4 (NTH 1 LIST2)))
			 (LIST CAT_WIRE (TENSION_EVAL LIST2 TENSION_MUL))
			 (LIST CON_WIRE (TENSION_EVAL LIST2 TENSION_MUL))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_BWA2 LEN))
	  )
	)
	(IF (FILTER_LIST '("ACA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_ACA2
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
			 (LIST LOC_NO_END (NTH 2 (NTH 1 LIST2)))
			 (LIST CHAINAGE_END (NTH 3 (NTH 1 LIST2)))
			 (LIST ANCHOR_END (NTH 4 (NTH 1 LIST2)))
			 (LIST AC_WIRE (TENSION_EVAL LIST2 TENSION_MUL))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_ACA2 LEN))
	  )
	)
	(IF (FILTER_LIST '("FA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_FA2
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
			 (LIST LOC_NO_END (NTH 2 (NTH 1 LIST2)))
			 (LIST CHAINAGE_END (NTH 3 (NTH 1 LIST2)))
			 (LIST ANCHOR_END (NTH 4 (NTH 1 LIST2)))
			 (LIST FEEDER_WIRE (TENSION_EVAL LIST2 TENSION_MUL))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_FA2 LEN))
	  )
	)
	(IF (FILTER_LIST '("AEWA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_AEWA2
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
			 (LIST LOC_NO_END (NTH 2 (NTH 1 LIST2)))
			 (LIST CHAINAGE_END (NTH 3 (NTH 1 LIST2)))
			 (LIST ANCHOR_END (NTH 4 (NTH 1 LIST2)))
			 (LIST AEW_WIRE (TENSION_EVAL LIST2 TENSION_MUL))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_AEWA2 LEN))
	  )
	)
      )
    )
    (IF	(= 1 (LENGTH LIST2))
      (PROGN
	(IF (FILTER_LIST '("BWA" "FTA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_BWA1
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_BWA1 LEN))
	  )
	)
	(IF (FILTER_LIST '("ACA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_ACA1
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_ACA1 LEN))
	  )
	)
	(IF (FILTER_LIST '("FA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_FA1
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_FA1 LEN))
	  )
	)
	(IF (FILTER_LIST '("AEWA") LIST2 4)
	  (PROGN
	    (SETQ ELE_LINK_AEWA1
		   (LIST (LIST S.NO (NTH 1 (NTH 0 LIST2)))
			 (LIST LOC_NO_ST (NTH 2 (NTH 0 LIST2)))
			 (LIST CHAINAGE_ST (NTH 3 (NTH 0 LIST2)))
			 (LIST ANCHOR_ST (NTH 4 (NTH 0 LIST2)))
		   )
	    )
	    (SETQ MAIN_LIST (LIST_ADJUSTMENT ELE_LINK_AEWA1 LEN))
	  )
	)
      )
    )
    MAIN_LIST
;;;;;;;;;;;;;;;;;;MAIN PROGRAM IN DATA ADJUSTMENT ENDS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PRIMARY PRINTING LIST FORMATION FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;
  (DEFUN ANCHOR_DATA_LIST (LIST1     TENSION_MUL	 /
			   RESULT    J	       I	 COUNT
			   RESULT_LIST	       LEN
			  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN ST_END_ADJUSTMENT (LIST1 /)
      (IF (= 2 (LENGTH LIST1))
	(IF (/= (NTH 6 (CAR LIST1)) "START")
	  (REVERSE LIST1)
	  LIST1
	)
	(IF (= (NTH 6 (CAR LIST1)) "END")
	  NIL
	  T
	)
      )
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN MAX_WIRE (LIST1 /)
      (LAST
	(VL-SORT (MAPCAR '(LAMBDA (X) (ATOI (CAR X))) LIST1) '<)
      )
;;;;;;CHECKING FOR REPEATION OF WIRE NUMBER IS PENDING IN THIS
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;MAIN_PROGRAM OF ANCHOR_DATA_LIST;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ RESULT_LIST NIL)
    (SETQ RESULT LIST1)
    (SETQ J 0)
    (SETQ LEN (FILTER_LIST (LIST " ") RESULT 0))
    (WHILE (< J (LENGTH LEN))
      (SETQ RESULT (VL-REMOVE (NTH J LEN) RESULT))
      (SETQ J (+ J 1))
    )
;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ COUNT (MAX_WIRE RESULT))
    (SETQ I 1)
    (WHILE (AND (<= I COUNT) (/= RESULT NIL))
      (SETQ LEN (FILTER_LIST (LIST (ITOA I)) RESULT 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (IF (= (LENGTH LEN) 1)
	(PROGN
	  (SETQ RESULT (VL-REMOVE (CAR LEN) RESULT))
	  (IF (= T (ST_END_ADJUSTMENT LEN))
	    (SETQ RESULT_LIST
		   (CONS (DATA_ADJUSTMENT LEN TENSION_MUL)
			 RESULT_LIST
		   )
	    )
	  )
	)
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (IF (= (LENGTH LEN) 2)
	(PROGN (SETQ RESULT (VL-REMOVE (CAR LEN) RESULT))
	       (SETQ RESULT (VL-REMOVE (CADR LEN) RESULT))
	       (SETQ RESULT_LIST
		      (CONS (DATA_ADJUSTMENT (ST_END_ADJUSTMENT LEN) TENSION_MUL)
			    RESULT_LIST
		      )
	       )
	)
      )
      (SETQ I (+ I 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    RESULT_LIST
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SUB_FUNCTIONS MODULE END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;MAIN PROGRAM START
;;;;;;;;;;MAIN PROGRAM START
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (SETQ EMPTY1 "A4")
  (SETQ EMPTY2 "A6")
  (SETQ EMPTY3 "A8")
  (SETQ EMPTY4 "A10")
;;;;;;;AEWA EMPTY BOX
  (SETQ ROW_WIDTH 6)
;;;;;;;;ROW WIDTH OF TABLE
;;;;;;DATA HANDLING PROGRAM THIS FOR THE RE-ORGANISING OF DATA AND MAPPING OF ANCHORS
  (SETQ LIST1 (ANCHOR_STRUCTURE_MAPPING_GPS))
  (SETQ TBL (CAR (ENTSEL)))
  (SETQ TENSION_MUL 1)
  (SETQ I 0)
  (SETQ MAIN_LIST NIL)
  (WHILE (< I (LENGTH LIST1))
    (SETQ MAIN_LIST
	   (APPEND
	     (MAST_ANC_TAG (CAR (NTH I LIST1)) (CADR (NTH I LIST1)))
	     MAIN_LIST
	   )
    )
    (SETQ I (+ I 1))
  )
  (SETQ MAIN_LIST1 (FILTER_LIST '("BWA" "FTA") MAIN_LIST 4))
  (SETQ MAIN_LIST2 (FILTER_LIST '("ACA") MAIN_LIST 4))
  (SETQ MAIN_LIST3 (FILTER_LIST '("FA") MAIN_LIST 4))
  (SETQ MAIN_LIST4 (FILTER_LIST '("AEWA") MAIN_LIST 4))
  (SETQ TEMP_MAIN_LIST1 (ANCHOR_DATA_LIST MAIN_LIST1 TENSION_MUL))
  (SETQ TEMP_MAIN_LIST2 (ANCHOR_DATA_LIST MAIN_LIST2 TENSION_MUL))
  (SETQ TEMP_MAIN_LIST3 (ANCHOR_DATA_LIST MAIN_LIST3 TENSION_MUL))
  (SETQ TEMP_MAIN_LIST4 (ANCHOR_DATA_LIST MAIN_LIST4 TENSION_MUL))
  (IF (/= MAIN_LIST1 NIL)
    (TBL_PRINT TBL
	       (STRING_SORT TEMP_MAIN_LIST1 0)
	       (CAR (ROW_COL EMPTY1))
	       ROW_WIDTH
    )
  )
  (IF (/= MAIN_LIST2 NIL)
    (TBL_PRINT TBL
	       (STRING_SORT TEMP_MAIN_LIST2 0)
	       (+ (LENGTH TEMP_MAIN_LIST1)
		  (CAR (ROW_COL EMPTY2))
	       )
	       ROW_WIDTH
    )
  )
  (IF (/= MAIN_LIST3 NIL)
    (TBL_PRINT TBL
	       (STRING_SORT TEMP_MAIN_LIST3 0)
	       (+ (LENGTH TEMP_MAIN_LIST2)
		  (LENGTH TEMP_MAIN_LIST1)
		  (CAR (ROW_COL EMPTY3))
	       )
	       ROW_WIDTH
    )
  )
  (IF (/= MAIN_LIST4 NIL)
    (TBL_PRINT TBL
	       (STRING_SORT TEMP_MAIN_LIST4 0)
	       (+ (LENGTH TEMP_MAIN_LIST1)
		  (LENGTH TEMP_MAIN_LIST2)
		  (LENGTH TEMP_MAIN_LIST3)
		  (CAR (ROW_COL EMPTY4))
	       )
	       ROW_WIDTH
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAIN PROGRAM END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)





































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;BACK UP LINK STATEMENTS 
					;(SETQ ELE_LINK_BWA1 (LIST (S.NO (NTH 1 (NTH 1 LIST2))) (LOC_NO_ST (NTH 1 (NTH 2 LIST2))) (CHAINAGE_ST (NTH 1 (NTH 2 LIST2))) (ANCHOR_ST (NTH 1 (NTH 4 LIST2))))) 
					;(SETQ ELE_LINK_ACA1 (LIST (S.NO (NTH 1 (NTH 1 LIST2))) (LOC_NO_ST (NTH 1 (NTH 2 LIST2))) (CHAINAGE_ST (NTH 1 (NTH 2 LIST2))) (ANCHOR_ST (NTH 1 (NTH 4 LIST2)))))
					;(SETQ ELE_LINK_BWA2 (LIST (S.NO (NTH 1 (NTH 1 LIST2))) (LOC_NO_ST (NTH 1 (NTH 2 LIST2))) (CHAINAGE_ST (NTH 1 (NTH 2 LIST2))) (ANCHOR_ST (NTH 1 (NTH 4 LIST2)))
					;    (LOC_NO_END (NTH 2 (NTH 2 LIST2))) (CHAINAGE_END (NTH 2 (NTH 3 LIST2))) (ANCHOR_END (NTH 2 (NTH 4 LIST2))) (CAT_WIRE (TENSION_EVAL LIST2)) (CON_WIRE (TENSION_EVAL LIST2)))) 
					;(SETQ ELE_LINK_ACA2 (LIST (S.NO (NTH 1 (NTH 1 LIST2))) (LOC_NO_ST (NTH 1 (NTH 2 LIST2))) (CHAINAGE_ST (NTH 1 (NTH 2 LIST2))) (ANCHOR_ST (NTH 1 (NTH 4 LIST2)))
					;    (LOC_NO_END (NTH 2 (NTH 2 LIST2))) (CHAINAGE_END (NTH 2 (NTH 3 LIST2))) (ANCHOR_END (NTH 2 (NTH 4 LIST2))) (AC_WIRE (TENSION_EVAL LIST2))))

					;(DEFUN TABLE_FUN (/)
					;  (SETQ S.NO "A3")
					;  (SETQ LOC_NO_ST "B3")
					;  (SETQ CHAINAGE_ST "C3")
					;  (SETQ ANCHOR_ST "D3")
					;  (SETQ LOC_NO_END "E3")
					;  (SETQ CHAINAGE_END "F3")
					;  (SETQ ANCHOR_END "G3")
					; (SETQ CAT_WIRE "H3")
					;  (SETQ CON_WIRE "I3")
					; (SETQ AC_WIRE "J3")
					; (SETQ LS_WIRE "K3")
					;  (SETQ FEEDER_WIRE "L3")
					;  (SETQ EMPTY1 "A4")
					;  (SETQ EMPTY2 "A6")
					; (SETQ EMPTY3 "A8")
					;  (SETQ EMPTY4 "A10");;;;;;;AEWA EMPTY BOX
					; (SETQ ROW_WIDTH 9);;;;;;;;ROW WIDTH OF TABLE
					;)
					;((<Entity name: 7f6837aca60> <Entity name: 7f6835404e0>) (<Entity name: 7f6835670f0> <Entity name: 7f683540980>) (<Entity name: 7f6835dbdf0> <Entity name: 7f68353a540>) (<Entity name: 7f6835f7440> <Entity name: 7f68353a720>) (<Entity name: 7f68355b140> <Entity name: 7f683548180>) (<Entity name: 7f68355ba00> <Entity name: 7f683548540>) (<Entity name: 7f68379fdc0> <Entity name: 7f683540b60>))

