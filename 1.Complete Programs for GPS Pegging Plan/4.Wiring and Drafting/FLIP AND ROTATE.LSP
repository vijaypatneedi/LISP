(DEFUN C:CHANGE_OBSTRUCTION_FLIP ( / )
  (SETQ WPT1 (GETPOINT "ENTER STARTING POINT IN LOP"))
  (SETQ WPT2 (GETPOINT "\n ENTER ENDING POINT IN LOP"))
  (SETQ BLOCKS_LIST (FORM_SSSET (SSGET "W" WPT1 WPT2 (LIST (CONS 0 "INSERT")))))
  (SETQ BLOCK_NAME_LIST (EXTRACT_ENTITY_INFO2 BLOCKS_LIST))
  (SETQ SIGNAL_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "SIGNAL_NEW") BLOCK_NAME_LIST 1) 0))
  (SETQ REPRESENTATION_LIST (SINGLE_ELE_LIST (FILTER_LIST (LIST "REPRESENTATION3") BLOCK_NAME_LIST 1) 0))
  (SETQ I 0)
  (IF (/= SIGNAL_LIST NIL)
    (PROGN
    (WHILE (< I (LENGTH SIGNAL_LIST))
      (IF (< (NTH 1 (NTH 0 (GET_DYNAMIC_PROPERTIES (NTH I SIGNAL_LIST) (LIST "A1")))) PI)
      (INSERT_DYNAMIC_PROPERTIES (NTH I SIGNAL_LIST) (LIST "Flip state1") (LIST 1))
      )
      (SETQ I (+ I 1))
    )
    )
  )
  (SETQ I 0)
  (IF (/= REPRESENTATION_LIST NIL)
    (PROGN
    (WHILE (< I (LENGTH REPRESENTATION_LIST))
      (INSERT_DYNAMIC_PROPERTIES (NTH I REPRESENTATION_LIST) (LIST "FLIP") (LIST 1))
      (SETQ I (+ I 1))
    )
    )
  )
)



(DEFUN C:ROTATE_ALL_ATTRIBUTES ( / )

  (SETQ WPT1 (GETPOINT "ENTER STARTING POINT IN LOP"))
  (SETQ WPT2 (GETPOINT "\n ENTER ENDING POINT IN LOP"))
  (SETQ BLOCKS_LIST (FORM_SSSET (SSGET "W" WPT1 WPT2 (LIST (CONS 0 "INSERT")))))
  (SETQ BLOCK_NAME_LIST (EXTRACT_ENTITY_INFO2 BLOCKS_LIST))
  (SETQ JUMPER_LIST (FILTER_LIST (LIST "JUMPER") BLOCK_NAME_LIST 1))
  (SETQ MAST_TTC (FILTER_LIST (LIST "SINGLE_CANT_MAST" "DOUBLE_CANT_MAST" "TRIPLE_CANT_MAST" "TTC" "SS0" "SS1" "SS2" "SS3" "SS4") BLOCK_NAME_LIST 1))

  (SETQ JUMPER_LIST (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST JUMPER_LIST 0)  10 -1) (LIST 0 1)))

  (SETQ MAST_TTC (BUILD_LIST (ENTITY_DFX (SINGLE_ELE_LIST MAST_TTC 0)  10 -1) (LIST 0 1)))
  
  (SETQ JUMPER_STRUCTURE_MAP_DATA (DATA_TO_STRUCTURE_MAP_SECTIONING JUMPER_LIST MAST_TTC 1 1))

  (SETQ I 0 TEMP_ATTRIB_LIST NIL TEMP_ROTATION NIL)

  (WHILE (< I (LENGTH JUMPER_STRUCTURE_MAP_DATA))

  (SETQ TEMP_ATTRIB_LIST (SINGLE_ELE_LIST (SUB_ENT_DATA_COLLECT1 (NTH 0 (NTH I JUMPER_STRUCTURE_MAP_DATA)) '((0 . "ATTDEF")) -1 10) 0))

  (SETQ TEMP_ROTATION (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT (NTH 1 (NTH I JUMPER_STRUCTURE_MAP_DATA))) 'ROTATION))

  (CHANGE_ATTRIBUTE_ROTATION (NTH 0 (NTH I JUMPER_STRUCTURE_MAP_DATA))  TEMP_ROTATION)

  (SETQ I (+ I 1))
   )
  
)



(DEFUN CHANGE_ATTRIBUTE_ROTATION1 (ENTNAME ATTRIBUTE_NAME TEMP_ROTATION / ENT_OBJECT SAFEARRAY_SET I LIST1)
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

    (IF (= (vlax-get-property (NTH I SAFEARRAY_SET) 'TAGSTRING) ATTRIBUTE_NAME)
      
    (vlax-put-property (NTH I SAFEARRAY_SET) 'ROTATION TEMP_ROTATION)
      
    )
    (IF (= (vlax-get-property (NTH I SAFEARRAY_SET) 'TAGSTRING) ATTRIBUTE_NAME)
      
    (vlax-put-property (NTH I SAFEARRAY_SET) 'ROTATION TEMP_ROTATION)
      
    )

    (SETQ I (+ I 1))
  )

  
)

 ))



(DEFUN CHANGE_ATTRIBUTE_ROTATION (ENTNAME TEMP_ROTATION / ENT_OBJECT SAFEARRAY_SET I LIST1)
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

    (IF (= (vlax-get-property (NTH I SAFEARRAY_SET) 'TAGSTRING) "JUMPER_TYPE1")
      
    (vlax-put-property (NTH I SAFEARRAY_SET) 'ROTATION TEMP_ROTATION)
      
    )
    (IF (= (vlax-get-property (NTH I SAFEARRAY_SET) 'TAGSTRING) "JUMPER_TYPE2")
      
    (vlax-put-property (NTH I SAFEARRAY_SET) 'ROTATION TEMP_ROTATION)
      
    )

    (SETQ I (+ I 1))
  )

  
)

 ))


