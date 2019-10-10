(DEFUN C:DISTANCE_ON_CURVEX ()
 (vl-load-com) 
   (setq ent1 (vlax-ename->vla-object POLYLINE_ENTITY_FOR_DISTANCE_V))
   ;(setq ent2 (vlax-ename->vla-object (car (entsel "First entity: "))))
   ;(setq ent3 (vlax-ename->vla-object (car (entsel "Second entity: ")))) 
   
   (setq ent2 (car (entsel "First entity: ")))
   (setq ent3 (car (entsel "Second entity: ")))
  
   
   (setq point11 (getpropertyvalue ent2 "Basepoint"))
   (setq point22 (getpropertyvalue ent3 "Basepoint"))
   
   
   
   (SETQ TEMP_DISTANCE1 (VLAX-CURVE-GETDISTATPOINT ent1 (vlax-curve-getclosestpointto ent1 point11)))
   (SETQ TEMP_DISTANCE2 (VLAX-CURVE-GETDISTATPOINT ent1 (vlax-curve-getclosestpointto ent1 point22)))
  
(ABS (- TEMP_DISTANCE1 TEMP_DISTANCE2))
)

(defun c:TRACK () 
  (SETQ POLYLINE_ENTITY_FOR_DISTANCE_V (CAR(ENTSEL "\n SELECT TRACK REFERENCE")))
  (SETQ DFCC_TO_CONTINUOUS (GETREAL "\n ENTER DFCC AND CONT. CHAINAGE CORRELATION:"))
)

 