* simple
EVALUATE TRUE 
  WHEN condition
    CONTINUE
END-EVALUATE.
* more alsos
EVALUATE TRUE ALSO TRUE ALSO TRUE
  WHEN condition
    CONTINUE
END-EVALUATE.
* more when alsos
EVALUATE TRUE
  WHEN condition1 ALSO condition2 ALSO condition3
    CONTINUE
END-EVALUATE.
* more whens
EVALUATE TRUE
  WHEN condition1
    CONTINUE
  WHEN condition2
    CONTINUE
END-EVALUATE.
* when other
EVALUATE TRUE
  WHEN condition1
    CONTINUE
  WHEN OTHER
    CONTINUE
END-EVALUATE.
* more alsos and more whens
EVALUATE TRUE ALSO TRUE ALSO TRUE
  WHEN condition1 ALSO condition2
    CONTINUE
  WHEN condition3 ALSO condition4 ALSO condition5
    CONTINUE
  WHEN condition6
    CONTINUE
  WHEN condition7 ALSO condition8
    CONTINUE
  WHEN condition9
    CONTINUE
  WHEN OTHER
    CONTINUE
END-EVALUATE.
* nested IF
EVALUATE TRUE
  WHEN conditionname1
    IF conditionname2
      DISPLAY "OK"
    END-IF
  WHEN conditionname3
    DISPLAY "KO"
END-EVALUATE.
* nested perform
EVALUATE TRUE
  WHEN condition
    PERFORM procedureName
END-EVALUATE.
* more nested statements
* TODO: this should work !
*EVALUATE TRUE
*  WHEN condition
*    CONTINUE
*	CONTINUE
*    CONTINUE
*END-EVALUATE.
* TODO: this should work !
*EVALUATE identifier
*  WHEN condition1
*    PERFORM funcname1
*  WHEN ('whatever')
*    PERFORM funcname2
*END-EVALUATE.