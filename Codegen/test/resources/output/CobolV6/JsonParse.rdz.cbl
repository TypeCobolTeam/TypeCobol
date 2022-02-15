Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x PIC X(200) value 'json_test'.
       01  y.
           03  z PIC X(15) VALUE "hello, world!".
         03  az PIC X(15) VALUE "goodbye, world!".
         03  ab.
              05  abc PIC x(3) value spaces.

       PROCEDURE DIVISION.
           JSON PARSE x INTO y
           JSON PARSE x INTO y.
           JSON PARSE x INTO y END-JSON
			 
      *    JSON PARSE x INTO y WITH DETAIL
      *       NAME y::ab IS omITTED
      *       SUPPRESS y::z
           JSON PARSE x INTO y WITH DETAIL 
              NAME ab OF y IS omITTED
              SUPPRESS z OF y
      
      *    JSON PARSE x INTO y DETAIL
      *       NAME OF y::ab IS 'aaa'
      *       SUPPRESS y::z
           JSON PARSE x INTO y DETAIL 
              NAME OF ab OF y IS 'aaa'
              SUPPRESS z OF y
      
      *    JSON PARSE x INTO y WITH DETAIL
      *       NAME y::ab IS 'bbb' y::z IS 'ccc' y::ab OMITTED
      *       SUPPRESS y::z y::ab
           JSON PARSE x INTO y WITH DETAIL 
              NAME ab OF y IS 'bbb' z OF y IS 'ccc' ab OF y OMITTED
              SUPPRESS z OF y ab OF y


           JSON PARSE x INTO y 
              ON EXCEPTION display "error"

           JSON PARSE x INTO y 
              NOT EXCEPTION display "success"
	        
           JSON PARSE x INTO y 
              ON EXCEPTION display "error"
              NOT EXCEPTION display "success"

           GOBACK.
       END PROGRAM Prog.
	   
