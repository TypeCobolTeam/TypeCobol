      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TypeCobol-Generated.
           05 TC-DPDCY01 pic X(08) value 'DPDCY01'.
           05 DPDCY01-Fct-a980c11f-Foo pic X(30)
                value 'Fct=a980c11f-Foo'.
           05 DPDCY01-Fct-eef83fbd-Foo pic X(30)
                value 'Fct=eef83fbd-Foo'.
                               
      *01  ThisReturnCode               TYPE ReturnCode.
       01 ThisReturnCode.
           02 Cod1 pic X(04).
              88 Ok value '0000'.
              88 Ko value '0001'.
           02 Cod2 pic X(04).
                                                        
      *01  W-SystemDateDB2              TYPE DateDB2.
       01 W-SystemDateDB2.
           02 YYYY PIC 9(04).
           02 filler PIC X value '-'.
           02 MM PIC 9(02).
           02 filler PIC X value '-'.
           02 DD PIC 9(02).
                                                     
      *01  W-Date                       TYPE Date.
       01 W-Date.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                                  
      *01  W-Type1                      TYPE DPDCY01::Type1.
       01 W-Type1.
           02 var1.
             03 YYYY PIC 9(4).
             03 MM PIC 9(2).
             03 DD PIC 9(2).
                                                            
       PROCEDURE DIVISION.
      *Calling first overload of DPDCY01::Foo
      *    CALL DPDCY01::Foo  INPUT W-Date
      *                       OUTPUT W-SystemDateDB2
      *                              ThisReturnCode
           CALL TC-DPDCY01 USING
                    DPDCY01-Fct-a980c11f-Foo
                                 W-Date
                    by reference W-SystemDateDB2
                                 ThisReturnCode
           end-call
                                                   
      *Calling second overload of DPDCY01::Foo
      *    CALL DPDCY01::Foo  INPUT W-SystemDateDB2
      *                       OUTPUT W-Type1::var1
      *                              ThisReturnCode
           CALL TC-DPDCY01 USING
                    DPDCY01-Fct-eef83fbd-Foo
                                 W-SystemDateDB2
                    by reference var1 IN W-Type1
                                 ThisReturnCode
           end-call
                                                   
           goback
           .
       END PROGRAM Main.
