       IDENTIFICATION DIVISION.
       PROGRAM-ID. Successive.
      *SERVICE IS YSUXXESS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01  DATE-EU                   TYPEDEF.
      *    10 DD                     PIC 9(02).
      *    10 MM                     PIC 9(02).
      *    10 YYYY                   PIC 9(04).
      *01  DATE-US                   TYPEDEF.
      *    10 YYYY                   PIC 9(04).
      *    10 DD                     PIC 9(02).
      *    10 MM                     PIC 9(02).
           01  LibFctList-Loaded PIC X(01) VALUE SPACE.
               88 LibFctList-IsLoaded      VALUE '1'.
           
           01  LibFctList-VALUES.
      *        a46caa81 -> GetCurrentDateUS
               05 PIC X(08) VALUE 'a46caa81'.
               05 PIC X(08) VALUE LOW-VALUES.
      *        baa9d61f -> GetCurrentDateEU
               05 PIC X(08) VALUE 'baa9d61f'.
               05 PIC X(08) VALUE LOW-VALUES.
           
           01  LibFctList REDEFINES LibFctList-Values.
               05   LibFctItem    OCCURS 2 INDEXED BY LibFctIndex.
                 10 LibFctCode    PIC X(08).
                 10 LibFctPointer PROCEDURE-POINTER.
           01 Successivecpy COPY Successivecpy.
           01 Successive PIC X(08) VALUE 'Successive'.
           01 ERROR-CODE PIC X(08).
           LINKAGE SECTION.
                   COPY YSUXXESS REPLACING ==:YSUXXESS:== BY ==FCT==.
           01  CallData.
               05  DescriptionId PIC X(08).
                 88 CallIsCopy VALUE 'YSUXXESS'.
           01  FctList.
               05 NumberOfFunctions   PIC 9(04).
               05 FctItem OCCURS 9999 DEPENDING ON NumberOfFunctions
                                      INDEXED BY FctIndex.
                 10 FctCode    PIC X(08).
                 10 FctPointer PROCEDURE-POINTER VALUE NULL.
       
      *PROCEDURE DIVISION.
           IF Successivecpy-POINTER-TABLE = LOW_VALUE
               CALL Successive USING Successivecpy
           END-IF
       PROCEDURE DIVISION USING CallData.
           IF CallIsCopy
             PERFORM Copy-Process-Mode
           ELSE
             PERFORM FctList-Process-Mode
           END-IF
       
           GOBACK
           .
       
      *DECLARE FUNCTION ConvertUS2EU PRIVATE
      *                 INPUT     mydate TYPE DATE-US
      *                 RETURNING result TYPE DATE-EU.
       
      *DECLARE PROCEDURE ConvertEU2US PRIVATE
      *                 INPUT     mydate TYPE DATE-EU
      *                 OUTPUT    result TYPE DATE-US.
       
      *DECLARE FUNCTION GetCurrentDateUS PUBLIC
      *                 RETURNING result TYPE DATE-US.
       
      *DECLARE FUNCTION GetCurrentDateEU PUBLIC
      *                 RETURNING result TYPE DATE-EU.
                        
                        Copy-Process-Mode.
                            SET ADDRESS OF FCT TO ADDRESS OF CallData
                        
                            SET FCT-GetCurrentDateUS-01   TO ENTRY 'a46caa81'
                            SET FCT-GetCurrentDateEU-01   TO ENTRY 'baa9d61f'
                            .
                        
                        FctList-Process-Mode.
                            SET ADDRESS OF FctList TO ADDRESS OF CallData
                        
                            IF NOT LibFctList-IsLoaded
                              SET LibFctPointer(1)   TO ENTRY 'a46caa81'
                              SET LibFctPointer(2)   TO ENTRY 'baa9d61f'
                        
                              SET LibFctList-IsLoaded TO TRUE
                            END-IF
                        
                            PERFORM VARYING FctIndex FROM 1 BY 1
                                    UNTIL FctIndex > NumberOfFunctions
                        
                              SEARCH LibFctItem VARYING LibFctIndex
                                WHEN LibFctCode(LibFctIndex) = FctCode(FctIndex)
                                  SET FctPointer(FctIndex) TO LibFctPointer(LibFctIndex)
                              END-SEARCH
                        
                            END-PERFORM
                            .
       
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested.
       DATA DIVISION.
       PROCEDURE DIVISION.
       END PROGRAM Nested.
       
       
       END PROGRAM Successive.
      *
      *DECLARE FUNCTION ConvertUS2EU PRIVATE
      *                 INPUT     mydate TYPE DATE-US
      *                 RETURNING result TYPE DATE-EU.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a3ed1262.
       DATA DIVISION.
       LINKAGE SECTION.
       01 mydate.
           02 YYYY PIC 9(04).
           02 DD PIC 9(02).
           02 MM PIC 9(02).
       01 result.
           02 DD PIC 9(02).
           02 MM PIC 9(02).
           02 YYYY PIC 9(04).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
                   BY REFERENCE result
           .
           MOVE mydate TO result.
       END PROGRAM a3ed1262.
      *
      *DECLARE PROCEDURE ConvertEU2US PRIVATE
      *                 INPUT     mydate TYPE DATE-EU
      *                 OUTPUT    result TYPE DATE-US.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. eb47acd8.
       DATA DIVISION.
       LINKAGE SECTION.
       01 mydate.
           02 DD PIC 9(02).
           02 MM PIC 9(02).
           02 YYYY PIC 9(04).
       01 result.
           02 YYYY PIC 9(04).
           02 DD PIC 9(02).
           02 MM PIC 9(02).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
                   BY REFERENCE result
           .
           MOVE mydate TO result.
       END PROGRAM eb47acd8.
      *
      *DECLARE FUNCTION GetCurrentDateUS PUBLIC
      *                 RETURNING result TYPE DATE-US.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a46caa81.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
      *01 MyDateUS TYPE DATE-US.
       01 MyDateUS.
           02 YYYY PIC 9(04).
           02 DD PIC 9(02).
           02 MM PIC 9(02).
       LINKAGE SECTION.
       01 result.
           02 YYYY PIC 9(04).
           02 DD PIC 9(02).
           02 MM PIC 9(02).
       PROCEDURE DIVISION
             USING BY REFERENCE result
           .
           CONTINUE.
       END PROGRAM a46caa81.
      *
      *DECLARE FUNCTION GetCurrentDateEU PUBLIC
      *                 RETURNING result TYPE DATE-EU.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. baa9d61f.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
      *01 mydateeu TYPE DATE-EU.
       01 mydateeu.
           02 DD PIC 9(02).
           02 MM PIC 9(02).
           02 YYYY PIC 9(04).
      *01 mydateus TYPE DATE-US.
       01 mydateus.
           02 YYYY PIC 9(04).
           02 DD PIC 9(02).
           02 MM PIC 9(02).
      *01 tmp TYPE DATE-US.
       01 tmp.
           02 YYYY PIC 9(04).
           02 DD PIC 9(02).
           02 MM PIC 9(02).
       LINKAGE SECTION.
       01 result.
           02 DD PIC 9(02).
           02 MM PIC 9(02).
           02 YYYY PIC 9(04).
       PROCEDURE DIVISION
             USING BY REFERENCE result
           .
      *    MOVE FUNCTION GetCurrentDateUS() TO tmp           
               CALL a46caa81 USING
           
                   BY REFERENCE GetCurrentDateUS-RESULT
                   BY REFERENCE ERROR-CODE
           
               IF ERROR-CODE = ZERO
                   MOVE GetCurrentDateUS-RESULT TO tmp
               ELSE
      *        TODO: error management
               END-IF
.
      *    MOVE FUNCTION ConvertUS2EU(tmp) TO result           
               CALL a3ed1262 USING
                   BY REFERENCE tmp
           
                   BY REFERENCE ConvertUS2EU-RESULT
                   BY REFERENCE ERROR-CODE
           
               IF ERROR-CODE = ZERO
                   MOVE ConvertUS2EU-RESULT TO result
               ELSE
      *        TODO: error management
               END-IF
.
      *    CALL  ConvertEU2US   INPUT mydateeu OUTPUT mydateus
                CALL eb47acd8 USING 
                                    mydateeu
                                    mydateus
.
       END PROGRAM baa9d61f.
