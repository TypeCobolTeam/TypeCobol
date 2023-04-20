       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MyPgm.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       01  Var1 pic X value "A".
       PROCEDURE  DIVISION.
      *    Ok ignored
           exit perform

           perform varying Var1 from 1 by 1 until Var1 > 10
                if Var1 = 3
                    exit perform cycle
                end-if
                display Var1

                if Var1 = 8
                    exit perform
                end-if

           end-perform

           perform PARA1
           goback
           .
       PARA1.
           display "Start of paragraph"
           if Var1 = "A"
                exit paragraph
           end-if
           display "End of paragraph"
           .

       PARA2.
           display "Start of paragraph"
           exit paragraph
           if Var1 = "A"
                exit paragraph
           end-if
           display "End of paragraph"
           .
       SECTION-A section.
           display "Start of section"
           if Var1 = "A"
                exit section
           end-if
           display "End of section"
           .
       SECTION-B section.
           display "Start of section"
           if Var1 = "A"
                exit paragraph
           end-if
           display "End of section"
           .
       SECTION-C section.
       PARA1.
           display "Start of C1"
           if Var1 = "A"
                exit paragraph
           end-if
           display "End of C1"
           .
       END PROGRAM MyPgm.