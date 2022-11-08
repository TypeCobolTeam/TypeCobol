       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Ok
       REPLACE ==:item-a:== BY ==A==, ==:item-b:== BY ==B==.
       01 DATA-:item-a: PIC X.
       01 DATA-:item-b: PIC X.
      
      *Warning
      *A blank was missing before character"="in column 39.
      *A blank was assumed.
       REPLACE ==:item-c:== BY ==C== ,==:item-d:== BY ==D==.
       01 DATA-:item-c: PIC X.
       01 DATA-:item-d: PIC X.
      
      *Ok
       REPLACE ==:item-e:== BY ==E==; ==:item-f:== BY ==F==.
       01 DATA-:item-e: PIC X.
       01 DATA-:item-f: PIC X.
      
      *Warning - accepted by IBM compiler but currently not supported
      *A blank was missing before character"="in column 39.
      *A blank was assumed.
       REPLACE ==:item-g:== BY ==G== ;==:item-h:== BY ==H==.
       01 DATA-:item-g: PIC X.
       01 DATA-:item-h: PIC X.
       PROCEDURE DIVISION.
           DISPLAY DATA-A
                   DATA-B
                   DATA-C
                   DATA-D
                   DATA-E
                   DATA-F
                   DATA-G
                   DATA-H
           GOBACK
           .
       END PROGRAM TCOMFL06.