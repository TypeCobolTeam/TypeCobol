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
      
      *Ok
       REPLACE ==:item-i:== BY ==I== , ==:item-j:== BY ==J==.
       01 DATA-:item-i: PIC X.
       01 DATA-:item-j: PIC X.
      
      *Warning
      *A blank was missing before character"="in column 39.
      *A blank was assumed.
       REPLACE ==:item-k:== BY ==K==,==:item-l:== BY ==L==.
       01 DATA-:item-k: PIC X.
       01 DATA-:item-l: PIC X.
      
      *Ok
       REPLACE ==:item-m:== BY ==M== ; ==:item-n:== BY ==N==.
       01 DATA-:item-m: PIC X.
       01 DATA-:item-n: PIC X.
      
      *Warning - accepted by IBM compiler but currently not supported
      *A blank was missing before character"="in column 39.
      *A blank was assumed.
       REPLACE ==:item-o:== BY ==O==;==:item-p:== BY ==P==.
       01 DATA-:item-o: PIC X.
       01 DATA-:item-p: PIC X.
       PROCEDURE DIVISION.
           DISPLAY DATA-A
                   DATA-B
                   DATA-C
                   DATA-D
                   DATA-E
                   DATA-F
                   DATA-G
                   DATA-H
                   DATA-I
                   DATA-J
                   DATA-K
                   DATA-L
                   DATA-M
                   DATA-N
                   DATA-O
                   DATA-P
           GOBACK
           .
       END PROGRAM TCOMFL06.