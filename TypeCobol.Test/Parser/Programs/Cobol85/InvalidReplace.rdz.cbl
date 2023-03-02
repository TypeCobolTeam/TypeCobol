       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOFM117.
       data division.
       working-storage section.
      *Warning
      *A blank was missing before character"B"in column 28.
      *A blank was assumed.
       REPLACE ==:DVZOSM:==BY ==ITEM1==.
       01 :DVZOSM: PIC X.
      *Error
      *The"REPLACE"statement was invalid.  Expected"BY", but found",BY".
      *The statement was discarded.
       REPLACE ==:DVZOSM:==,BY ==ITEM2==.
       01 :DVZOSM: PIC X.
      *Error
      *The"REPLACE"statement was invalid.  Expected"BY", but found";BY".
      *The statement was discarded.
       REPLACE ==:DVZOSM:==;BY ==ITEM3==.
       01 :DVZOSM: PIC X.
      *Error
      *The"REPLACE"statement was invalid.  Expected"BY", but found".BY".
      *The statement was discarded.
       REPLACE ==:DVZOSM:==.BY ==ITEM4==.
       01 :DVZOSM: PIC X.
      *Warning
      *A blank was missing before character"="in column 31.
      *A blank was assumed.
       REPLACE ==:DVZOSM:== BY==ITEM5==.
       01 :DVZOSM: PIC X.
      *Warning
      *A blank was missing before character"="in column 32.
      *A blank was assumed.
       REPLACE ==:DVZOSM:== BY,==ITEM6==.
       01 :DVZOSM: PIC X.
      *Warning - accepted by IBM compiler but currently not supported
      *A blank was missing before character"="in column 32.
      *A blank was assumed.
       REPLACE ==:DVZOSM:== BY;==ITEM7==.
       01 :DVZOSM: PIC X.
      *Error and warnings
      *Unexpected end of"REPLACE"statement was found.
      *The"REPLACE"statement was discarded.
      *"="was invalid.  Scanning was resumed atthe next area"A"item,
      *level-number, or the start of the next clause.
      *A blank was missing before character"="in column 40.
      *A blank was assumed.
      *A blank was missing before character"="in column 39.
      *A blank was assumed.
      *A blank was missing before character"I"in column 34.
      *A blank was assumed.
      *A blank was missing before character"="in column 33.
      *A blank was assumed.
       REPLACE ==:DVZOSM:== BY.==ITEM8==.
       01 :DVZOSM: PIC X.
       END PROGRAM TCOFM117.