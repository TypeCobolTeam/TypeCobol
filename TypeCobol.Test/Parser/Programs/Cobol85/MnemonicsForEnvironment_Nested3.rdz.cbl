       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOFM117.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
      *    can only be used in ACCEPT
           SYSIN    is SYSIN-M
           SYSIPT      SYSIPT-M

      *    can only be used in DISPLAY
           SYSOUT   is SYSOUT-M
           SYSLIST     SYSLIST-M
           SYSLST   is SYSLST-M
           SYSPUNCH    SYSPUNCH-M
           SYSPCH   is SYSPCH-M

      *    Ok in ACCEPT and DISPLAY
           CONSOLE     CONSOLE-M

      *    Ok in WRITE ADVANCING
           C01      is C01-M
           C02         C02-M
           C03      is C03-M
           C04         C04-M
           C05      is C05-M
           C06         C06-M
           C07      is C07-M
           C08         C08-M
           C09      is C09-M
           C10         C10-M
           C11      is C11-M
           C12         C12-M
           CSP      is CSP-M
           S01         S01-M
           S02      is S02-M
           S03         S03-M
           S04      is S04-M
           S05         S05-M
           AFP-5A   is AFP-5A-M
           decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  FSYS020  ASSIGN TO UT-S-SYS020.
       DATA DIVISION.
       FILE SECTION.
       FD  FSYS020
           LABEL RECORD STANDARD
           RECORDING MODE F
           BLOCK 0 RECORDS
           DATA RECORD E-EDIT.
       01  E-EDIT                 PIC X(150) global.
       WORKING-STORAGE SECTION.
      * Following variables are KO because they conflict with
      * previously defined mnemonics.
       01 SYSIN-M pic x global.
       01 SYSIPT-M pic x global.
       01 SYSOUT-M pic x global.
       01 SYSLIST-M pic x global.
       01 SYSLST-M pic x global.
       01 SYSPUNCH-M pic x global.
       01 SYSPCH-M pic x global.
       01 CONSOLE-M pic x global.
       01 C01-M pic x global.
       01 C02-M pic x global.
       01 C03-M pic x global.
       01 C04-M pic x global.
       01 C05-M pic x global.
       01 C06-M pic x global.
       01 C07-M pic x global.
       01 C08-M pic x global.
       01 C09-M pic x global.
       01 C10-M pic x global.
       01 C11-M pic x global.
       01 C12-M pic x global.
       01 CSP-M pic x global.
       01 S01-M pic x global.
       01 S02-M pic x global.
       01 S03-M pic x global.
       01 S04-M pic x global.
       01 S05-M pic x global.
       01 AFP-5A-M pic x global.
       PROCEDURE DIVISION.
           OPEN OUTPUT FSYS020
           CALL "Nested"
           CLOSE FSYS020
           goback
           .
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 pic x.
       PROCEDURE DIVISION.
      * On IBM compiler, no error is issued on statements
      * because conflicting variables are discarded from Global table
      * In our parser, they are still visible here hence the errors.
           accept var1 from SYSIN-M
           accept var1 from SYSIPT-M
           display var1 upon SYSOUT-M
           display var1 upon SYSLIST-M
           display var1 upon SYSLST-M
           display var1 upon SYSPUNCH-M
           display var1 upon SYSPCH-M
           accept var1 from CONSOLE-M
           display var1 upon CONSOLE-M
           write E-EDIT after advancing C01-M
           write E-EDIT after advancing C02-M
           write E-EDIT after advancing C03-M
           write E-EDIT after advancing C04-M
           write E-EDIT after advancing C05-M
           write E-EDIT after advancing C06-M
           write E-EDIT after advancing C07-M
           write E-EDIT after advancing C08-M
           write E-EDIT after advancing C09-M
           write E-EDIT after advancing C10-M
           write E-EDIT after advancing C11-M
           write E-EDIT after advancing C12-M
           write E-EDIT after advancing CSP-M
           write E-EDIT after advancing S01-M
           write E-EDIT after advancing S02-M
           write E-EDIT after advancing S03-M
           write E-EDIT after advancing S04-M
           write E-EDIT after advancing S05-M
           write E-EDIT after advancing AFP-5A-M
           goback
           .
       END PROGRAM Nested.
       end program TCOFM117.