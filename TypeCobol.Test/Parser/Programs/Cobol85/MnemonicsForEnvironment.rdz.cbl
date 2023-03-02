       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TCOFM117.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
      * Syntax is:
      *  Environment-name IS Mnemonic-name
      *  - Valid values of Environment-name are listed in IBM
      *    specifications
      *  - Mnemonic-name is an user defined word
      
      
      *    can only be used in ACCEPT
           SYSIN  is SYSIN-M
      *Ko define same environment-name twice
      *A duplicate"SYSIN"clause was found.  Scanning was resumed at the
      *next area"A"item or the start of the next clause.
           SYSIN  is SYSIN-M2
           SYSIPT    SYSIPT-M
      
      *    can only be used in DISPLAY
           SYSOUT   is SYSOUT-M
           SYSLIST     SYSLIST-M
           SYSLST   is SYSLST-M
           SYSPUNCH is SYSPUNCH-M
           SYSPCH   is SYSPCH-M
      
      *    Ok in ACCEPT and DISPLAY
           CONSOLE is CONSOLE-M
      
      *    Ok in WRITE ADVANCING
           C01 is C01-M
           C02    C02-M
           C03 is C03-M
      
      *    KO syntax only for UPSI switch
      *    Will be picked up by ANTLR but stops the whole SPECIAL-NAMES
      *    CodeElement parsing.
      *    C04 is C04-M
      *      ON STATUS IS C04-M-ON
      *      OFF STATUS IS C04-M-OFF
      
      *    KO syntax only for UPSI switch
      *    Will be picked up by ANTLR but stops the whole SPECIAL-NAMES
      *    CodeElement parsing.
      *    C05
      *      ON STATUS IS C05-ON
      *      OFF STATUS IS C05-OFF
      
      
           CSP    IS CSP-M
           S01    IS S01-M
           S02       S02-M
           S03       S03-M
           S04    IS S04-M
      *Ko reuse same mnemonic-name
      *Mnemonic-name"S04-M"was previously defined.  The duplicate
      *environment-name clause was discarded.
           S05    IS S04-M
           AFP-5A IS AFP-5A-M
      
      
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
       01  E-EDIT                 PIC X(150).
      *-----------------------*
       WORKING-STORAGE SECTION.
       01 Var1 pic X(08).
      *Ko The name"S03-M"was used for an item that was
      *   not defined as a data-name. References to this name
      *   may be resolved incorrectly.
       01 S03-M pic X.
      
       PROCEDURE DIVISION.
      
      
      *-ACCEPT--------------------
      *OK usage of environment-name
           accept Var1 from SYSIN
           accept Var1 from SYSIPT
           accept Var1 from CONSOLE
      
      *OK usage of mnemonic-name
           accept Var1 from SYSIN-M
           accept Var1 from SYSIPT-M
           accept Var1 from CONSOLE-M
      
      *-DISPLAY-------------------
      *OK usage of environment-name
           display " " upon  SYSOUT
           display " " upon  SYSLIST
           display " " upon  SYSLST
           display " " upon  SYSPUNCH
           display " " upon  SYSPCH
           display " " upon  CONSOLE
      
      *OK usage of mnemonic-name
           display " " upon  SYSOUT-M
           display " " upon  SYSLIST-M
           display " " upon  SYSLST-M
           display " " upon  SYSPUNCH-M
           display " " upon  SYSPCH-M
           display " " upon  CONSOLE-M
      
      
      *KO mnemonic-name cannot be used as variables
      *    "SYSIN"was not defined as a data-name.
      *    The statement was discarded.
           display SYSIN
           display SYSIPT
           display CONSOLE
           display SYSOUT
           display SYSLIST
           display SYSLST
           display SYSPUNCH
           display SYSPCH
           display CONSOLE
      
      *KO mnemonic-name cannot be used as variables
      *    "SYSIN-M"was not defined as a data-name.
      *    The statement was discarded.
           display SYSIN-M
           display SYSIPT-M
           display CONSOLE-M
           display SYSOUT-M
           display SYSLIST-M
           display SYSLST-M
           display SYSPUNCH-M
           display SYSPCH-M
           display CONSOLE-M
      
      *    Presence of Open before write is not currently checked
      *    by our parser
           OPEN OUTPUT FSYS020
           write E-EDIT after advancing C01-M
           write E-EDIT after advancing C02-M
           write E-EDIT after advancing C03-M
      
           write E-EDIT after advancing S01-M
           write E-EDIT after advancing S02-M
      *    KO "S03-M"was not a uniquely defined name.
      *      The definition to be used could not be determined
      *      from the context.  The reference to the name was discarded.
      *      Ambiguous reference to S03-M
           write E-EDIT after advancing S03-M
      *    Warning Ambiguous reference to S04-M
      *    Not supported by our parser, but error is reported
      *    on data definition.
           write E-EDIT after advancing S04-M
      *    KO "S05-M"was not defined as a data-name.
      *      The statement was discarded.
      *      Unable to resolve reference to S05-M
           write E-EDIT after advancing S05-M
           CLOSE FSYS020
      
           goback
           .
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested.
       PROCEDURE DIVISION.
      *OK usage of mnemonic-name defined in parent program
           display " " upon  SYSOUT-M
           display " " upon  SYSLIST-M
           display " " upon  SYSLST-M
           display " " upon  SYSPUNCH-M
           display " " upon  SYSPCH-M
           display " " upon  CONSOLE-M
           goback
           .
       END PROGRAM Nested.
       end program TCOFM117.