000000 IDENTIFICATION DIVISION.
000000 PROGRAM-ID. StringStatement.
000000 ENVIRONMENT DIVISION.
000000 CONFIGURATION SECTION.
000000 SOURCE-COMPUTER. IBM-370.
       special-names. decimal-point is comma.
000000 DATA DIVISION.
000000 working-storage section.
000000 01 MyVar  pic X(30).
000000 01 MyInt pic 9(30).
000000 01 MyPointer pointer.
000000 linkage section.
000000 01 MyVar2 pic X(30).
000000 PROCEDURE DIVISION.
           move WHEN-COMPILED to MyVar
           move WHEN-COMPILED to unknown1
           move 1 to Return-code
           move WHEN-COMPILED to MyVar(unknown1:unknown2)
           move WHEN-COMPILED to MyVar(unknown1:MyInt)
           move WHEN-COMPILED to MyVar(MyInt:unknown2)
           move WHEN-COMPILED to MyVar(MyInt:MyInt)
           move WHEN-COMPILED to notDeclared1
           move function Current-Date() to MyVar
           move length of MyVar to MyInt
           move length of myvar to MyInt
           move length of notDeclared2 to MyInt
           set address of MyVar2 to address of MyVar
000000     .
000000 END PROGRAM StringStatement.