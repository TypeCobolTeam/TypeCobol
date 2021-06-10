       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT FILE-01 ASSIGN TO F01.
          SELECT FILE-02 ASSIGN TO F02.
          SELECT FILE-03 ASSIGN TO F03.
       DATA DIVISION.
       FILE SECTION.
      *Input file
       FD FILE-01.
       01 INPUT-STUDENT.
           05 STUDENT-ID-I PIC 9(5).
           05 STUDENT-NAME-I PIC A(25).
      *Output file
       FD FILE-02.
       01 OUTPUT-STUDENT.
           05 STUDENT-ID-O PIC 9(5).
           05 STUDENT-NAME-O PIC A(25).
      *Sort file
       SD FILE-03.
       01 WORK-STUDENT.
           05 STUDENT-ID-W PIC 9(5).
           05 STUDENT-NAME-W PIC A(25).
       WORKING-STORAGE SECTION.
       01 tab.
          05 item OCCURS 10 INDEXED BY idx.
             10 element PIC X.
       01 varCondition PIC X.
          88 test1 VALUE 'A'.
          88 test2 VALUE 'B'.
       01 group1.
          05 item1.
             10 var1 PIC 9.
             10 var2 PIC X(20).
      *KO qualifiedTextName
      *Syntax is not supported by Cup compiler directives parser
      *COPY SYSLIB::SOMECOPY.
       PROCEDURE DIVISION.
       main.
      *KO qualifiedIndexName
           SET tab::idx UP BY 2
      *KO qualifiedParagraphNameReference
           PERFORM s1::b
      *KO qualifiedDataName1
           SORT FILE-03 ON ASCENDING KEY WORK-STUDENT::STUDENT-ID-W
           USING FILE-01 GIVING FILE-02
      *KO qualifiedConditionName
           SET varCondition::test2 TO TRUE
      *KO qualifiedDataNameOrQualifiedConditionName1
           MOVE 'test' TO group1::item1::var2
           GOBACK
           .
       s1 SECTION.
       a.
           CONTINUE
           .
       b.
           CONTINUE
           .
       END Program Pgm.