﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Pgm.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SEQUENTIAL with FILE STATUS
           SELECT CUSTOMER-F01 ASSIGN TO UT-CUSTOMER-F01
           FILE STATUS IS FS-CUSTOMER.
		   
      *    SEQUENTIAL without FILE STATUS
           SELECT CUSTOMER-F02 ASSIGN TO UT-CUSTOMER-F02.
		   
      *    INDEXED with FILE STATUS
           SELECT STUDENT-F01  ASSIGN TO UT-STUDENT-F01
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS STUDENT-F01-ID
           FILE STATUS IS FS-STUDENT.
		   
      *    INDEXED without FILE STATUS
           SELECT STUDENT-F02  ASSIGN TO UT-STUDENT-F02
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS STUDENT-F02-ID.
		
      *    Syntax RECORD KEY IS allow qualified data name        
           SELECT CUSTOMER-F03 ASSIGN TO UT-CUSTOMER-F03
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CUSTOMER-F03-ID OF CUSTOMER-F03-FILE.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-F01.
       01  CUSTOMER-F01-FILE.
         05 CUSTOMER-F01-ID  PIC X(8).
         05 FILLER           PIC X(100).
       FD  CUSTOMER-F02.
       01  CUSTOMER-F02-FILE.
         05 CUSTOMER-F02-ID  PIC X(8).
         05 FILLER           PIC X(100).
       FD  CUSTOMER-F03.
       01  CUSTOMER-F03-FILE.
         05 CUSTOMER-F03-ID  PIC X(8).
         05 FILLER           PIC X(100).
       FD STUDENT-F01.
       01 STUDENT-F01-FILE.
         05 STUDENT-F01-ID   PIC X(5).
         05 FILLER           PIC X(25).
       FD STUDENT-F02.
       01 STUDENT-F02-FILE.
         05 STUDENT-F02-ID   PIC X(5).
         05 FILLER           PIC X(25).
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-F01-FILE         PIC X(128).
       01 WS-CUSTOMER-F02-FILE         PIC X(128).
       01 FS-CUSTOMER                  PIC X(2).
       01 WS-STUDENT-F01-FILE          PIC X(30).
       01 WS-STUDENT-F02-FILE          PIC X(30).
       01 FS-STUDENT                   PIC X(2).
           
      
       END PROGRAM Pgm.