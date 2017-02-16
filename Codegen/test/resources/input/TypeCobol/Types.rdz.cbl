       IDENTIFICATION DIVISION.
       PROGRAM-ID. Types.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       77  MyVariable1                 pic 9 value 0.
      * unused variables
       01  W-ELEMENT                 PIC X(20).
       01  W-ATTRIBUTE               PIC X(20).
       
      * Redeclaration of "intrinsic" TYPE DATE
       01  TC-DATE                    TYPEDEF.
      * YEAR (on 4 chars)
           10 YYYY                   PIC 9(04).
      * MONTH NUMBER (on 2 chars)
           10 MM                     PIC 9(02).
      * DAY OF MONTH (on 2 chars)
           10 DD                     PIC 9(02).
       
       01  DESCRIPTION               TYPEDEF.
           05 Ligne-01               PIC X(32).
           05 Ligne-02               PIC X(32).
           05 Ligne-03               PIC X(32).
           05 Ligne-04               PIC X(32).
           05 Ligne-05               PIC X(32).
           
       01  IDENTITY                  TYPEDEF.
           05 LastName               PIC X(32).
           05 FirstName              PIC X(25).
           05 BirthDay               TYPE TC-DATE.
           
       01  INTERNAL-REF              TYPEDEF STRONG.
           05  RType                 PIC X(03).
           05  RReference            PIC X(13).
           
       01  EXTERNAL-REF              TYPEDEF STRONG.
           05  RType                 PIC X(03).
           05  RReference            PIC X(30).
           
       01  PERSON                    TYPEDEF.
           05 UID                    PIC 9(13).
           05 InternalRef            TYPE INTERNAL-REF.
           05 ExternalRef            TYPE EXTERNAL-REF.
           05 Identite               TYPE IDENTITY.
           05 Description            TYPE DESCRIPTION.
       
       01  W-InternalRef             type INTERNAL-REF.

       01  PERSON-1                  TYPE PERSON.
       01  PERSON-2                  TYPE PERSON.



       PROCEDURE DIVISION.

       TRAITEMENT.
           MOVE PERSON-1                  TO PERSON-2
           MOVE Description OF PERSON-1   TO Description OF PERSON-2

           MOVE W-InternalRef            TO InternalRef OF PERSON-2
           MOVE InternalRef OF PERSON-2  TO InternalRef OF PERSON-1
           .

       END PROGRAM Types.
