SORT x    ASCENDING      d     USING filename1 GIVING filename2.
SORT x ON DESCENDING KEY d     USING filename1 GIVING filename2.
* multiple keys
SORT x ON ASCENDING KEY d1 d2 d3 USING filename1 GIVING filename2.
SORT x    DESCENDING    d1 d2 d3 USING filename1 GIVING filename2.
SORT x ASCENDING d1 d2 ASCENDING d3 DESCENDING d4 ASCENDING d5 d6 DESCENDING d7 d8 d9 USING filename1 GIVING filename2.
* DUPLICATES
SORT x ASCENDING  d      DUPLICATES          USING filename1 GIVING filename2.
SORT x DESCENDING d      DUPLICATES IN ORDER USING filename1 GIVING filename2.
SORT x ASCENDING  d WITH DUPLICATES    ORDER USING filename1 GIVING filename2.
SORT x DESCENDING d WITH DUPLICATES IN ORDER USING filename1 GIVING filename2.
* SEQUENCE
SORT x DESCENDING d           SEQUENCE    alphabetname USING filename1 GIVING filename2.
SORT x ASCENDING  d           SEQUENCE IS alphabetname USING filename1 GIVING filename2.
SORT x DESCENDING d COLLATING SEQUENCE    alphabetname USING filename1 GIVING filename2.
SORT x ASCENDING  d COLLATING SEQUENCE IS alphabetname USING filename1 GIVING filename2.
* both DUPLICATES and SEQUENCE
SORT x DESCENDING d      DUPLICATES                    SEQUENCE    alphabetname USING filename1 GIVING filename2.
SORT x ASCENDING  d WITH DUPLICATES IN ORDER COLLATING SEQUENCE IS alphabetname USING filename1 GIVING filename2.
* USING and OUTPUT PROCEDURE
SORT x DESCENDING d USING filename1 filename2 filename3 OUTPUT PROCEDURE procedurename.
SORT x ASCENDING  d1 d2 d3 USING filename1              OUTPUT PROCEDURE IS procedurename1 THRU procedurename2.
* INPUT PROCEDURE and GIVING
SORT x DESCENDING d        INPUT PROCEDURE    procedurename                      GIVING filename1 filename2 filename3.
SORT x ASCENDING  d1 d2 d3 INPUT PROCEDURE IS procedurename1 THRU procedurename2 GIVING filename1.
* both INPUT PROCEDURE and OUTPUT PROCEDURE
SORT x DESCENDING d        INPUT PROCEDURE    procedurename1                     OUTPUT PROCEDURE    procedurename2 THRU procedurename3.
SORT x ASCENDING  d1 d2 d3 INPUT PROCEDURE IS procedurename1 THRU procedurename2 OUTPUT PROCEDURE    procedurename3.
SORT x ASCENDING  d1 d2 d3 INPUT PROCEDURE    procedurename1 THRU procedurename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4.
* everything
SORT x DESCENDING d             DUPLICATES                    SEQUENCE    alphabetname INPUT PROCEDURE    procedurename1                     OUTPUT PROCEDURE    procedurename2 THRU procedurename3.
SORT x ASCENDING  d1 d2 d3 WITH DUPLICATES IN ORDER COLLATING SEQUENCE IS alphabetname INPUT PROCEDURE IS procedurename1 THRU procedurename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4.