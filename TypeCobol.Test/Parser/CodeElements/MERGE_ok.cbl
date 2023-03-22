MERGE x    ASCENDING      d     USING filename1 filename2 GIVING filename3.
MERGE x ON DESCENDING KEY d     USING filename1 filename2 GIVING filename3.
* multiple keys
MERGE x ON ASCENDING KEY d1 d2 d3 USING filename1 filename2 GIVING filename3.
MERGE x    DESCENDING    d1 d2 d3 USING filename1 filename2 GIVING filename3.
MERGE x ASCENDING d1 d2 ASCENDING d3 DESCENDING d4 ASCENDING d5 d6 DESCENDING d7 d8 d9 USING filename1 filename2 GIVING filename3.
* SEQUENCE
MERGE x DESCENDING d           SEQUENCE    alphabetname USING filename1 filename2 GIVING filename3.
MERGE x ASCENDING  d           SEQUENCE IS alphabetname USING filename1 filename2 GIVING filename3.
MERGE x DESCENDING d COLLATING SEQUENCE    alphabetname USING filename1 filename2 GIVING filename3.
MERGE x ASCENDING  d COLLATING SEQUENCE IS alphabetname USING filename1 filename2 GIVING filename3.
* USING and OUTPUT PROCEDURE
MERGE x DESCENDING d        USING filename1 filename2 filename3 OUTPUT PROCEDURE procedurename.
MERGE x ASCENDING  d1 d2 d3 USING filename1 filename2           OUTPUT PROCEDURE IS procedurename1 THRU procedurename2.
* everything
MERGE x DESCENDING d                  SEQUENCE    alphabetname USING filename1 filename2 OUTPUT PROCEDURE    procedurename2 THRU procedurename3.
MERGE x ASCENDING  d1 d2 d3 COLLATING SEQUENCE IS alphabetname USING filename1 filename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4.