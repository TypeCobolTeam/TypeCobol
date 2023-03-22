* neither USING not GIVING
MERGE x    ASCENDING      d.
MERGE x ON DESCENDING KEY d.
MERGE x    ASCENDING      d1 d2 d3.
MERGE x    DESCENDING     d1 d2 d3.
MERGE x ASCENDING d1 d2 ASCENDING d3 DESCENDING d4 ASCENDING d5 d6 DESCENDING d7 d8 d9.
* missing USING
MERGE x DESCENDING d        GIVING filename1 filename2 filename3.
MERGE x ASCENDING  d1 d2 d3 GIVING filename.
* at least 2 filenames in USING
MERGE x DESCENDING d        USING filename  OUTPUT PROCEDURE procedurename1 THRU procedurename2.
MERGE x ASCENDING  d1 d2 d3 USING filename1 GIVING filename2.
* USING and GIVING interverted
MERGE x ASCENDING  d        GIVING filename1 filename2 USING filename2 filename4.
MERGE x DESCENDING d1 d2 d3 GIVING filename1 USING filename2 filename3 filename4.
* missing GIVING
MERGE x DESCENDING d        USING filename1 filename2 filename3.
MERGE x ASCENDING  d1 d2 d3 USING filename1 filename2.
* either GIVING or OUTPUT PROCEDURE
MERGE x DESCENDING d        USING filename1 filename2 GIVING filename3           OUTPUT PROCEDURE procedurename.
MERGE x ASCENDING  d1 d2 d3 USING filename1 filename2 GIVING filename3 filename4 OUTPUT PROCEDURE IS procedurename1 THRU procedurename2.
* only 1 filename in OUTPUT PROCEDURE
MERGE x ASCENDING  USING filename1 filename2 OUTPUT PROCEDURE procedurename1 procedurename2.
MERGE x DESCENDING USING filename1 filename2 OUTPUT PROCEDURE procedurename1 THRU procedurename2 procedure3.
* missing SEQUENCE keyword
MERGE x ASCENDING d WITH DUPLICATES IN ORDER COLLATING IS alphabetname USING filename1 filename2 filename3 GIVING filename4.
MERGE x ASCENDING d WITH DUPLICATES IN ORDER COLLATING IS alphabetname USING filename1 filename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4.
* missing alphabetname in SEQUENCE
MERGE x DESCENDING d      DUPLICATES                    SEQUENCE    USING filename1 filename2 GIVING filename3.
MERGE x ASCENDING  d WITH DUPLICATES IN ORDER COLLATING SEQUENCE IS USING filename1 filename2 GIVING filename3.
* SEQUENCE at the end
MERGE x ASCENDING  d        USING filename1 filename2 GIVING filename3           SEQUENCE    alphabetname. 
MERGE x DESCENDING d1 d2 d3 USING filename1 filename2 GIVING filename3 COLLATING SEQUENCE IS alphabetname.
MERGE x DESCENDING d        USING filename1 filename2 OUTPUT PROCEDURE    procedurename2 THRU procedurename3           SEQUENCE    alphabetname.
MERGE x ASCENDING  d1 d2 d3 USING filename1 filename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4 COLLATING SEQUENCE IS alphabetname.