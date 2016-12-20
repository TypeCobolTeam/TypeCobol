* neither USING not GIVING
SORT x    ASCENDING      d.
SORT x ON DESCENDING KEY d.
SORT x    ASCENDING      d1 d2 d3.
SORT x    DESCENDING     d1 d2 d3.
SORT x ASCENDING d1 d2 ASCENDING d3 DESCENDING d4 ASCENDING d5 d6 DESCENDING d7 d8 d9.
* missing USING
SORT x DESCENDING d GIVING filename1 filename2 filename3.
SORT x ASCENDING  d1 d2 d3 GIVING filename.
* missing GIVING
SORT x DESCENDING d USING filename1 filename2 filename3.
SORT x ASCENDING  d1 d2 d3 USING filename.
* either USING or INPUT PROCEDURE
SORT x DESCENDING d USING filename1 filename2 filename3 INPUT PROCEDURE procedurename GIVING filename.
SORT x ASCENDING  d1 d2 d3 USING filename1 INPUT PROCEDURE IS procedurename1 THRU procedurename2 GIVING filename2.
* either GIVING or OUTPUT PROCEDURE
SORT x DESCENDING d USING filename1 GIVING filename2 OUTPUT PROCEDURE procedurename.
SORT x ASCENDING  d1 d2 d3 USING filename1 GIVING filename2 filename3 OUTPUT PROCEDURE IS procedurename1 THRU procedurename2.
* only 1 filename in INPUT/OUTPUT PROCEDURE
SORT x ASCENDING a USING filename1 OUTPUT PROCEDURE procedurename1 procedurename2.
SORT x DESCENDING d USING filename1 OUTPUT PROCEDURE procedurename1 THRU procedurename2 procedure3.
SORT x ASCENDING  INPUT PROCEDURE procedurename1 procedurename2 GIVING filename.
SORT x DESCENDING INPUT PROCEDURE procedurename1 THRU procedurename2 procedure3 GIVING filename.
* missing DUPLICATES keyword
SORT x ASCENDING d WITH IN ORDER COLLATING SEQUENCE IS alphabetname USING filename GIVING filename.
SORT x ASCENDING d WITH IN ORDER COLLATING SEQUENCE IS alphabetname INPUT PROCEDURE IS procedurename1 THROUGH procedurename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4.
* missing SEQUENCE keyword
SORT x ASCENDING d WITH DUPLICATES IN ORDER COLLATING IS alphabetname USING filename GIVING filename.
SORT x ASCENDING d WITH DUPLICATES IN ORDER COLLATING IS alphabetname INPUT PROCEDURE IS procedurename1 THROUGH procedurename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4.
* missing alphabetname in SEQUENCE
SORT x DESCENDING d      DUPLICATES                    SEQUENCE    USING filename1 GIVING filename2.
SORT x ASCENDING  d WITH DUPLICATES IN ORDER COLLATING SEQUENCE IS USING filename1 GIVING filename2.
* DUPLICATES and SEQUENCE interverted
SORT x DESCENDING d SEQUENCE    alphabetname      DUPLICATES                    USING filename1 GIVING filename2.
SORT x ASCENDING  d SEQUENCE IS alphabetname WITH DUPLICATES IN ORDER COLLATING USING filename1 GIVING filename2.
* DUPLICATES-SEQUENCE at the end
SORT x ASCENDING  d        USING filename1 GIVING filename2      DUPLICATES                    SEQUENCE    alphabetname. 
SORT x DESCENDING d1 d2 d3 USING filename1 GIVING filename2 WITH DUPLICATES IN ORDER COLLATING SEQUENCE IS alphabetname.
SORT x DESCENDING d        INPUT PROCEDURE    procedurename1                     OUTPUT PROCEDURE    procedurename2 THRU procedurename3      DUPLICATES                    SEQUENCE    alphabetname.
SORT x ASCENDING  d1 d2 d3 INPUT PROCEDURE IS procedurename1 THRU procedurename2 OUTPUT PROCEDURE IS procedurename3 THRU procedurename4 WITH DUPLICATES IN ORDER COLLATING SEQUENCE IS alphabetname.