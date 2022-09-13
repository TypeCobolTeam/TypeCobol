015800 CCVS1 SECTION.                                                   K6SCA4.2
015900 OPEN-FILES.                                                      K6SCA4.2
016000     OPEN     OUTPUT PRINT-FILE.                                  K6SCA4.2
016100     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   K6SCA4.2
016200     MOVE    SPACE TO TEST-RESULTS.                               K6SCA4.2
016300     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             K6SCA4.2
016400     GO TO CCVS1-EXIT.                                            K6SCA4.2
016500 CLOSE-FILES.                                                     K6SCA4.2
016600     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   K6SCA4.2
016700 TERMINATE-CCVS.                                                  K6SCA4.2
      *Initially next two lines had the 'S' indicator which is unknown
016800     EXIT PROGRAM.                                                K6SCA4.2
016900 TERMINATE-CALL.                                                  K6SCA4.2
017000     STOP     RUN.                                                K6SCA4.2
017100 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         K6SCA4.2
017200 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           K6SCA4.2
017300 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          K6SCA4.2
017400 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      K6SCA4.2
017500     MOVE "****TEST DELETED****" TO RE-MARK.                      K6SCA4.2
017600 PRINT-DETAIL.                                                    K6SCA4.2
017700     IF REC-CT NOT EQUAL TO ZERO                                  K6SCA4.2
017800             MOVE "." TO PARDOT-X                                 K6SCA4.2
017900             MOVE REC-CT TO DOTVALUE.                             K6SCA4.2
018000     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      K6SCA4.2
018100     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               K6SCA4.2
018200        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 K6SCA4.2
018300          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 K6SCA4.2
018400     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              K6SCA4.2
018500     MOVE SPACE TO CORRECT-X.                                     K6SCA4.2
018600     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         K6SCA4.2
018700     MOVE     SPACE TO RE-MARK.                                   K6SCA4.2
018800 HEAD-ROUTINE.                                                    K6SCA4.2
018900     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  K6SCA4.2
019000     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  K6SCA4.2
019100     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  K6SCA4.2
019200     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  K6SCA4.2
019300 COLUMN-NAMES-ROUTINE.                                            K6SCA4.2
019400     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           K6SCA4.2
019500     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   K6SCA4.2
019600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        K6SCA4.2
019700 END-ROUTINE.                                                     K6SCA4.2
019800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.K6SCA4.2
019900 END-RTN-EXIT.                                                    K6SCA4.2
020000     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   K6SCA4.2
020100 END-ROUTINE-1.                                                   K6SCA4.2
020200      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      K6SCA4.2
020300      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               K6SCA4.2
020400      ADD PASS-COUNTER TO ERROR-HOLD.                             K6SCA4.2
020500*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   K6SCA4.2
020600      MOVE PASS-COUNTER TO CCVS-E-4-1.                            K6SCA4.2
020700      MOVE ERROR-HOLD TO CCVS-E-4-2.                              K6SCA4.2
020800      MOVE CCVS-E-4 TO CCVS-E-2-2.                                K6SCA4.2
020900      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           K6SCA4.2
021000  END-ROUTINE-12.                                                 K6SCA4.2
021100      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        K6SCA4.2
021200     IF       ERROR-COUNTER IS EQUAL TO ZERO                      K6SCA4.2
021300         MOVE "NO " TO ERROR-TOTAL                                K6SCA4.2
021400         ELSE                                                     K6SCA4.2
021500         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       K6SCA4.2
021600     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           K6SCA4.2
021700     PERFORM WRITE-LINE.                                          K6SCA4.2
021800 END-ROUTINE-13.                                                  K6SCA4.2
021900     IF DELETE-COUNTER IS EQUAL TO ZERO                           K6SCA4.2
022000         MOVE "NO " TO ERROR-TOTAL  ELSE                          K6SCA4.2
022100         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      K6SCA4.2
022200     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   K6SCA4.2
022300     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           K6SCA4.2
022400      IF   INSPECT-COUNTER EQUAL TO ZERO                          K6SCA4.2
022500          MOVE "NO " TO ERROR-TOTAL                               K6SCA4.2
022600      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   K6SCA4.2
022700      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            K6SCA4.2
022800      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          K6SCA4.2
022900     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           K6SCA4.2
023000 WRITE-LINE.                                                      K6SCA4.2
023100     ADD 1 TO RECORD-COUNT.                                       K6SCA4.2
      *Initially next 13 lines had the 'Y' indicator which is unknown
023200     IF RECORD-COUNT GREATER 42                                   K6SCA4.2
023300         MOVE DUMMY-RECORD TO DUMMY-HOLD                          K6SCA4.2
023400         MOVE SPACE TO DUMMY-RECORD                               K6SCA4.2
023500         WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  K6SCA4.2
023600         MOVE CCVS-H-1  TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES    K6SCA4.2
023700         MOVE CCVS-H-2A TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES    K6SCA4.2
023800         MOVE CCVS-H-2B TO DUMMY-RECORD PERFORM WRT-LN 3 TIMES    K6SCA4.2
023900         MOVE CCVS-H-3  TO DUMMY-RECORD PERFORM WRT-LN 3 TIMES    K6SCA4.2
024000         MOVE CCVS-C-1  TO DUMMY-RECORD PERFORM WRT-LN            K6SCA4.2
024100         MOVE CCVS-C-2  TO DUMMY-RECORD PERFORM WRT-LN            K6SCA4.2
024200         MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          K6SCA4.2
024300         MOVE DUMMY-HOLD TO DUMMY-RECORD                          K6SCA4.2
024400         MOVE ZERO TO RECORD-COUNT.                               K6SCA4.2
024500     PERFORM WRT-LN.                                              K6SCA4.2
024600 WRT-LN.                                                          K6SCA4.2
024700     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               K6SCA4.2
024800     MOVE SPACE TO DUMMY-RECORD.                                  K6SCA4.2
024900 BLANK-LINE-PRINT.                                                K6SCA4.2
025000     PERFORM WRT-LN.                                              K6SCA4.2
025100 FAIL-ROUTINE.                                                    K6SCA4.2
025200     IF     COMPUTED-X NOT EQUAL TO SPACE                         K6SCA4.2
025300            GO TO   FAIL-ROUTINE-WRITE.                           K6SCA4.2
025400     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.K6SCA4.2
025500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 K6SCA4.2
025600     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   K6SCA4.2
025700     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   K6SCA4.2
025800     MOVE   SPACES TO INF-ANSI-REFERENCE.                         K6SCA4.2
025900     GO TO  FAIL-ROUTINE-EX.                                      K6SCA4.2
026000 FAIL-ROUTINE-WRITE.                                              K6SCA4.2
026100     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         K6SCA4.2
026200     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 K6SCA4.2
026300     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. K6SCA4.2
026400     MOVE   SPACES TO COR-ANSI-REFERENCE.                         K6SCA4.2
026500 FAIL-ROUTINE-EX. EXIT.                                           K6SCA4.2
026600 BAIL-OUT.                                                        K6SCA4.2
026700     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   K6SCA4.2
026800     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           K6SCA4.2
026900 BAIL-OUT-WRITE.                                                  K6SCA4.2
027000     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  K6SCA4.2
027100     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 K6SCA4.2
027200     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   K6SCA4.2
027300     MOVE   SPACES TO INF-ANSI-REFERENCE.                         K6SCA4.2
027400 BAIL-OUT-EX. EXIT.                                               K6SCA4.2
027500 CCVS1-EXIT.                                                      K6SCA4.2
027600     EXIT.                                                        K6SCA4.2
027700 LB106A-INIT SECTION.                                             K6SCA4.2
027800 LB106A-001.                                                      K6SCA4.2
027900     MOVE  " REGARDLESS OF WHAT APPEARS ABOVE OR BELOW, THIS IS THK6SCA4.2
028000-          "E REPORT FOR SM106A" TO PRINT-REC.                    K6SCA4.2
028100     PERFORM WRITE-LINE.                                          K6SCA4.2
028200     PERFORM BLANK-LINE-PRINT.                                    K6SCA4.2
028300     MOVE     " THE PRESENCE OF THIS MESSAGE INDICATES THAT TEXT FK6SCA4.2
028400-    "OR ALL 3 DIVISIONS CAN BE GENERATED BY ONE COPY STATEMENT." K6SCA4.2
028500              TO PRINT-REC.                                       K6SCA4.2
028600     PERFORM       WRITE-LINE.                                    K6SCA4.2
028700     PERFORM       INSPT.                                         K6SCA4.2
028800 CCVS-EXIT SECTION.                                               K6SCA4.2
028900 CCVS-999999.                                                     K6SCA4.2
029000     GO TO CLOSE-FILES.                                           K6SCA4.2
