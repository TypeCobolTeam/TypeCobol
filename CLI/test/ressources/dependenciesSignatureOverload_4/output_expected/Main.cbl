Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TypeCobol-Generated.
           05 TC-Pgm2 pic X(08) value 'PGM2'.
           05 Pgm2-Fct-a0263d55-Foo pic X(30)
                value 'Fct=a0263d55-Foo'.
                     
       LOCAL-STORAGE SECTION.
       01 txt   pic X(100).
       01 l     pic S9(5) comp-5.
       01 RC    pic 99.
       PROCEDURE DIVISION.
      *All 4 calls should resolve to the same overload
      *    Call Pgm2::Foo Input txt
      *                         l
      *                  Output RC.
           CALL 'zcallpgm' using TC-Pgm2
                    Pgm2-Fct-a0263d55-Foo
                                 txt
                                 l
                    by reference RC
           end-call
                                  .

      *    Call Pgm2::Foo Input txt
      *              by content l
      *                  Output RC.
           CALL 'zcallpgm' using TC-Pgm2
                    Pgm2-Fct-a0263d55-Foo
                                 txt
                    by content   l
                    by reference RC
           end-call
                                  .

      *    Call Pgm2::Foo Input txt
      *               length of txt
      *                  Output RC.
           CALL 'zcallpgm' using TC-Pgm2
                    Pgm2-Fct-a0263d55-Foo
                                 txt
                                 length of txt
                    by reference RC
           end-call
                                  .

      *    Call Pgm2::Foo Input txt
      *    by content length of txt
      *                  Output RC.
           CALL 'zcallpgm' using TC-Pgm2
                    Pgm2-Fct-a0263d55-Foo
                                 txt
                    by content   length of txt
                    by reference RC
           end-call
                                  .

       END PROGRAM Pgm1.
