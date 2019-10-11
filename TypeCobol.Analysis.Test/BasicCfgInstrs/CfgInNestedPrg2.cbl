       IDENTIFICATION DIVISION.
       PROGRAM-ID. StackedNestedPgms.
      
       PROCEDURE DIVISION.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested0.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.  SELECT VUE ASSIGN TO ECRAN-SI
                      ORGANIZATION INDEXED.
       DATA DIVISION.
       FILE SECTION.
       FD VUE.
       01 TAMPON.
          02 fct1 pic s9(10)v99.
          02 opt pic x.
          02 fct2 pic s9(10)v99.
          02 zres pic s9(12)v999.
       WORKING-STORAGE SECTION.
       01 tabind.
          02 ind pic 1 occurs 99.
       77 ef pic 1 value "1".
       77 fct3 pic 1 value "1".
       PROCEDURE DIVISION.
           open i-o VUE.
           move zero to fct1 fct2 zres.
           perform affichage.
           move ef to ind(40)
           perform until ind(03) = ef
             evaluate opt
             when "+" if ind(08) = ef
                         compute zres = (fct1 + ((fct1 * fct2) / 100))
                         on size error move ef to ind(31)
                         end-compute
                      else
                         compute zres = (fct1 + fct2)
                         on size error move ef to ind(31)
                         end-compute
                      end-if
             when "-" if ind(08) = ef
                         compute zres = (fct1 - ((fct1 * fct2) / 100))
                         on size error move ef to ind(31)
                         end-compute
                      else
                         compute zres = (fct1 - fct2)
                         on size error move ef to ind(31)
                         end-compute
                      end-if
             when "*" if ind(08) = ef
                         move ef to ind(30)
                      else
                         compute zres = (fct1 * fct2)
                         on size error move ef to ind(31)
                         end-compute
                      end-if
             when "/" if ind(08) = ef
                         move ef to ind(30)
                      else
                         compute zres = (fct1 / fct2)
                         on size error move ef to ind(31)
                         end-compute
                      end-if
             when "P" if ind(08) = ef
                         move ef to ind(30)
                      else
                         move fct1 to zres
                         move zero to fct3
                         perform varying fct3 from 1 by 1
                          until fct3 = fct2  or ind(31) = ef
                             compute zres = (zres * fct1)
                             on size error move ef to ind(31)
                             end-compute
                         end-perform
                      end-if
             end-evaluate
           perform affichage
           end-perform.
           close VUE stop run.
       affichage.
             write tabind
             read  tabind.
       END PROGRAM Nested0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested1.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Nested1.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested2.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Nested2.
      
       END PROGRAM StackedNestedPgms.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacke0.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Stacke0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacke1.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Stacke1.
      