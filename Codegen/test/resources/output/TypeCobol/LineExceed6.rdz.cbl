       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION.
       working-storage section.
       01 Table-OthElm.
           05 OthElm.
                10 ElmTaxes.
                    15 MaxElmTax PIC X occurs 10.
                    15 ElmTaxNbr PIC X occurs 10 indexed by Idx-0thE1n.
      *---------------------------------------------------
       PROCEDURE DIVISION.


      *    IF Table-OthElm::OthElm::ElmTaxes::ElmTaxNbr (Idx-0thE1n)
      *                  > Table-OthElm::OthElm::ElmTaxes::MaxElmTax
      *                                                   (Idx-0thE1n)
           IF ElmTaxNbr OF ElmTaxes OF OthElm OF Table-OthElm
                                                  (Idx-0thE1n)
                         > MaxElmTax OF ElmTaxes OF OthElm OF 
                                                            Table-OthElm
                                                          (Idx-0thE1n)
                exit
           end-if
           .
       END PROGRAM DVZF0OSM.
