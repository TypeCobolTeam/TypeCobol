       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION .
       local-STORAGE SECTION.
      
       01  maDateFormatInconnu pic 9(08).
       01 Car.
           05 Driver.
      *        10 BirthDate type Date.
               10 BirthDate.
           11 YYYY PIC 9(4).
           11 MM PIC 9(2).
           11 DD PIC 9(2).
                                      
      
       PROCEDURE DIVISION.
      *    move unsafe 19900101 to Car::Driver::BirthDate
           move        19900101 to BirthDate OF Driver OF Car
      *    move unsafe 19900101 to            Car::Driver::BirthDate
           move        19900101 to            BirthDate OF Driver OF Car
      *    move Car::Driver::BirthDate::MM to maDateFormatInconnu(5:2)
           move MM OF BirthDate OF Driver OF Car to maDateFormatInconnu(5:2)
           .
       END PROGRAM DVZF0OSM.
      * 1 errors
      * Line 14[0,0] <38, Error, Semantics> - Error during Cobol generation: generated line is after column 72 in fixed format or line exceed 80 columns
