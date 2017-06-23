       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION .
       local-STORAGE SECTION.
      
       01  maDateFormatInconnu pic 9(08).
       01 Car.
           05 Driver.
               10 BirthDate type Date.
      
       PROCEDURE DIVISION.
           move unsafe 19900101 to Car::Driver::BirthDate
           move unsafe 19900101 to            Car::Driver::BirthDate
           move Car::Driver::BirthDate::MM to maDateFormatInconnu(5:2)
           .
       END PROGRAM DVZF0OSM.
