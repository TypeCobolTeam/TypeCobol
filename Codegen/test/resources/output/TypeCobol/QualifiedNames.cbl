       IDENTIFICATION DIVISION.
         PROGRAM-ID.   Test-Name-Qualification.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 a PIC 9.
       01 b PIC 9.

      *01 Vector TYPEDEF.                                                     
      *    05 x PIC 9.                                                        
      *    05 y PIC 9.                                                        
      *    05 z PIC 9.                                                        

      *01 Point TYPEDEF.                                                      
      *    02 Location TYPE Vector.                                           
      *    02 Movment  TYPE Vector.                                           
			 
       01 Segment.
      *    05 p TYPE POINT.                                                   
           05 p.                                                              
           06 Location.                                                       
             07 x PIC 9.                                                      
             07 y PIC 9.                                                      
             07 z PIC 9.                                                      
           06 Movment.                                                        
             07 x PIC 9.                                                      
             07 y PIC 9.                                                      
             07 z PIC 9.                                                      
      *    05 q TYPE POINT.                                                   
           05 q.                                                              
           06 Location.                                                       
             07 x PIC 9.                                                      
             07 y PIC 9.                                                      
             07 z PIC 9.                                                      
           06 Movment.                                                        
             07 x PIC 9.                                                      
             07 y PIC 9.                                                      
             07 z PIC 9.                                                      

       PROCEDURE DIVISION.

           MOVE a TO b
		   
           MOVE a TO x OF Location OF p
           MOVE a TO x OF Location OF p
           .

       END PROGRAM Test-Name-Qualification.
