       IDENTIFICATION DIVISION.
       PROGRAM-ID. TypedefCyclic0.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *Test 0, type is not cyclic
       01 NotCyclic TYPEDEF STRICT.
          05 X PIC 9.
          05 Y PIC 9.
          05 Z PIC 9.
       
      *Test 1 : type is directly cyclic with itself
       01 Cyclic1 TYPEDEF STRICT.
          05 X PIC 9.
          05 Y TYPE Cyclic1.
          05 Z PIC 9.
          
      *Test 2 : indirect cycle T2 -> T3 -> T2         
       01 Cyclic2 TYPEDEF STRICT.
          05 X PIC 9.
          05 Y TYPE Cyclic3.
          05 Z PIC 9.
       01 Cyclic3 TYPEDEF STRICT.
          05 X PIC 9.
          05 Y TYPE Cyclic2.
          05 Z PIC 9.
       
      *Test 3 : parent typedefs using cyclic types
       01 Cyclic4 TYPEDEF STRICT.
          05 X TYPE Cyclic5.
          05 Y TYPE Cyclic6.
          05 Z TYPE Cyclic7.
       01 Cyclic5 TYPEDEF STRICT.
          05 X PIC 9.
          05 Y TYPE Cyclic6.
          05 Z PIC 9.
       01 Cyclic6 TYPEDEF STRICT.
          05 X PIC 9.
          05 Y TYPE Cyclic5.
          05 Z PIC 9.
       01 Cyclic7 TYPEDEF STRICT.
          05 X PIC 9.
          05 Y TYPE Cyclic7.
          05 Z PIC 9.
       01 Cyclic8 TYPEDEF STRICT.
          05 item TYPE Cyclic4.
          
      *Typed variables using cyclic types directly, in arrays or groups
       01 var1 TYPE Cyclic1.
       01 var2 TYPE Cyclic2.
       01 var3 TYPE Cyclic3.
       01 var4 TYPE Cyclic4.
       01 var5 TYPE Cyclic5.
       01 var6 TYPE Cyclic6.
       01 var7 TYPE Cyclic7.
       01 var8 TYPE Cyclic8.
       01 array1 TYPE Cyclic1 OCCURS 9.
       01 array2 TYPE Cyclic2 OCCURS 9.
       01 array3 TYPE Cyclic3 OCCURS 9.
       01 array4 TYPE Cyclic4 OCCURS 9.
       01 array5 TYPE Cyclic5 OCCURS 9.
       01 array6 TYPE Cyclic6 OCCURS 9.
       01 array7 TYPE Cyclic7 OCCURS 9.
       01 array8 TYPE Cyclic8 OCCURS 9.
       01 group1.
          05 item1 PIC X.
          05 item2.
             10 sub-item-1 PIC X.
             10 sub-item-2 TYPE Cyclic8.
             10 sub-item-3 PIC X.
          05 item3 PIC X.
      
       END PROGRAM TypedefCyclic0.