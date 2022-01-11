       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTSTRTST.
       DATA DIVISION.
       Working-Storage Section.
       01 W-Var1 pic X.
      
       01 A pic X(10).
       01 B pic X(10).
      * Ok accepted by US and IBM
       01 C PIC X(200) VALUE
                     "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEEE
      -              "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJJJJJJJJ
      -          "LLLLLLLLLLMMMMMMMMMMM".
      * Ok accepted by US and IBM
       01 D PIC N(200) VALUE
                     N"AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEE
      -          "LLLLLLLLLLMMMMMMMMMMM".
      * Ok accepted by US and IBM
       01 F PIC X(200) VALUE
                     "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDD
      -              "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ
      -          "LLLLLLLLLLMMMMMMMMMMM".
      * Ko for US and IBM as not used for a level 88
        01 G PIC X(200) VALUE
                  "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDD"
      -          "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ"
      -          "LLLLLLLLLLMMMMMMMMMMM".
      
      * Ko: Was accepted by US and not IBM but only in DATA DIVISION
        01 CONSTPARM2             PIC X(78) VALUE
            'WYYYYYY PROV RANGE 01: YYYYYYY THRU YYYYYYY'
      -    'TAXONOMY: YYYYYYYYYYYY'.
      * Ko: was accepted by US and not IBM because ' is not colum 72
       01 CONSTPARM2             PIC X(78) VALUE
           'WYYYYYY PROV RANGE 01: YYYYYYY THRU YYYYYYY'
      -    ''TAXONOMY: YYYYYYYYYYYY'.
      * accepted by US and IBM because ' is colum 72
       01 CONSTPARM3             PIC X(78) VALUE
                           'WYYYYYY PROV RANGE 01: YYYYYYY THRU YYYYYYY'
      -    ''TAXONOMY: YYYYYYYYYYYY'.
      * Ok: it is for a 88 level.
       01 H PIC X(200).
           88 HH VALUE
                 "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEEEEEE"
      -          "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ"
      -          "LLLLLLLLLLMMMMMMMMMMM".
      
      * Ko it is not for a 88 level.
       01 I PIC X(200) VALUE
                 "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEEEEEE"
      -          "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ"
      -          "LLLLLLLLLLMMMMMMMMMMM".

      * Ko: continuation < Area B.
       01 J PIC X(200).
           88 JJ VALUE
                 "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEEEEE"
      -"GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ"
      -"LLLLLLLLLLMMMMMMMMMMM".      
       Procedure Division.
      * Ok: was not accepted by US.
           DISPLAY 'A sample text 1 '
      -            '(foo 1) : ' W-Var1
           DISPLAY 'A sample text 2 '
      -            "(foo 2) : " W-Var1
           DISPLAY "A sample text 3 "
      -            "(foo 3) : " W-Var1
           DISPLAY "A sample text 4 "
      -            '(foo 4) : ' W-Var1
           DISPLAY 'W89JSXX PROV RANGE 01: XXXXXXX THRU XXXXXXX   '
      -            'TAXONOMY: XXXXXXXXXX'.
      
      * Ok accepted by US and IBM
           DISPLAY
                     "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEEE
      -              "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJJJJJJJJ
      -          "LLLLLLLLLLMMMMMMMMMMM".
      
      * Ok accepted by US and IBM
           DISPLAY
                     N"AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDDEEEEEE
      -          "LLLLLLLLLLMMMMMMMMMMM".
      
      * Ok accepted by US and IBM
           DISPLAY
                     "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDD
      -              "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ
      -          "LLLLLLLLLLMMMMMMMMMMM".
      * Ok: was Ko for US but Ok for IBM
           DISPLAY
                 "AAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCDDDDDDD"
      -          "GGGGGGGGGGHHHHHHHHHHHHHHHHHHHHKKKKKKKKKKKKJ"
      -          "LLLLLLLLLLMMMMMMMMMMM".
      
           move A to B
      -    .
      
           move "A" to B
      -    (2:1).
           goback
           .
       End Program CONTSTRTST.