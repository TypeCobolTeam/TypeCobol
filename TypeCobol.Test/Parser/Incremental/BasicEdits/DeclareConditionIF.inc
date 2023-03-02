1_DeclareCond1:
(4, 21)  -> (4,  21)	\n          88 cond1 VALUE 'A'.
2_DeclareCond2:
(5, 29)  -> (5,  29)	\n          88 cond2 VALUE 'B'.
3_WriteIF:
(8, 26)  -> (8,  26)	\n           IF (cond1)
4_WriteIFBody:
(9, 21)  -> (9,  21)	\n              DISPLAY var2
5_WriteENDIF:
(10, 26) -> (10, 26)	\n           END-IF