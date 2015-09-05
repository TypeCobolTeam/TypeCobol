OPEN INPUT filename
OPEN INPUT f1 f2 f3
OPEN OUTPUT filename
OPEN OUTPUT f1 f2 f3
OPEN I-O filename
OPEN I-O f1 f2 f3
OPEN EXTEND filename
OPEN EXTEND f1 f2 f3
OPEN INPUT filename
     I-O filename
	 INPUT f1 f2 REVERSED f3 WITH NO REWIND f4 NO REWIND
	 OUTPUT filename
     INPUT filename REVERSED
	 OUTPUT f1 f2 NO REWIND f3 WITH NO REWIND
     INPUT filename NO REWIND
	 EXTEND f1 f2 f3
	 I-O f1 f2 f3
	 EXTEND filename