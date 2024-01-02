CBL
010PROCESS
000000 CODEPAGE(1140)
000001 CBL NOADATA,AFP(VOLATILE),ARCH(6), BUFSIZE(1K)
000002 PROCESS CICS(’string2’),CICS("string3"),CURRENCY('EUR')
000CBL EXIT( INEXIT([’str1’,]mod1) LIBEXIT([’str2’,]mod2) )
PROCESS FLAG(I,I FLAGSTD(x[yy][,0])

000010    CBL DATA(31)                                                  00010000
000020*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-  00011001
000010   CBL OFFSET                                                     00008000
000020******************************************************************00008000
000010* CBL OPTIMIZE(FULL)                                              00010098
000020*========================*                                        00020098
000010 CBL ARITH(EXTEND)                                                00001099
000020* CST 26/01/2011 RU012046 DEB                                     00002099
000010CBL DATA(31)                                                      00010000
000020 CBL OPTIMIZE(FULL),TEST(NONE,SYM),SSRANGE,FLAG(I,E)              00021000
000030*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-  00022001
000010  CBL OPT(FULL)                                                   00010001
000020***           
000010CBL DATA(31)                                                      00010000
000020*-----------------------------------------------------------------00020000  
000010 PROCESS ARITH(EXTEND)                                                    
000015 CBL LIB
CBL COPYLOC, DEFINE(compilation-variable = 1), INITCHECK(LAX), INITIAL
000010 CBL INVDATA(FORCENUMCMP,NOCLEANSIGN)
000020 CBL NUMCHECK, PARMCHECK(ABD)
000030 CBL SUPPRESS, ZONECHECK(MSG), ZONEDATA(NOPFD), LANGUAGE, SOURCE
000040 CBL CPLC DEF(compilation-variable = 1)
000050 CBL IC(STRICT) NOINITIAL NOINVDATA
000060 CBL NUMCHECK(ABD) PC(ABD,500) SUPP NOZONECHECK ZD(MIG) NOSTGOPT
000070 PROCESS NOCOPYLOC,NODEFINE,NOINITCHECK,NOINVD
000080 PROCESS NC(ZON(NOALPHNUM,LAX),NOPAC,BIN(NOTRUNCBIN),ABD)
000090 PROCESS NOPARMCHECK,NOPC,NOSUPP,ZC(ABD)
PROCESS SSRANGE,OFFSET
000100 CBL NOCPLC, NODEF, NOIC, INVD(FNC,NOCS), NONUMCHECK, NONC
000110 CBL NOSUPPRESS NOZC TEST
000111 CBL VSAMOPENFS(SUCC) VS(C)
000120*Invalid/not allowed syntax
000130 CBL AMODE(64)
000140 CBL ALOWCOPYLOC
000150 CBL ALOWDEFINE
000160 CBL LP(64)
000020 IDENTIFICATION DIVISION. 
000100*-Reason  : AMEELIORATION PROCESS ACTIMAT                         00012701
086490          PERFORM PROCESSUS-INDEXATION-IMMO.                      08868001
007500**** PROCESSUS DE MENAGE                                          05270300