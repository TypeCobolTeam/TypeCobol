000010*01  PRG.                                                         00000100
000020     02  PRG-KOMPIL.                                              00000200
000030         03  PRG-KOMPIL-HEURE    PIC X(8).                        00000300
000040         03  PRG-KOMPIL-DATE     PIC X(12).                       00000400
000050     02  PRG-REFUNIC.                                             00000500
000060         03  PRG-NOM             PIC X(04).                       00000600
000070             88  PRG-TEST  VALUE 'TEST'.                          00000700
000080         03  PRG-TIRET           PIC X(01).                       00000800
000090         03  PRG-REF.                                             00000900
000100             04  PRG-HEURE.                                       00001000
000110                 05  PRG-HEURE7-N PIC 9(07).                      00001100
000120                 05  FILLER      PIC X.                           00001200
000130             04  PRG-DATE        PIC X(05).                       00001300
000140             04  PRG-CENTRE      PIC X(02).                       00001400
