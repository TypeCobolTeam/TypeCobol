#nullable enable

using System.Diagnostics.CodeAnalysis;

namespace TypeCobol.Compiler.Directives
{
    // Cobol Programming Guide p299 : Compiler options
    // You can direct and control your compilation by using compiler options or by using compiler-directing statements (compiler directives).
    // Compiler options affect the aspects of your program that are listed in the table below. 
    // The linked-to information for each option provides the syntax for specifying the option and describes the option, its parameters, and its interaction with other parameters.
    
    // Compiler option | Default | Option abbreviations 
    // --- Source language ---
    // “ARITH” on page 306 ARITH(COMPAT) AR(C|E) 
    // “CICS” on page 309 NOCICS None 
    // “CODEPAGE” on page 310 CODEPAGE(01140) CP(ccsid) 
    // “CURRENCY” on page 313 NOCURRENCY CURR|NOCU RR 
    // “DBCS” on page 315 DBCS None 
    // “NSYMBOL” on page 334 NSYMBOL(NATIONAL) NS(DBCS|NAT) 
    // “NUMBER” on page 335 NONUMBER NUM|NONUM 
    // “QUOTE/APOST” on page 343 QUOTE Q|APOST 
    // “SEQUENCE” on page 346 SEQUENCE SEQ|NOSEQ 
    // “SQL” on page 348 NOSQL None 
    // “SQLCCSID” on page 350 SQLCCSID SQLC|NOSQLC 
    // “SQLIMS” on page 349 NOSQLIMS None 
    // “SUPPRESS” on page 378 SUPPRESS SUPP
    // “WORD” on page 360 NOWORD WD|NOWD 
    // --- Date processing ---
    // “INTDATE” on page 327 INTDATE(ANSI) None 
    // --- Maps and listings ---
    // “LANGUAGE” on page 328 LANGUAGE(ENGLISH) LANG(EN|UE|JA|JP) 
    // “LINECOUNT” on page 329 LINECOUNT(60) LC 
    // “LIST” on page 329 NOLIST None 
    // “MAP” on page 330 NOMAP None 
    // “OFFSET” on page 337 NOOFFSET OFF|NOOFF 
    // “SOURCE” on page 347 SOURCE S|NOS 
    // “SPACE” on page 348 SPACE(1) None 
    // “TERMINAL” on page 353 NOTERMINAL TERM|NOTERM 
    // “VBREF” on page 360 NOVBREF None 
    // “XREF” on page 361 XREF(FULL) X|NOX 
    // --- Object deck generation  ---
    // “COMPILE” on page 313 NOCOMPILE(S) C|NOC 
    // “DECK” on page 316 NODECK D|NOD 
    // “NAME” on page 334 NONAME, orNAME(NOALIAS) if only NAME is specified None 
    // “OBJECT” on page 337 OBJECT OBJ|NOOBJ 
    // “PGMNAME” on page 341 PGMNAME(COMPAT) PGMN(CO|LU|LM)
    // --- Object code control ---
    // “ADV” on page 303 ADV None 
    // “AFP” on page 304 AFP(VOLATILE) None 
    // “ARCH” on page 305 ARCH(6) None 
    // “AWO” on page 307 NOAWO None 
    // “BLOCK0” on page 307 NOBLOCK0 None 
    // “DEFINE” on page 328 NODEFINE DEF | NODEF
    // “DISPSIGN” on page 317 DISPSIGN(COMPAT) DS(S|C) 
    // “DLL” on page 318 NODLL None 
    // “EXPORTALL” on page 323 NOEXPORTALL EXP|NOEXP 
    // “FASTSRT” on page 323 NOFASTSRT FSRT|NOFSRT 
    // “MAXPCF” on page 331 MAXPCF(60000) None 
    // “HGPR” on page 326 HGPR(PRESERVE) None 
    // “INITIAL” on page 342 NOINITIAL None
    // “INVDATA” on page 344 NOINVDATA NOINV | INVD(FNC | NOFNC, CS | NOCS)
    // “LP” on page 349 LP(32) None
    // “NUMCHECK” on page 355 NONUMCHECK NC | NONC
    // “NUMPROC” on page 336 NUMPROC(NOPFD) None 
    // “OPTIMIZE” on page 339 OPTIMIZE(0) OPT(n) 
    // “OUTDD” on page 340 OUTDD(SYSOUT) OUT 
    // “PARMCHECK” on page 364 NOPARMCHECK PC | NOPC
    // “TRUNC” on page 357 TRUNC(STD) None 
    // “ZONECHECK” on page 392 NOZONECHECK NOZC | ZC(MSG) | ZC(ABD)
    //.“ZONEDATA” on page 393 ZONEDATA(PFD) ZD(PFD) | ZD(MIG) | ZD(NOPFD)
    // “ZWB” on page 362 ZWB None
    // --- Virtual storage usage ---
    // “BUFSIZE” on page 309 4096 BUF 
    // “DATA” on page 314 DATA(31) None 
    // “DYNAM” on page 320 NODYNAM DYN|NODYN 
    // “RENT” on page 344 RENT None 
    // “RMODE” on page 345 AUTO None 
    // “SIZE” on page 346 SIZE(5000000) SZ 
    // “STGOPT” on page 352 NOSTGOPT SO|NOSO
    // --- Debugging and diagnostics ---
    // “DIAGTRUNC” on page 316 NODIAGTRUNC DTR|NODTR 
    // “DUMP” on page 319 NODUMP DU|NODU 
    // “FLAG” on page 324 FLAG(I,I) F|NOF 
    // “FLAGSTD” on page 325 NOFLAGSTD None 
    // “INITCHECK” on page 341 NOINITCHECK IC | NOIC
    // “SSRANGE” on page 351 NOSSRANGE SSR|NOSSR 
    // “TEST” on page 353 NOTEST None 
    // --- Other ---
    // “ADATA” on page 303 NOADATA None 
    // “COPYLOC” on page 324 NOCOPYLOC CPLC
    // “EXIT” on page 320 NOEXIT NOEX|EX(INX|NOINX, LIBX|NOLIBX, PRTX|NOPRTX, ADX|NOADX, MSGX|NOMSGX) 
    // “MDECK” on page 332 NOMDECK NOMD|MD|MD(C|NOC) 
    // “OPTFILE” on page 338 None None 
    // “THREAD” on page 356 NOTHREAD None
    // --- Deprecated ---
    // “LIB”


    /// <summary>
    /// Current status and value for all the IBM compiler options
    /// </summary>
    public class IBMCompilerOptionStatus
    {
        /// <summary>
        /// Name of the option
        /// </summary>
        public IBMCompilerOptionName Name { get; }

        /// <summary>
        /// True if the option status is On (DBCS)
        /// False if the option status is Off (NODBCS)
        /// </summary>
        public bool IsActivated { get; private set; }

        /// <summary>
        /// String parameter passed to configured the option
        /// Ex : FLAG(I,I) => Value = "I,I"
        /// </summary>
        public string? Value { get; private set; }

        /// <summary>
        /// Initialize a compiler option directive to its default state
        /// (all default values taken from : IBM Enterprise Cobol 5.1 for zOs - Programming Guide)
        /// </summary>
        internal IBMCompilerOptionStatus(IBMCompilerOptionName name)
        {
            Name = name;
            switch(name)
            {
                case IBMCompilerOptionName.ADATA: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.ADV: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.AFP: IsActivated = true; Value = "VOLATILE"; break;
                case IBMCompilerOptionName.ARCH: IsActivated = true; Value = "6"; break;
                case IBMCompilerOptionName.ARITH: IsActivated = true; Value = "COMPAT"; break;
                case IBMCompilerOptionName.AWO: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.BLOCK0: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.BUFSIZE: IsActivated = true; Value = "4096"; break;
                case IBMCompilerOptionName.CICS: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.CODEPAGE: IsActivated = true; Value = "1140"; break;
                case IBMCompilerOptionName.COMPILE: IsActivated = false; Value = "S"; break;
                case IBMCompilerOptionName.COPYLOC: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.CURRENCY: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.DATA: IsActivated = true; Value = "31"; break;
                case IBMCompilerOptionName.DBCS: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.DECK: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.DEFINE: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.DIAGTRUNC: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.DISPSIGN: IsActivated = true; Value = "COMPAT"; break;
                case IBMCompilerOptionName.DLL: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.DUMP: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.DYNAM: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.EXIT: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.EXPORTALL: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.FASTSRT: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.FLAG: IsActivated = true; Value = "I,I"; break;
                case IBMCompilerOptionName.FLAGSTD: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.HGPR: IsActivated = true; Value = "PRESERVE"; break;
                case IBMCompilerOptionName.INITCHECK: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.INITIAL: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.INTDATE: IsActivated = true; Value = "ANSI"; break;
                case IBMCompilerOptionName.INVDATA: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.LANGUAGE: IsActivated = true; Value = "ENGLISH"; break;
                case IBMCompilerOptionName.LIB: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.LINECOUNT: IsActivated = true; Value = "60"; break;
                case IBMCompilerOptionName.LIST: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.LP: IsActivated = true; Value = "32"; break;
                case IBMCompilerOptionName.MAP: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.MAXPCF: IsActivated = true; Value = "60000"; break;
                case IBMCompilerOptionName.MDECK: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.NAME: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.NSYMBOL: IsActivated = true; Value = "NATIONAL"; break;
                case IBMCompilerOptionName.NUMBER: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.NUMCHECK: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.NUMPROC: IsActivated = true; Value = "NOPFD"; break;
                case IBMCompilerOptionName.OBJECT: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.OFFSET: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.OPTFILE: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.OPTIMIZE: IsActivated = true; Value = "0"; break;
                case IBMCompilerOptionName.OUTDD: IsActivated = true; Value = "SYSOUT"; break;
                case IBMCompilerOptionName.PARMCHECK: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.PGMNAME: IsActivated = true; Value = "COMPAT"; break;
                case IBMCompilerOptionName.QUOTE: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.RENT: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.RMODE: IsActivated = true; Value = "AUTO"; break;
                case IBMCompilerOptionName.SEQUENCE: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.SIZE: IsActivated = true; Value = "5000000"; break;
                case IBMCompilerOptionName.SOURCE: IsActivated = true; Value = "DEC"; break;
                case IBMCompilerOptionName.SPACE: IsActivated = true; Value = "1"; break;
                case IBMCompilerOptionName.SQL: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.SQLCCSID: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.SQLIMS: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.SSRANGE: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.STGOPT: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.SUPPRESS: IsActivated = true; Value = null; break;
                case IBMCompilerOptionName.TERMINAL: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.TEST: IsActivated = false; Value = "NODWARF"; break;
                case IBMCompilerOptionName.THREAD: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.TRUNC: IsActivated = true; Value = "STD"; break;
                case IBMCompilerOptionName.VBREF: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.WORD: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.XREF: IsActivated = true; Value = "FULL"; break;
                case IBMCompilerOptionName.ZONECHECK: IsActivated = false; Value = null; break;
                case IBMCompilerOptionName.ZONEDATA: IsActivated = true; Value = "PFD"; break;
                case IBMCompilerOptionName.ZWB: IsActivated = true; Value = null; break;
            }
        }
        
        /// <summary>
        /// Set the option status and value from the word and parameters found in source text
        /// </summary>
        public bool SetStatusAndValue(string optionWord, string? optionParameters)
        {
            // Special case QUOTE / APOST
            if(Name == IBMCompilerOptionName.QUOTE)
            {
                if(optionWord == "APOST")
                {
                    IsActivated = false;
                }
                else
                {
                    IsActivated = true;
                }
            }
            // For all other option words, the format is word/NOword or abbr/NOabbr
            else
            {
                if(optionWord.StartsWith("NO"))
                {
                    IsActivated = false;
                }
                else
                {
                    IsActivated = true;
                }
            }
            if(string.IsNullOrEmpty(optionParameters))
            {
                Value = null;
            }
            else
            {
                Value = optionParameters;
            }
            return true;
        }
    }

    /// <summary>
    /// Current status and value for all options supported by the IBM Enterprise Cobol 5.1 for zOs.
    /// Options specified as installation default, compiler parameters, or CBL/PROCESS directives in source code.
    /// </summary>
    public class IBMCompilerOptions
    {
        /// <summary>
        /// Use ADATA when you want the compiler to create a SYSADATA file that contains records of additional compilation information
        /// </summary>
        public IBMCompilerOptionStatus ADATA { get; private set; }

        /// <summary>
        /// ADV has meaning only if you use WRITE...ADVANCING in your source code. With ADV in effect, the compiler adds 1 byte to the record length to account for the printer control character.
        /// </summary>
        public IBMCompilerOptionStatus ADV { get; private set; }

        /// <summary>
        /// The AFP option controls the compiler usage of the Additional Floating Point (AFP) registers that are provided by z/Architecture processors.
        /// </summary>
        public IBMCompilerOptionStatus AFP { get; private set; }

        /// <summary>
        /// The ARCH option specifies the machine architecture for which the executable program instructions are to be generated.
        /// </summary>
        public IBMCompilerOptionStatus ARCH { get; private set; }

        /// <summary>
        /// ARITH affects the maximum number of digits that you can code for integers, and the number of digits used in fixed-point intermediate results.
        /// </summary>
        public IBMCompilerOptionStatus ARITH { get; private set; }

        /// <summary>
        /// If you specify AWO, an implicit APPLY WRITE-ONLY clause is activated for all QSAM files in the program that have blocked variable-length records.
        /// </summary>
        public IBMCompilerOptionStatus AWO { get; private set; }

        /// <summary>
        /// Use BLOCK0 to change the compiler default for QSAM files from unblocked to blocked (as if BLOCK CONTAINS 0 were specified) and thus gain the benefit of system-determined blocking for output files.
        /// </summary>
        public IBMCompilerOptionStatus BLOCK0 { get; private set; }

        /// <summary>
        /// Use BUFSIZE to allocate an amount of main storage to the buffer for each compiler work data set. Usually, a large buffer size improves the performance of the compiler.
        /// </summary>
        public IBMCompilerOptionStatus BUFSIZE { get; private set; }

        /// <summary>
        /// The CICS compiler option enables the integrated CICS translator and lets you specify CICS suboptions. You must use the CICS option if your COBOL source program contains EXEC CICS or EXEC DLI statements and the program has not been processed by the separate CICS translator.
        /// </summary>
        public IBMCompilerOptionStatus CICS { get; private set; }

        /// <summary>
        /// Use CODEPAGE to specify the coded character set identifier (CCSID) for an EBCDIC code page for processing compile-time and runtime COBOL operations that are sensitive to character encoding.
        /// </summary>
        public IBMCompilerOptionStatus CODEPAGE { get; private set; }

        /// <summary>
        /// Use the COPYLOC compiler option to add either a PDSE (or PDS) dataset or z/OS UNIX directory as an additional location to be searched for copy members during the library phase.
        /// </summary>
        public IBMCompilerOptionStatus COPYLOC { get; private set; }

        /// <summary>
        /// Use the COMPILE option only if you want to force full compilation even in the presence of serious errors. All diagnostics and object code will be generated. Do not try to run the object code if the compilation resulted in serious errors: the results could be unpredictable or an abnormal termination could occur.
        /// </summary>
        public IBMCompilerOptionStatus COMPILE { get; private set; }

        /// <summary>
        /// You can use the CURRENCY option to provide an alternate default currency symbol to be used for a COBOL program. (The default currency symbol is the dollar sign ($).)
        /// </summary>
        public IBMCompilerOptionStatus CURRENCY { get; private set; }

        /// <summary>
        /// The DATA option affects whether storage for dynamic data areas and other dynamic runtime storage is obtained from above or below the 16 MB line.
        /// </summary>
        public IBMCompilerOptionStatus DATA { get; private set; }

        /// <summary>
        /// Using DBCS causes the compiler to recognize X’0E’ (SO) and X’0F’ (SI) as shift codes for the double-byte portion of an alphanumeric literal.
        /// </summary>
        public IBMCompilerOptionStatus DBCS { get; private set; }

        /// <summary>
        /// Use DECK to produce object code in the form of 80-column records. If you use the DECK option, be certain that SYSPUNCH is defined in your JCL for compilation.
        /// </summary>
        public IBMCompilerOptionStatus DECK { get; private set; }

        /// <summary>
        /// Use the DEFINE compiler option to assign a literal value to a compilation variable that is defined in the program by using the DEFINE directive with the PARAMETER phrase.
        /// </summary>
        public IBMCompilerOptionStatus DEFINE { get; private set; }

        /// <summary>
        /// DIAGTRUNC causes the compiler to issue a severity-4 (Warning) diagnostic message for MOVE statements that have numeric receivers when the receiving data item has fewer integer positions than the sending data item or literal. In statements that have multiple receivers, the message is issued separately for each receiver that could be truncated.
        /// </summary>
        public IBMCompilerOptionStatus DIAGTRUNC { get; private set; }

        /// <summary>
        /// The DISPSIGN option controls output formatting for DISPLAY of signed numeric items.
        /// </summary>
        public IBMCompilerOptionStatus DISPSIGN { get; private set; }

        /// <summary>
        /// Use DLL to instruct the compiler to generate an object module that is enabled for dynamic link library (DLL) support. DLL enablement is required if the program will be part of a DLL, will reference DLLs, or if the program contains object-oriented COBOL syntax such as INVOKE statements or class definitions.
        /// </summary>
        public IBMCompilerOptionStatus DLL { get; private set; }

        /// <summary>
        /// Use DUMP to produce a system dump at compile time for an internal compiler error.
        /// </summary>
        public IBMCompilerOptionStatus DUMP { get; private set; }

        /// <summary>
        /// Use DYNAM to cause nonnested, separately compiled programs invoked through the CALL literal statement to be loaded for CALL, and deleted for CANCEL, dynamically at run time.
        /// </summary>
        public IBMCompilerOptionStatus DYNAM { get; private set; }

        /// <summary>
        /// Use the EXIT option to provide user-supplied modules in place of various compiler functions.
        /// </summary>
        public IBMCompilerOptionStatus EXIT { get; private set; }

        /// <summary>
        /// Use EXPORTALL to instruct the compiler to automatically export the PROGRAM-ID name and each alternate entry-point name from each program definition when the object deck is link-edited to form a DLL.
        /// </summary>
        public IBMCompilerOptionStatus EXPORTALL { get; private set; }

        /// <summary>
        /// Use FASTSRT to let IBM DFSORT, or an equivalent product, perform sort input and output instead of Enterprise COBOL.
        /// </summary>
        public IBMCompilerOptionStatus FASTSRT { get; private set; }

        /// <summary>
        /// Use FLAG(x) to produce diagnostic messages at the end of the source listing for errors of a severity level x or above.
        /// </summary>
        public IBMCompilerOptionStatus FLAG { get; private set; }

        /// <summary>
        /// Use FLAGSTD to specify the level or subset of Standard COBOL 85 to be regarded as conforming, and to get informational messages about Standard COBOL 85 elements that are included in your program.
        /// </summary>
        public IBMCompilerOptionStatus FLAGSTD { get; private set; }

        /// <summary>
        /// The HGPR option controls the compiler usage of the 64-bit registers provided by z/Architecture processors.
        /// </summary>
        public IBMCompilerOptionStatus HGPR { get; private set; }

        /// <summary>
        /// Use the INITCHECK option to have the compiler check for uninitialized data items and issue warning messages when they are used without being initialized.
        /// </summary>
        public IBMCompilerOptionStatus INITCHECK{ get; private set; }

        /// <summary>
        /// The INITIAL compiler option causes a program and all of its nested programs to behave as if the IS INITIAL clause was specified on the PROGRAM-ID paragraph.
        /// </summary>
        public IBMCompilerOptionStatus INITIAL { get; private set; }

        /// <summary>
        /// The INVDATA option tells the compiler whether the data in USAGE DISPLAY and PACKED-DECIMAL data items is valid, and if not, what the behavior of the compiler should be.
        /// </summary>
        public IBMCompilerOptionStatus INVDATA { get; private set; }

        /// <summary>
        /// INTDATE(ANSI) instructs the compiler to use the Standard COBOL 85 starting date for integer dates used with date intrinsic functions. Day 1 is Jan 1, 1601. INTDATE(LILIAN) instructs the compiler to use the Language Environment Lilian starting date for integer dates used with date intrinsic functions. Day 1 is Oct 15, 1582.
        /// </summary>
        public IBMCompilerOptionStatus INTDATE { get; private set; }

        /// <summary>
        /// Use the LANGUAGE option to select the language in which compiler output will be printed. The information that will be printed in the selected language includes diagnostic messages, source listing page and scale headers, FIPS message headers, message summary headers, compilation summary, and headers and notations that result from the selection of certain compiler options (MAP, XREF, VBREF, and FLAGSTD).
        /// </summary>
        public IBMCompilerOptionStatus LANGUAGE { get; private set; }

        /// <summary>
        /// Use LINECOUNT(nnn) to specify the number of lines to be printed on each page of the compilation listing, or use LINECOUNT(0) to suppress pagination.
        /// </summary>
        public IBMCompilerOptionStatus LINECOUNT { get; private set; }

        /// <summary>
        /// Use the LIST compiler option to produce a listing of the assembler-language expansion of your source code.
        /// </summary>
        public IBMCompilerOptionStatus LIST { get; private set; }

        /// <summary>
        /// Use the LP compiler option to indicate whether a AMODE 31 (31-bit) or AMODE 64 (64-bit) program should be generated with the related language features enabled.
        /// </summary>
        public IBMCompilerOptionStatus LP { get; private set; }

        /// <summary>
        /// Use MAP to produce a listing of the items defined in the DATA DIVISION.
        /// </summary>
        public IBMCompilerOptionStatus MAP { get; private set; }

        /// <summary>
        /// Use the MAXPCF option to specify a maximum program complexity factor value. The program complexity factor (PCF) is computed by the compiler and the computed value is in the listing file. If the PCF of your program exceeds the maximum value, the compiler will automatically reduce the optimization level to speed up the compilation and use less storage. Therefore, when you compile a suite of programs, you do not have to specify an OPTIMIZE option value for each program.
        /// </summary>
        public IBMCompilerOptionStatus MAXPCF { get; private set; }

        /// <summary>
        /// The MDECK compiler option specifies that a copy of the updated input source after library processing (that is, the result of COPY, BASIS, REPLACE, EXEC SQL INCLUDE, and EXEC SQLIMS INCLUDE statements) is written to a file.
        /// </summary>
        public IBMCompilerOptionStatus MDECK { get; private set; }

        /// <summary>
        /// Use NAME to generate a link-edit NAME card for each object module. You can also use NAME to generate names for each load module when you are doing batch compilations.
        /// </summary>
        public IBMCompilerOptionStatus NAME { get; private set; }

        /// <summary>
        /// The NSYMBOL option controls the interpretation of the N symbol used in literals and PICTURE clauses, indicating whether national or DBCS processing is assumed.
        /// </summary>
        public IBMCompilerOptionStatus NSYMBOL { get; private set; }

        /// <summary>
        /// Use the NUMBER compiler option if you have line numbers in your source code and want those numbers to be used in error messages and SOURCE, MAP, LIST, and XREF listings.
        /// </summary>
        public IBMCompilerOptionStatus NUMBER { get; private set; }

        /// <summary>
        /// The NUMCHECK compiler option tells the compiler whether to generate extra code to validate data items when they are used as sending data items.
        /// </summary>
        public IBMCompilerOptionStatus NUMCHECK { get; private set; }

        /// <summary>
        /// Use NUMPROC(NOPFD) if your internal decimal and zoned decimal data might use nonpreferred signs.
        /// </summary>
        public IBMCompilerOptionStatus NUMPROC { get; private set; }

        /// <summary>
        /// Use OBJECT to write the generated object code to a file to be used as input for the binder.
        /// </summary>
        public IBMCompilerOptionStatus OBJECT { get; private set; }

        /// <summary>
        /// Use OFFSET to produce a condensed PROCEDURE DIVISION listing.
        /// </summary>
        public IBMCompilerOptionStatus OFFSET { get; private set; }

        /// <summary>
        /// Use OPTFILE to enable the specifying of COBOL compiler options in a data set. Using a compiler-option data set circumvents the 100-character limit on options specified in a JCL PARM string.
        /// </summary>
        public IBMCompilerOptionStatus OPTFILE { get; private set; }

        /// <summary>
        /// Use OPTIMIZE to reduce the run time of your object program. Optimization might also reduce the amount of storage your object program uses.
        /// </summary>
        public IBMCompilerOptionStatus OPTIMIZE { get; private set; }

        /// <summary>
        /// Use OUTDD to specify that you want DISPLAY output that is directed to the system logical output device to go to a specific ddname.
        /// </summary>
        public IBMCompilerOptionStatus OUTDD { get; private set; }

        /// <summary>
        /// The PARMCHECK option tells the compiler to generate an extra data item following the last item in WORKING-STORAGE.
        /// </summary>
        public IBMCompilerOptionStatus PARMCHECK { get; private set; }

        /// <summary>
        /// The PGMNAME option controls the handling of program-names and entry-point names.
        /// </summary>
        public IBMCompilerOptionStatus PGMNAME { get; private set; }

        /// <summary>
        /// Use QUOTE if you want the figurative constant [ALL] QUOTE or [ALL] QUOTES to represent one or more quotation mark () characters. Use APOST if you want the figurative constant [ALL] QUOTE or [ALL] QUOTES to represent one or more single quotation mark (') characters.
        /// </summary>
        public IBMCompilerOptionStatus QUOTE { get; private set; }

        /// <summary>
        /// A program compiled as RENT is generated as a reentrant object program. A program compiled as NORENT is generated as a nonreentrant object program.
        /// </summary>
        public IBMCompilerOptionStatus RENT { get; private set; }

        /// <summary>
        /// The RMODE setting influences the RMODE (residency mode) of your generated object program.
        /// </summary>
        public IBMCompilerOptionStatus RMODE { get; private set; }

        /// <summary>
        /// When you use SEQUENCE, the compiler examines columns 1 through 6 to check that the source statements are arranged in ascending order according to their EBCDIC collating sequence. The compiler issues a diagnostic message if any statements are not in ascending order.
        /// </summary>
        public IBMCompilerOptionStatus SEQUENCE { get; private set; }

        /// <summary>
        /// Use SIZE to indicate the amount of main storage to be made available to the compiler front end for compilation. The compiler front end is the phase of compilation that occurs before code generation and optimization.
        /// </summary>
        public IBMCompilerOptionStatus SIZE { get; private set; }

        /// <summary>
        /// Use SOURCE to get a listing of your source program. This listing will include any statements embedded by PROCESS or COPY statements.
        /// </summary>
        public IBMCompilerOptionStatus SOURCE { get; private set; }

        /// <summary>
        /// Use SPACE to select single-, double-, or triple-spacing in your source code listing.
        /// </summary>
        public IBMCompilerOptionStatus SPACE { get; private set; }

        /// <summary>
        /// Use the SQL compiler option to enable the DB2 coprocessor and to specify DB2 suboptions. You must specify the SQL option if a COBOL source program contains SQL statements (EXEC SQL statements) and the program has not been processed by the DB2 precompiler.
        /// </summary>
        public IBMCompilerOptionStatus SQL { get; private set; }

        /// <summary>
        /// Use the SQLCCSID compiler option to control whether the CODEPAGE compiler option will influence the processing of SQL statements in your COBOL programs.
        /// </summary>
        public IBMCompilerOptionStatus SQLCCSID { get; private set; }

        /// <summary>
        /// Use the SQLIMS compiler option to enable the IMS SQL coprocessor and to specify Information Management System (IMS) suboptions. You must specify the SQLIMS option if a COBOL source program contains SQLIMS statements (EXEC SQLIMS statements).
        /// </summary>
        public IBMCompilerOptionStatus SQLIMS { get; private set; }

        /// <summary>
        /// Use SSRANGE to generate code that checks for out-of-range storage references.
        /// </summary>
        public IBMCompilerOptionStatus SSRANGE { get; private set; }

        /// <summary>
        /// The STGOPT option controls storage optimization.
        /// </summary>
        public IBMCompilerOptionStatus STGOPT { get; private set; }

        /// <summary>
        /// Use the NOSUPPRESS option to ignore the SUPPRESS phrase of all COPY statements in a program so that copybook information can appear in the listing.
        /// </summary>
        public IBMCompilerOptionStatus SUPPRESS { get; private set; }

        /// <summary>
        /// Use TERMINAL to send progress and diagnostic messages to the SYSTERM ddname.
        /// </summary>
        public IBMCompilerOptionStatus TERMINAL { get; private set; }

        /// <summary>
        /// Use TEST to produce object code that enables debugging and problem determination tools such as Debug Tool and Fault Analyzer. With TEST, you can also enable the inclusion of symbolic variables in the formatted dumps that are produced by Language Environment.
        /// </summary>
        public IBMCompilerOptionStatus TEST { get; private set; }

        /// <summary>
        /// THREAD indicates that a COBOL program is to be enabled for execution in a Language Environment enclave that has multiple POSIX threads or PL/I tasks.
        /// </summary>
        public IBMCompilerOptionStatus THREAD { get; private set; }

        /// <summary>
        /// TRUNC affects the way that binary data is truncated during moves and arithmetic operations.
        /// </summary>
        public IBMCompilerOptionStatus TRUNC { get; private set; }

        /// <summary>
        /// Use VBREF to get a cross-reference between all verbs used in the source program and the line numbers in which they are used. VBREF also produces a summary of the number of times each verb was used in the program.
        /// </summary>
        public IBMCompilerOptionStatus VBREF { get; private set; }

        /// <summary>
        /// Use WORD(xxxx) to specify that an alternate reserved-word table is to be used during compilation.
        /// </summary>
        public IBMCompilerOptionStatus WORD { get; private set; }

        /// <summary>
        /// Use XREF to produce a sorted cross-reference listing.
        /// </summary>
        public IBMCompilerOptionStatus XREF { get; private set; }

        /// <summary>
        /// Use the ZONECHECK option to have the compiler generate IF NUMERIC class tests for zoned decimal data items that are used as sending data items.
        /// </summary>
        public IBMCompilerOptionStatus ZONECHECK { get; private set; }

        /// <summary>
        /// The ZONEDATA option tells the compiler whether the data in USAGE DISPLAY and PACKED-DECIMAL data items is valid, and if not, what the behavior of the compiler should be.
        /// </summary>
        public IBMCompilerOptionStatus ZONEDATA { get; private set; }

        /// <summary>
        /// If you compile using ZWB, the compiler removes the sign from a signed zoned decimal (DISPLAY) field before comparing this field to an alphanumeric elementary field during execution.
        /// </summary>
        public IBMCompilerOptionStatus ZWB { get; private set; }

        // Conversion of option words in source text to option name enumeration
        private readonly IDictionary<string, IBMCompilerOptionStatus> optionWordToOptionName = new Dictionary<string, IBMCompilerOptionStatus>(194);
              
        // Deprecated options which are not supported anymore with the corresponding warning message to be displayed
        private static readonly IDictionary<string, string> deprecatedOptions = new Dictionary<string, string>(1, StringComparer.OrdinalIgnoreCase);

        // Not allowed options with the corresponding error message to be displayed
        private static readonly IDictionary<string, string> notAllowedOptions = new Dictionary<string, string>(1, StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// Initialize all compiler options to their default status and value
        /// </summary>
        public IBMCompilerOptions()
        {
            ADATA = new IBMCompilerOptionStatus(IBMCompilerOptionName.ADATA);
            ADV = new IBMCompilerOptionStatus(IBMCompilerOptionName.ADV);
            AFP = new IBMCompilerOptionStatus(IBMCompilerOptionName.AFP);
            ARCH = new IBMCompilerOptionStatus(IBMCompilerOptionName.ARCH);
            ARITH = new IBMCompilerOptionStatus(IBMCompilerOptionName.ARITH);
            AWO = new IBMCompilerOptionStatus(IBMCompilerOptionName.AWO);
            BLOCK0 = new IBMCompilerOptionStatus(IBMCompilerOptionName.BLOCK0);
            BUFSIZE = new IBMCompilerOptionStatus(IBMCompilerOptionName.BUFSIZE);
            CICS = new IBMCompilerOptionStatus(IBMCompilerOptionName.CICS);
            CODEPAGE = new IBMCompilerOptionStatus(IBMCompilerOptionName.CODEPAGE);
            COPYLOC = new IBMCompilerOptionStatus(IBMCompilerOptionName.COPYLOC);
            COMPILE = new IBMCompilerOptionStatus(IBMCompilerOptionName.COMPILE);
            CURRENCY = new IBMCompilerOptionStatus(IBMCompilerOptionName.CURRENCY);
            DATA = new IBMCompilerOptionStatus(IBMCompilerOptionName.DATA);
            DBCS = new IBMCompilerOptionStatus(IBMCompilerOptionName.DBCS);
            DECK = new IBMCompilerOptionStatus(IBMCompilerOptionName.DECK);
            DEFINE = new IBMCompilerOptionStatus(IBMCompilerOptionName.DEFINE);
            DIAGTRUNC = new IBMCompilerOptionStatus(IBMCompilerOptionName.DIAGTRUNC);
            DISPSIGN = new IBMCompilerOptionStatus(IBMCompilerOptionName.DISPSIGN);
            DLL = new IBMCompilerOptionStatus(IBMCompilerOptionName.DLL);
            DUMP = new IBMCompilerOptionStatus(IBMCompilerOptionName.DUMP);
            DYNAM = new IBMCompilerOptionStatus(IBMCompilerOptionName.DYNAM);
            EXIT = new IBMCompilerOptionStatus(IBMCompilerOptionName.EXIT);
            EXPORTALL = new IBMCompilerOptionStatus(IBMCompilerOptionName.EXPORTALL);
            FASTSRT = new IBMCompilerOptionStatus(IBMCompilerOptionName.FASTSRT);
            FLAG = new IBMCompilerOptionStatus(IBMCompilerOptionName.FLAG);
            FLAGSTD = new IBMCompilerOptionStatus(IBMCompilerOptionName.FLAGSTD);
            HGPR = new IBMCompilerOptionStatus(IBMCompilerOptionName.HGPR);
            INITCHECK = new IBMCompilerOptionStatus(IBMCompilerOptionName.INITCHECK);
            INITIAL = new IBMCompilerOptionStatus(IBMCompilerOptionName.INITIAL);
            INVDATA = new IBMCompilerOptionStatus(IBMCompilerOptionName.INVDATA);
            INTDATE = new IBMCompilerOptionStatus(IBMCompilerOptionName.INTDATE);
            LANGUAGE = new IBMCompilerOptionStatus(IBMCompilerOptionName.LANGUAGE);
            LINECOUNT = new IBMCompilerOptionStatus(IBMCompilerOptionName.LINECOUNT);
            LIST = new IBMCompilerOptionStatus(IBMCompilerOptionName.LIST);
            LP = new IBMCompilerOptionStatus(IBMCompilerOptionName.LP);
            MAP = new IBMCompilerOptionStatus(IBMCompilerOptionName.MAP);
            MAXPCF = new IBMCompilerOptionStatus(IBMCompilerOptionName.MAXPCF);
            MDECK = new IBMCompilerOptionStatus(IBMCompilerOptionName.MDECK);
            NAME = new IBMCompilerOptionStatus(IBMCompilerOptionName.NAME);
            NSYMBOL = new IBMCompilerOptionStatus(IBMCompilerOptionName.NSYMBOL);
            NUMBER = new IBMCompilerOptionStatus(IBMCompilerOptionName.NUMBER);
            NUMCHECK = new IBMCompilerOptionStatus(IBMCompilerOptionName.NUMCHECK);
            NUMPROC = new IBMCompilerOptionStatus(IBMCompilerOptionName.NUMPROC);
            OBJECT = new IBMCompilerOptionStatus(IBMCompilerOptionName.OBJECT);
            OFFSET = new IBMCompilerOptionStatus(IBMCompilerOptionName.OFFSET);
            OPTFILE = new IBMCompilerOptionStatus(IBMCompilerOptionName.OPTFILE);
            OPTIMIZE = new IBMCompilerOptionStatus(IBMCompilerOptionName.OPTIMIZE);
            OUTDD = new IBMCompilerOptionStatus(IBMCompilerOptionName.OUTDD);
            PARMCHECK = new IBMCompilerOptionStatus(IBMCompilerOptionName.PARMCHECK);
            PGMNAME = new IBMCompilerOptionStatus(IBMCompilerOptionName.PGMNAME);
            QUOTE = new IBMCompilerOptionStatus(IBMCompilerOptionName.QUOTE);
            RENT = new IBMCompilerOptionStatus(IBMCompilerOptionName.RENT);
            RMODE = new IBMCompilerOptionStatus(IBMCompilerOptionName.RMODE);
            SEQUENCE = new IBMCompilerOptionStatus(IBMCompilerOptionName.SEQUENCE);
            SIZE = new IBMCompilerOptionStatus(IBMCompilerOptionName.SIZE);
            SOURCE = new IBMCompilerOptionStatus(IBMCompilerOptionName.SOURCE);
            SPACE = new IBMCompilerOptionStatus(IBMCompilerOptionName.SPACE);
            SQL = new IBMCompilerOptionStatus(IBMCompilerOptionName.SQL);
            SQLCCSID = new IBMCompilerOptionStatus(IBMCompilerOptionName.SQLCCSID);
            SQLIMS = new IBMCompilerOptionStatus(IBMCompilerOptionName.SQLIMS);
            SSRANGE = new IBMCompilerOptionStatus(IBMCompilerOptionName.SSRANGE);
            STGOPT = new IBMCompilerOptionStatus(IBMCompilerOptionName.STGOPT);
            SUPPRESS = new IBMCompilerOptionStatus(IBMCompilerOptionName.SUPPRESS);
            TERMINAL = new IBMCompilerOptionStatus(IBMCompilerOptionName.TERMINAL);
            TEST = new IBMCompilerOptionStatus(IBMCompilerOptionName.TEST);
            THREAD = new IBMCompilerOptionStatus(IBMCompilerOptionName.THREAD);
            TRUNC = new IBMCompilerOptionStatus(IBMCompilerOptionName.TRUNC);
            VBREF = new IBMCompilerOptionStatus(IBMCompilerOptionName.VBREF);
            WORD = new IBMCompilerOptionStatus(IBMCompilerOptionName.WORD);
            XREF = new IBMCompilerOptionStatus(IBMCompilerOptionName.XREF);
            ZONECHECK = new IBMCompilerOptionStatus(IBMCompilerOptionName.ZONECHECK);
            ZONEDATA = new IBMCompilerOptionStatus(IBMCompilerOptionName.ZONEDATA);
            ZWB = new IBMCompilerOptionStatus(IBMCompilerOptionName.ZWB);

            optionWordToOptionName["ADATA"] = ADATA; optionWordToOptionName["NOADATA"] = ADATA;
            optionWordToOptionName["ADV"] = ADV; optionWordToOptionName["NOADV"] = ADV;
            optionWordToOptionName["AFP"] = AFP;
            optionWordToOptionName["ARCH"] = ARCH;
            optionWordToOptionName["ARITH"] = ARITH; optionWordToOptionName["AR"] = ARITH;
            optionWordToOptionName["AWO"] = AWO; optionWordToOptionName["NOAWO"] = AWO;
            optionWordToOptionName["BLOCK0"] = BLOCK0; optionWordToOptionName["NOBLOCK0"] = BLOCK0;
            optionWordToOptionName["BUFSIZE"] = BUFSIZE; optionWordToOptionName["BUF"] = BUFSIZE;
            optionWordToOptionName["CICS"] = CICS; optionWordToOptionName["NOCICS"] = CICS;
            optionWordToOptionName["CODEPAGE"] = CODEPAGE; optionWordToOptionName["CP"] = CODEPAGE;
            optionWordToOptionName["COPYLOC"] = COPYLOC; optionWordToOptionName["CPLC"] = COPYLOC; optionWordToOptionName["NOCOPYLOC"] = COPYLOC; optionWordToOptionName["NOCPLC"] = COPYLOC;
            optionWordToOptionName["COMPILE"] = COMPILE; optionWordToOptionName["C"] = COMPILE; optionWordToOptionName["NOCOMPILE"] = COMPILE; optionWordToOptionName["NOC"] = COMPILE;
            optionWordToOptionName["CURRENCY"] = CURRENCY; optionWordToOptionName["CURR"] = CURRENCY; optionWordToOptionName["NOCURRENCY"] = CURRENCY; optionWordToOptionName["NOCURR"] = CURRENCY;
            optionWordToOptionName["DATA"] = DATA;
            optionWordToOptionName["DBCS"] = DBCS; optionWordToOptionName["NODBCS"] = DBCS;
            optionWordToOptionName["DECK"] = DECK; optionWordToOptionName["D"] = DECK; optionWordToOptionName["NODECK"] = DECK; optionWordToOptionName["NOD"] = DECK;
            optionWordToOptionName["DEFINE"] = DEFINE; optionWordToOptionName["DEF"] = DEFINE; optionWordToOptionName["NODEFINE"] = DEFINE; optionWordToOptionName["NODEF"] = DEFINE;
            optionWordToOptionName["DIAGTRUNC"] = DIAGTRUNC; optionWordToOptionName["DTR"] = DIAGTRUNC; optionWordToOptionName["NODIAGTRUNC"] = DIAGTRUNC; optionWordToOptionName["NODTR"] = DIAGTRUNC;
            optionWordToOptionName["DISPSIGN"] = DISPSIGN; optionWordToOptionName["DS"] = DISPSIGN;
            optionWordToOptionName["DLL"] = DLL; optionWordToOptionName["NODLL"] = DLL;
            optionWordToOptionName["DUMP"] = DUMP; optionWordToOptionName["DU"] = DUMP; optionWordToOptionName["NODUMP"] = DUMP; optionWordToOptionName["NODU"] = DUMP;
            optionWordToOptionName["DYNAM"] = DYNAM; optionWordToOptionName["DYN"] = DYNAM; optionWordToOptionName["NODYNAM"] = DYNAM; optionWordToOptionName["NODYN"] = DYNAM;
            optionWordToOptionName["EXIT"] = EXIT; optionWordToOptionName["EX"] = EXIT; optionWordToOptionName["NOEXIT"] = EXIT; optionWordToOptionName["NOEX"] = EXIT;
            optionWordToOptionName["EXPORTALL"] = EXPORTALL; optionWordToOptionName["EXP"] = EXPORTALL; optionWordToOptionName["NOEXPORTALL"] = EXPORTALL; optionWordToOptionName["NOEXP"] = EXPORTALL;
            optionWordToOptionName["FASTSRT"] = FASTSRT; optionWordToOptionName["FSRT"] = FASTSRT; optionWordToOptionName["NOFASTSRT"] = FASTSRT; optionWordToOptionName["NOFSRT"] = FASTSRT;
            optionWordToOptionName["FLAG"] = FLAG; optionWordToOptionName["F"] = FLAG; optionWordToOptionName["NOFLAG"] = FLAG; optionWordToOptionName["NOF"] = FLAG;
            optionWordToOptionName["FLAGSTD"] = FLAGSTD; optionWordToOptionName["NOFLAGSTD"] = FLAGSTD;
            optionWordToOptionName["HGPR"] = HGPR;
            optionWordToOptionName["INITCHECK"] = INITCHECK; optionWordToOptionName["IC"] = INITCHECK; optionWordToOptionName["NOINITCHECK"] = INITCHECK; optionWordToOptionName["NOIC"] = INITCHECK;
            optionWordToOptionName["INITIAL"] = INITIAL; optionWordToOptionName["NOINITIAL"] = INITIAL;
            optionWordToOptionName["INVDATA"] = INVDATA; optionWordToOptionName["INVD"] = INVDATA; optionWordToOptionName["NOINVDATA"] = INVDATA; optionWordToOptionName["NOINVD"] = INVDATA;
            optionWordToOptionName["INTDATE"] = INTDATE;
            optionWordToOptionName["LANGUAGE"] = LANGUAGE; optionWordToOptionName["LANG"] = LANGUAGE;
            optionWordToOptionName["LINECOUNT"] = LINECOUNT; optionWordToOptionName["LC"] = LINECOUNT;
            optionWordToOptionName["LIST"] = LIST; optionWordToOptionName["NOLIST"] = LIST;
            optionWordToOptionName["MAP"] = MAP; optionWordToOptionName["NOMAP"] = MAP;
            optionWordToOptionName["MAXPCF"] = MAXPCF;
            optionWordToOptionName["MDECK"] = MDECK; optionWordToOptionName["MD"] = MDECK; optionWordToOptionName["NOMDECK"] = MDECK; optionWordToOptionName["NOMD"] = MDECK;
            optionWordToOptionName["NAME"] = NAME; optionWordToOptionName["NONAME"] = NAME;
            optionWordToOptionName["NSYMBOL"] = NSYMBOL; optionWordToOptionName["NS"] = NSYMBOL;
            optionWordToOptionName["NUMBER"] = NUMBER; optionWordToOptionName["NONUMBER"] = NUMBER;
            optionWordToOptionName["NUMCHECK"] = NUMCHECK; optionWordToOptionName["NC"] = NUMCHECK; optionWordToOptionName["NONUMCHECK"] = NUMCHECK; optionWordToOptionName["NONC"] = NUMCHECK;
            optionWordToOptionName["NUMPROC"] = NUMPROC;
            optionWordToOptionName["OBJECT"] = OBJECT; optionWordToOptionName["OBJ"] = OBJECT; optionWordToOptionName["NOOBJECT"] = OBJECT; optionWordToOptionName["NOOBJ"] = OBJECT;
            optionWordToOptionName["OFFSET"] = OFFSET; optionWordToOptionName["OFF"] = OFFSET; optionWordToOptionName["NOOFFSET"] = OFFSET; optionWordToOptionName["NOOFF"] = OFFSET;
            optionWordToOptionName["OPTFILE"] = OPTFILE;
            optionWordToOptionName["OPTIMIZE"] = OPTIMIZE; optionWordToOptionName["OPT"] = OPTIMIZE;
            optionWordToOptionName["OUTDD"] = OUTDD; optionWordToOptionName["OUT"] = OUTDD;
            optionWordToOptionName["PARMCHECK"] = PARMCHECK; optionWordToOptionName["PC"] = PARMCHECK; optionWordToOptionName["NOPARMCHECK"] = PARMCHECK; optionWordToOptionName["NOPC"] = PARMCHECK;
            optionWordToOptionName["PGMNAME"] = PGMNAME; optionWordToOptionName["PGMN"] = PGMNAME;
            optionWordToOptionName["QUOTE"] = QUOTE; optionWordToOptionName["Q"] = QUOTE; optionWordToOptionName["APOST"] = QUOTE;
            optionWordToOptionName["RENT"] = RENT; optionWordToOptionName["NORENT"] = RENT;
            optionWordToOptionName["RMODE"] = RMODE;
            optionWordToOptionName["SEQUENCE"] = SEQUENCE; optionWordToOptionName["SEQ"] = SEQUENCE; optionWordToOptionName["NOSEQUENCE"] = SEQUENCE; optionWordToOptionName["NOSEQ"] = SEQUENCE;
            optionWordToOptionName["SIZE"] = SIZE; optionWordToOptionName["SZ"] = SIZE;
            optionWordToOptionName["SOURCE"] = SOURCE; optionWordToOptionName["S"] = SOURCE; optionWordToOptionName["NOSOURCE"] = SOURCE; optionWordToOptionName["NOS"] = SOURCE;
            optionWordToOptionName["SPACE"] = SPACE;
            optionWordToOptionName["SQL"] = SQL; optionWordToOptionName["NOSQL"] = SQL;
            optionWordToOptionName["SQLCCSID"] = SQLCCSID; optionWordToOptionName["SQLC"] = SQLCCSID; optionWordToOptionName["NOSQLCCSID"] = SQLCCSID; optionWordToOptionName["NOSQLC"] = SQLCCSID;
            optionWordToOptionName["SQLIMS"] = SQLIMS; optionWordToOptionName["NOSQLIMS"] = SQLIMS;
            optionWordToOptionName["SSRANGE"] = SSRANGE; optionWordToOptionName["SSR"] = SSRANGE; optionWordToOptionName["NOSSRANGE"] = SSRANGE; optionWordToOptionName["NOSSR"] = SSRANGE;
            optionWordToOptionName["STGOPT"] = STGOPT; optionWordToOptionName["SO"] = STGOPT; optionWordToOptionName["NOSTGOPT"] = STGOPT; optionWordToOptionName["NOSO"] = STGOPT;
            optionWordToOptionName["SUPPRESS"] = SUPPRESS; optionWordToOptionName["SUPP"] = SUPPRESS; optionWordToOptionName["NOSUPPRESS"] = SUPPRESS; optionWordToOptionName["NOSUPP"] = SUPPRESS;
            optionWordToOptionName["TERMINAL"] = TERMINAL; optionWordToOptionName["TERM"] = TERMINAL; optionWordToOptionName["NOTERMINAL"] = TERMINAL; optionWordToOptionName["NOTERM"] = TERMINAL;
            optionWordToOptionName["TEST"] = TEST; optionWordToOptionName["NOTEST"] = TEST;
            optionWordToOptionName["THREAD"] = THREAD; optionWordToOptionName["NOTHREAD"] = THREAD;
            optionWordToOptionName["TRUNC"] = TRUNC;
            optionWordToOptionName["VBREF"] = VBREF; optionWordToOptionName["NOVBREF"] = VBREF;
            optionWordToOptionName["WORD"] = WORD; optionWordToOptionName["WD"] = WORD; optionWordToOptionName["NOWORD"] = WORD; optionWordToOptionName["NOWD"] = WORD;
            optionWordToOptionName["XREF"] = XREF; optionWordToOptionName["X"] = XREF; optionWordToOptionName["NOXREF"] = XREF; optionWordToOptionName["NOX"] = XREF;
            optionWordToOptionName["ZONECHECK"] = ZONECHECK; optionWordToOptionName["ZC"] = ZONECHECK; optionWordToOptionName["NOZONECHECK"] = ZONECHECK; optionWordToOptionName["NOZC"] = ZONECHECK;
            optionWordToOptionName["ZONEDATA"] = ZONEDATA; optionWordToOptionName["ZD"] = ZONEDATA;
            optionWordToOptionName["ZWB"] = ZWB; optionWordToOptionName["NOZWB"] = ZWB; 

            deprecatedOptions["LIB"] = "the \"LIB\" option specification is no longer required. COBOL library processing is always in effect.";

            notAllowedOptions["LP"] = "the \"LP\" option is not allowed because only AMODE 31 (31-bit) is supported.";
        }

        /// <summary>
        /// If optionWord is a supported option name (EXIT)    or abbreviation (EX) or negation (NOEXIT / NOEX)
        /// this method sets its status (IsActivated) and value (from parameters).
        /// <br>Deprecated options are simply ignored (but are considered as valid).</br>
        /// </summary>
        public bool TrySetIBMOptionStatusAndValue(string optionWord, string? optionParameters)
        {
            if (deprecatedOptions.ContainsKey(optionWord) || notAllowedOptions.ContainsKey(optionWord))
            {
                return true;
            }

            string optionWordUpper = optionWord.ToUpper();
            if (optionWordToOptionName.TryGetValue(optionWordUpper, out var optionStatus))
            {
                return optionStatus.SetStatusAndValue(optionWordUpper, optionParameters);
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// If optionWord is a deprecated option this method returns the corresponding warning message to be displayed
        /// </summary>
        public bool IsOptionDeprecated(string optionWord, [MaybeNullWhen(false)] out string warningMessage)
        {
            return deprecatedOptions.TryGetValue(optionWord, out warningMessage);
        }

        /// <summary>
        /// If optionWord is not an allowed option this method returns the corresponding error message to be displayed
        /// </summary>
        public bool IsOptionNotAllowed(string optionWord, [MaybeNullWhen(false)] out string errorMessage)
        {
            return notAllowedOptions.TryGetValue(optionWord, out errorMessage);
        }

        /// <summary>
        /// Get the option name from an option word (which can be an abbreviation or a negation)
        /// </summary>
        /// <param name="optionWord">the option word</param>
        /// <param name="optionName">the option name (if any) corresponding to the option word, otherwise the enum's default value</param>
        /// <returns>true if there is an option name corresponding to the option word, false otherwise</returns>
        public bool TryGetOptionName(string optionWord, out IBMCompilerOptionName optionName)
        {
            if (optionWordToOptionName.TryGetValue(optionWord.ToUpper(), out IBMCompilerOptionStatus? optionStatus))
            {
                optionName = optionStatus.Name;
                return true;
            }

            optionName = default;
            return false;
        }
    }

    /// <summary>
    /// Enumeration of all the IBM compiler options
    /// </summary>
    public enum IBMCompilerOptionName
    {
        /* Use ADATA when you want the compiler to create a SYSADATA file that contains records of additional compilation information */
        ADATA,
        /* ADV has meaning only if you use WRITE...ADVANCING in your source code. With ADV in effect, the compiler adds 1 byte to the record length to account for the printer control character. */
        ADV,
        /* The AFP option controls the compiler usage of the Additional Floating Point (AFP) registers that are provided by z/Architecture processors. */
        AFP,
        /* The ARCH option specifies the machine architecture for which the executable program instructions are to be generated. */
        ARCH,
        /* ARITH affects the maximum number of digits that you can code for integers, and the number of digits used in fixed-point intermediate results. */
        ARITH,
        /* If you specify AWO, an implicit APPLY WRITE-ONLY clause is activated for all QSAM files in the program that have blocked variable-length records. */
        AWO,
        /* Use BLOCK0 to change the compiler default for QSAM files from unblocked to blocked (as if BLOCK CONTAINS 0 were specified) and thus gain the benefit of system-determined blocking for output files. */
        BLOCK0,
        /* Use BUFSIZE to allocate an amount of main storage to the buffer for each compiler work data set. Usually, a large buffer size improves the performance of the compiler. */
        BUFSIZE,
        /* The CICS compiler option enables the integrated CICS translator and lets you specify CICS suboptions. You must use the CICS option if your COBOL source program contains EXEC CICS or EXEC DLI statements and the program has not been processed by the separate CICS translator. */
        CICS,
        /* Use CODEPAGE to specify the coded character set identifier (CCSID) for an EBCDIC code page for processing compile-time and runtime COBOL operations that are sensitive to character encoding. */
        CODEPAGE,
        /* Use the COMPILE option only if you want to force full compilation even in the presence of serious errors. All diagnostics and object code will be generated. Do not try to run the object code if the compilation resulted in serious errors: the results could be unpredictable or an abnormal termination could occur. */
        COMPILE,
        /* Use the COPYLOC compiler option to add either a PDSE (or PDS) dataset or z/OS UNIX directory as an additional location to be searched for copy members during the library phase.*/
        COPYLOC,
        /* You can use the CURRENCY option to provide an alternate default currency symbol to be used for a COBOL program. (The default currency symbol is the dollar sign ($).) */
        CURRENCY,
        /* The DATA option affects whether storage for dynamic data areas and other dynamic runtime storage is obtained from above or below the 16 MB line. */
        DATA,
        /* Using DBCS causes the compiler to recognize X’0E’ (SO) and X’0F’ (SI) as shift codes for the double-byte portion of an alphanumeric literal. */
        DBCS,
        /* Use DECK to produce object code in the form of 80-column records. If you use the DECK option, be certain that SYSPUNCH is defined in your JCL for compilation. */
        DECK,
        /* Use the DEFINE compiler option to assign a literal value to a compilation variable that is defined in the program by using the DEFINE directive with the PARAMETER phrase. */
        DEFINE,
        /* DIAGTRUNC causes the compiler to issue a severity-4 (Warning) diagnostic message for MOVE statements that have numeric receivers when the receiving data item has fewer integer positions than the sending data item or literal. In statements that have multiple receivers, the message is issued separately for each receiver that could be truncated. */
        DIAGTRUNC,
        /* The DISPSIGN option controls output formatting for DISPLAY of signed numeric items. */
        DISPSIGN,
        /* Use DLL to instruct the compiler to generate an object module that is enabled for dynamic link library (DLL) support. DLL enablement is required if the program will be part of a DLL, will reference DLLs, or if the program contains object-oriented COBOL syntax such as INVOKE statements or class definitions. */
        DLL,
        /* Use DUMP to produce a system dump at compile time for an internal compiler error. */
        DUMP,
        /* Use DYNAM to cause nonnested, separately compiled programs invoked through the CALL literal statement to be loaded for CALL, and deleted for CANCEL, dynamically at run time. */
        DYNAM,
        /* Use the EXIT option to provide user-supplied modules in place of various compiler functions. */
        EXIT,
        /* Use EXPORTALL to instruct the compiler to automatically export the PROGRAM-ID name and each alternate entry-point name from each program definition when the object deck is link-edited to form a DLL. */
        EXPORTALL,
        /* Use FASTSRT to let IBM DFSORT, or an equivalent product, perform sort input and output instead of Enterprise COBOL. */
        FASTSRT,
        /* Use FLAG(x) to produce diagnostic messages at the end of the source listing for errors of a severity level x or above. */
        FLAG,
        /* Use FLAGSTD to specify the level or subset of Standard COBOL 85 to be regarded as conforming, and to get informational messages about Standard COBOL 85 elements that are included in your program. */
        FLAGSTD,
        /* The HGPR option controls the compiler usage of the 64-bit registers provided by z/Architecture processors. */
        HGPR,
        /* Use the INITCHECK option to have the compiler check for uninitialized data items and issue warning messages when they are used without being initialized. */
        INITCHECK,
        /* The INITIAL compiler option causes a program and all of its nested programs to behave as if the IS INITIAL clause was specified on the PROGRAM-ID paragraph. */
        INITIAL,
        /* The INVDATA option tells the compiler whether the data in USAGE DISPLAY and PACKED-DECIMAL data items is valid, and if not, what the behavior of the compiler should be. */
        INVDATA,
        /* INTDATE(ANSI) instructs the compiler to use the Standard COBOL 85 starting date for integer dates used with date intrinsic functions. Day 1 is Jan 1, 1601. INTDATE(LILIAN) instructs the compiler to use the Language Environment Lilian starting date for integer dates used with date intrinsic functions. Day 1 is Oct 15, 1582. */
        INTDATE,
        /* Use the LANGUAGE option to select the language in which compiler output will be printed. The information that will be printed in the selected language includes diagnostic messages, source listing page and scale headers, FIPS message headers, message summary headers, compilation summary, and headers and notations that result from the selection of certain compiler options (MAP, XREF, VBREF, and FLAGSTD). */
        LANGUAGE,
        /* The LIB option specification is no longer required. COBOL library processing is always in effect. We keep it only for compatibility with old versions. */
        LIB,
        /* Use LINECOUNT(nnn) to specify the number of lines to be printed on each page of the compilation listing, or use LINECOUNT(0) to suppress pagination. */
        LINECOUNT,
        /* Use the LIST compiler option to produce a listing of the assembler-language expansion of your source code. */
        LIST,
        /* Use the LP compiler option to indicate whether a AMODE 31 (31-bit) or AMODE 64 (64-bit) program should be generated with the related language features enabled. */
        LP,
        /* Use MAP to produce a listing of the items defined in the DATA DIVISION. */
        MAP,
        /* Use the MAXPCF option to specify a maximum program complexity factor value. The program complexity factor (PCF) is computed by the compiler and the computed value is in the listing file. If the PCF of your program exceeds the maximum value, the compiler will automatically reduce the optimization level to speed up the compilation and use less storage. Therefore, when you compile a suite of programs, you do not have to specify an OPTIMIZE option value for each program. */
        MAXPCF,
        /* The MDECK compiler option specifies that a copy of the updated input source after library processing (that is, the result of COPY, BASIS, REPLACE, EXEC SQL INCLUDE, and EXEC SQLIMS INCLUDE statements) is written to a file. */
        MDECK,
        /* Use NAME to generate a link-edit NAME card for each object module. You can also use NAME to generate names for each load module when you are doing batch compilations. */
        NAME,
        /* The NSYMBOL option controls the interpretation of the N symbol used in literals and PICTURE clauses, indicating whether national or DBCS processing is assumed. */
        NSYMBOL,
        /* Use the NUMBER compiler option if you have line numbers in your source code and want those numbers to be used in error messages and SOURCE, MAP, LIST, and XREF listings. */
        NUMBER,
        /* The NUMCHECK compiler option tells the compiler whether to generate extra code to validate data items when they are used as sending data items. */
        NUMCHECK,
        /* Use NUMPROC(NOPFD) if your internal decimal and zoned decimal data might use nonpreferred signs. */
        NUMPROC,
        /* Use OBJECT to write the generated object code to a file to be used as input for the binder. */
        OBJECT,
        /* Use OFFSET to produce a condensed PROCEDURE DIVISION listing. */
        OFFSET,
        /* Use OPTFILE to enable the specifying of COBOL compiler options in a data set. Using a compiler-option data set circumvents the 100-character limit on options specified in a JCL PARM string. */
        OPTFILE,
        /* Use OPTIMIZE to reduce the run time of your object program. Optimization might also reduce the amount of storage your object program uses. */
        OPTIMIZE,
        /* Use OUTDD to specify that you want DISPLAY output that is directed to the system logical output device to go to a specific ddname. */
        OUTDD,
        /* The PARMCHECK option tells the compiler to generate an extra data item following the last item in WORKING-STORAGE. */
        PARMCHECK,
        /* The PGMNAME option controls the handling of program-names and entry-point names. */
        PGMNAME,
        /* Use QUOTE if you want the figurative constant [ALL] QUOTE or [ALL] QUOTES to represent one or more quotation mark (") characters. Use APOST if you want the figurative constant [ALL] QUOTE or [ALL] QUOTES to represent one or more single quotation mark (') characters. */
        QUOTE,
        /* A program compiled as RENT is generated as a reentrant object program. A program compiled as NORENT is generated as a nonreentrant object program. */
        RENT,
        /* The RMODE setting influences the RMODE (residency mode) of your generated object program. */
        RMODE,
        /* When you use SEQUENCE, the compiler examines columns 1 through 6 to check that the source statements are arranged in ascending order according to their EBCDIC collating sequence. The compiler issues a diagnostic message if any statements are not in ascending order. */
        SEQUENCE,
        /* Use SIZE to indicate the amount of main storage to be made available to the compiler front end for compilation. The compiler front end is the phase of compilation that occurs before code generation and optimization. */
        SIZE,
        /* Use SOURCE to get a listing of your source program. This listing will include any statements embedded by PROCESS or COPY statements. */
        SOURCE,
        /* Use SPACE to select single-, double-, or triple-spacing in your source code listing. */
        SPACE,
        /* Use the SQL compiler option to enable the DB2 coprocessor and to specify DB2 suboptions. You must specify the SQL option if a COBOL source program contains SQL statements (EXEC SQL statements) and the program has not been processed by the DB2 precompiler. */
        SQL,
        /* Use the SQLCCSID compiler option to control whether the CODEPAGE compiler option will influence the processing of SQL statements in your COBOL programs. */
        SQLCCSID,
        /* Use the SQLIMS compiler option to enable the IMS SQL coprocessor and to specify Information Management System (IMS) suboptions. You must specify the SQLIMS option if a COBOL source program contains SQLIMS statements (EXEC SQLIMS statements). */
        SQLIMS,
        /* Use SSRANGE to generate code that checks for out-of-range storage references. */
        SSRANGE,
        /* The STGOPT option controls storage optimization. */
        STGOPT,
        /* Use the NOSUPPRESS option to ignore the SUPPRESS phrase of all COPY statements in a program so that copybook information can appear in the listing. */
        SUPPRESS,
        /* Use TERMINAL to send progress and diagnostic messages to the SYSTERM ddname. */
        TERMINAL,
        /* Use TEST to produce object code that enables debugging and problem determination tools such as Debug Tool and Fault Analyzer. With TEST, you can also enable the inclusion of symbolic variables in the formatted dumps that are produced by Language Environment. */
        TEST,
        /* THREAD indicates that a COBOL program is to be enabled for execution in a Language Environment enclave that has multiple POSIX threads or PL/I tasks. */
        THREAD,
        /* TRUNC affects the way that binary data is truncated during moves and arithmetic operations. */
        TRUNC,
        /* Use VBREF to get a cross-reference between all verbs used in the source program and the line numbers in which they are used. VBREF also produces a summary of the number of times each verb was used in the program. */
        VBREF,
        /* Use WORD(xxxx) to specify that an alternate reserved-word table is to be used during compilation. */
        WORD,
        /* Use XREF to produce a sorted cross-reference listing. */
        XREF,
        /* Use the ZONECHECK option to have the compiler generate IF NUMERIC class tests for zoned decimal data items that are used as sending data items. */
        ZONECHECK,
        /* The ZONEDATA option tells the compiler whether the data in USAGE DISPLAY and PACKED-DECIMAL data items is valid, and if not, what the behavior of the compiler should be. */
        ZONEDATA,
        /* If you compile using ZWB, the compiler removes the sign from a signed zoned decimal (DISPLAY) field before comparing this field to an alphanumeric elementary field during execution. */
        ZWB
    }
}
